/* jsaddle-webview2: host a jsaddle app in a Microsoft Edge WebView2 control.
 *
 * Plain C against the official WebView2.h (COBJMACROS / C-style COM).  The
 * WebView2Loader.dll is loaded dynamically (LoadLibrary) so nothing from the
 * WebView2 SDK is needed at link time -- only WebView2.h at compile time.
 *
 * Threading: WebView2 is single-threaded (STA).  Everything that touches
 * ICoreWebView2* must run on the thread that created the controller -- the
 * thread that called runJsaddleWebView2 and is pumping the message loop.
 * Calls arriving from other (Haskell) threads are marshalled to it with
 * PostMessage (the payload is a heap-allocated wide string freed by the
 * window proc).
 *
 * The synchronous jsaddle bridge mirrors jsaddle-wkwebview: the page calls
 * window.prompt("JSaddleSync", <Results JSON>); with default script dialogs
 * disabled this raises ScriptDialogOpening on the UI thread while the page's
 * JS is blocked, we call into Haskell (which computes the reply Batch and
 * hands it back via completeSyncWV2 before returning), then put_ResultText +
 * Accept complete the prompt with the Batch JSON.
 */
#define COBJMACROS
#include <windows.h>
#include <objbase.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "HsFFI.h"
#include "WebView2.h"

#define WM_JSADDLE_EVAL     (WM_APP + 1)
#define WM_JSADDLE_NAV_URL  (WM_APP + 2)
#define WM_JSADDLE_NAV_HTML (WM_APP + 3)

typedef struct JSaddleWV2 {
    HWND hwnd;
    ICoreWebView2Controller *controller;
    ICoreWebView2 *webview;
    HsStablePtr mainCallback;
    HsStablePtr startHandler;
    HsStablePtr resultHandler;
    HsStablePtr syncHandler;
    int handlersSet;
    int started;
    int devTools;
    char *syncResult; /* set by completeSyncWV2 inside jsaddleSyncResult */
} JSaddleWV2;

/* Haskell (foreign export ccall) */
extern void jsaddleStart(HsStablePtr);
extern void jsaddleResult(HsStablePtr, const char *);
extern void jsaddleSyncResult(HsStablePtr, JSaddleWV2 *, const char *);
extern void callWithWebView2(JSaddleWV2 *, HsStablePtr);

/* ---------------------------------------------------------------- utf8 */

static char *utf8FromWide(LPCWSTR w)
{
    if (!w) return NULL;
    int n = WideCharToMultiByte(CP_UTF8, 0, w, -1, NULL, 0, NULL, NULL);
    if (n <= 0) return NULL;
    char *s = (char *)malloc((size_t)n);
    if (s) WideCharToMultiByte(CP_UTF8, 0, w, -1, s, n, NULL, NULL);
    return s;
}

static LPWSTR wideFromUtf8(const char *s)
{
    if (!s) return NULL;
    int n = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0);
    if (n <= 0) return NULL;
    LPWSTR w = (LPWSTR)malloc((size_t)n * sizeof(WCHAR));
    if (w) MultiByteToWideChar(CP_UTF8, 0, s, -1, w, n);
    return w;
}

/* ------------------------------------------------- generic COM handler
 * Every callback interface WebView2 hands us is IUnknown + one Invoke, so a
 * single struct layout covers them all; only the Invoke slot differs.
 * QueryInterface accepts any IID: WebView2 only ever asks for the interface
 * it was given (or IUnknown), and we have no IID table to compare against
 * without dragging in a MIDL-generated _i.c.
 */
typedef struct Handler {
    const void *lpVtbl;
    volatile LONG rc;
    JSaddleWV2 *app;
} Handler;

static HRESULT STDMETHODCALLTYPE h_QueryInterface(void *self, REFIID riid, void **ppv)
{
    (void)riid;
    if (!ppv) return E_POINTER;
    *ppv = self;
    InterlockedIncrement(&((Handler *)self)->rc);
    return S_OK;
}

static ULONG STDMETHODCALLTYPE h_AddRef(void *self)
{
    return (ULONG)InterlockedIncrement(&((Handler *)self)->rc);
}

static ULONG STDMETHODCALLTYPE h_Release(void *self)
{
    LONG rc = InterlockedDecrement(&((Handler *)self)->rc);
    if (rc == 0) free(self);
    return (ULONG)rc;
}

#define IMPL_UNKNOWN(TYPE) \
    (HRESULT (STDMETHODCALLTYPE *)(TYPE *, REFIID, void **))h_QueryInterface, \
    (ULONG (STDMETHODCALLTYPE *)(TYPE *))h_AddRef, \
    (ULONG (STDMETHODCALLTYPE *)(TYPE *))h_Release

static Handler *newHandler(const void *vtbl, JSaddleWV2 *app)
{
    Handler *h = (Handler *)calloc(1, sizeof *h);
    if (!h) return NULL;
    h->lpVtbl = vtbl;
    h->rc = 1;
    h->app = app;
    return h;
}

/* -------------------------------------------- no-op completed handler
 * Shared by AddScriptToExecuteOnDocumentCreated and ExecuteScript -- both
 * Invoke signatures are (HRESULT, LPCWSTR).
 */
static HRESULT STDMETHODCALLTYPE noop_Invoke(
    ICoreWebView2ExecuteScriptCompletedHandler *self, HRESULT hr, LPCWSTR result)
{
    (void)self; (void)hr; (void)result;
    return S_OK;
}

static const ICoreWebView2ExecuteScriptCompletedHandlerVtbl noopVtbl = {
    IMPL_UNKNOWN(ICoreWebView2ExecuteScriptCompletedHandler),
    noop_Invoke
};

/* --------------------------------------------------- event callbacks */

static HRESULT STDMETHODCALLTYPE navCompleted_Invoke(
    ICoreWebView2NavigationCompletedEventHandler *self,
    ICoreWebView2 *sender, ICoreWebView2NavigationCompletedEventArgs *args)
{
    (void)sender; (void)args;
    JSaddleWV2 *app = ((Handler *)self)->app;
    /* jsaddle state does not survive a reload, so only signal the first
       completed navigation (a second putMVar would block the UI thread). */
    if (app->handlersSet && !app->started) {
        app->started = 1;
        jsaddleStart(app->startHandler);
    }
    return S_OK;
}

static const ICoreWebView2NavigationCompletedEventHandlerVtbl navCompletedVtbl = {
    IMPL_UNKNOWN(ICoreWebView2NavigationCompletedEventHandler),
    navCompleted_Invoke
};

static HRESULT STDMETHODCALLTYPE webMessage_Invoke(
    ICoreWebView2WebMessageReceivedEventHandler *self,
    ICoreWebView2 *sender, ICoreWebView2WebMessageReceivedEventArgs *args)
{
    (void)sender;
    JSaddleWV2 *app = ((Handler *)self)->app;
    LPWSTR msg = NULL;
    if (app->handlersSet
        && SUCCEEDED(ICoreWebView2WebMessageReceivedEventArgs_TryGetWebMessageAsString(args, &msg))
        && msg) {
        char *u = utf8FromWide(msg);
        if (u) {
            jsaddleResult(app->resultHandler, u);
            free(u);
        }
    }
    if (msg) CoTaskMemFree(msg);
    return S_OK;
}

static const ICoreWebView2WebMessageReceivedEventHandlerVtbl webMessageVtbl = {
    IMPL_UNKNOWN(ICoreWebView2WebMessageReceivedEventHandler),
    webMessage_Invoke
};

static HRESULT STDMETHODCALLTYPE scriptDialog_Invoke(
    ICoreWebView2ScriptDialogOpeningEventHandler *self,
    ICoreWebView2 *sender, ICoreWebView2ScriptDialogOpeningEventArgs *args)
{
    (void)sender;
    JSaddleWV2 *app = ((Handler *)self)->app;
    COREWEBVIEW2_SCRIPT_DIALOG_KIND kind;
    if (FAILED(ICoreWebView2ScriptDialogOpeningEventArgs_get_Kind(args, &kind))
        || kind != COREWEBVIEW2_SCRIPT_DIALOG_KIND_PROMPT)
        return S_OK;

    LPWSTR message = NULL;
    ICoreWebView2ScriptDialogOpeningEventArgs_get_Message(args, &message);
    int isSync = message && wcscmp(message, L"JSaddleSync") == 0;
    if (message) CoTaskMemFree(message);
    if (!isSync || !app->handlersSet)
        return S_OK; /* not ours: leave unaccepted, prompt() returns null */

    LPWSTR defText = NULL;
    ICoreWebView2ScriptDialogOpeningEventArgs_get_DefaultText(args, &defText);
    char *u = utf8FromWide(defText);
    if (defText) CoTaskMemFree(defText);
    if (!u) return S_OK;

    free(app->syncResult);
    app->syncResult = NULL;
    /* Synchronous round trip into Haskell: computes the reply Batch and
       stores it via completeSyncWV2 before returning. */
    jsaddleSyncResult(app->syncHandler, app, u);
    free(u);

    if (app->syncResult) {
        LPWSTR w = wideFromUtf8(app->syncResult);
        if (w) {
            ICoreWebView2ScriptDialogOpeningEventArgs_put_ResultText(args, w);
            ICoreWebView2ScriptDialogOpeningEventArgs_Accept(args);
            free(w);
        }
        free(app->syncResult);
        app->syncResult = NULL;
    }
    return S_OK;
}

static const ICoreWebView2ScriptDialogOpeningEventHandlerVtbl scriptDialogVtbl = {
    IMPL_UNKNOWN(ICoreWebView2ScriptDialogOpeningEventHandler),
    scriptDialog_Invoke
};

/* -------------------------------------------- controller / environment */

static HRESULT STDMETHODCALLTYPE controllerCompleted_Invoke(
    ICoreWebView2CreateCoreWebView2ControllerCompletedHandler *self,
    HRESULT hr, ICoreWebView2Controller *controller)
{
    JSaddleWV2 *app = ((Handler *)self)->app;
    if (FAILED(hr) || !controller) {
        fprintf(stderr, "jsaddle-webview2: controller creation failed (0x%08lx)\n",
                (unsigned long)hr);
        PostQuitMessage(1);
        return hr;
    }

    app->controller = controller;
    ICoreWebView2Controller_AddRef(controller);
    ICoreWebView2Controller_get_CoreWebView2(controller, &app->webview);

    ICoreWebView2Settings *settings = NULL;
    ICoreWebView2_get_Settings(app->webview, &settings);
    if (settings) {
        ICoreWebView2Settings_put_IsScriptEnabled(settings, TRUE);
        ICoreWebView2Settings_put_IsWebMessageEnabled(settings, TRUE);
        /* Required for the JSaddleSync prompt() intercept. */
        ICoreWebView2Settings_put_AreDefaultScriptDialogsEnabled(settings, FALSE);
        ICoreWebView2Settings_put_AreDevToolsEnabled(settings, app->devTools ? TRUE : FALSE);
        ICoreWebView2Settings_Release(settings);
    }

    EventRegistrationToken tok;
    Handler *h;

    h = newHandler(&navCompletedVtbl, app);
    ICoreWebView2_add_NavigationCompleted(app->webview,
        (ICoreWebView2NavigationCompletedEventHandler *)h, &tok);
    h_Release(h);

    h = newHandler(&webMessageVtbl, app);
    ICoreWebView2_add_WebMessageReceived(app->webview,
        (ICoreWebView2WebMessageReceivedEventHandler *)h, &tok);
    h_Release(h);

    h = newHandler(&scriptDialogVtbl, app);
    ICoreWebView2_add_ScriptDialogOpening(app->webview,
        (ICoreWebView2ScriptDialogOpeningEventHandler *)h, &tok);
    h_Release(h);

    RECT rc;
    GetClientRect(app->hwnd, &rc);
    ICoreWebView2Controller_put_Bounds(app->controller, rc);
    ICoreWebView2Controller_put_IsVisible(app->controller, TRUE);

    /* Hand control to Haskell: it wires the jsaddle handlers (wv2SetHandlers)
       and starts the initial navigation. */
    callWithWebView2(app, app->mainCallback);
    return S_OK;
}

static const ICoreWebView2CreateCoreWebView2ControllerCompletedHandlerVtbl controllerCompletedVtbl = {
    IMPL_UNKNOWN(ICoreWebView2CreateCoreWebView2ControllerCompletedHandler),
    controllerCompleted_Invoke
};

static HRESULT STDMETHODCALLTYPE envCompleted_Invoke(
    ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler *self,
    HRESULT hr, ICoreWebView2Environment *env)
{
    JSaddleWV2 *app = ((Handler *)self)->app;
    if (FAILED(hr) || !env) {
        fprintf(stderr,
            "jsaddle-webview2: environment creation failed (0x%08lx); is the WebView2 runtime installed?\n",
            (unsigned long)hr);
        PostQuitMessage(1);
        return hr;
    }
    Handler *cc = newHandler(&controllerCompletedVtbl, app);
    HRESULT r = ICoreWebView2Environment_CreateCoreWebView2Controller(env, app->hwnd,
        (ICoreWebView2CreateCoreWebView2ControllerCompletedHandler *)cc);
    h_Release(cc);
    if (FAILED(r)) {
        fprintf(stderr, "jsaddle-webview2: CreateCoreWebView2Controller failed (0x%08lx)\n",
                (unsigned long)r);
        PostQuitMessage(1);
    }
    return r;
}

static const ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandlerVtbl envCompletedVtbl = {
    IMPL_UNKNOWN(ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler),
    envCompleted_Invoke
};

/* ------------------------------------------------------- window proc */

static LRESULT CALLBACK wndProc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
    JSaddleWV2 *app = (JSaddleWV2 *)GetWindowLongPtrW(hwnd, GWLP_USERDATA);
    switch (msg) {
    case WM_SIZE:
        if (app && app->controller) {
            RECT rc;
            GetClientRect(hwnd, &rc);
            ICoreWebView2Controller_put_Bounds(app->controller, rc);
        }
        return 0;
    case WM_MOVE:
        if (app && app->controller)
            ICoreWebView2Controller_NotifyParentWindowPositionChanged(app->controller);
        return 0;
    case WM_JSADDLE_EVAL:
        if (app && app->webview) {
            Handler *h = newHandler(&noopVtbl, app);
            ICoreWebView2_ExecuteScript(app->webview, (LPCWSTR)lp,
                (ICoreWebView2ExecuteScriptCompletedHandler *)h);
            h_Release(h);
        }
        free((void *)lp);
        return 0;
    case WM_JSADDLE_NAV_URL:
        if (app && app->webview)
            ICoreWebView2_Navigate(app->webview, (LPCWSTR)lp);
        free((void *)lp);
        return 0;
    case WM_JSADDLE_NAV_HTML:
        if (app && app->webview)
            ICoreWebView2_NavigateToString(app->webview, (LPCWSTR)lp);
        free((void *)lp);
        return 0;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcW(hwnd, msg, wp, lp);
}

/* ------------------------------------------- API called from Haskell */

void wv2SetHandlers(JSaddleWV2 *app, HsStablePtr start, HsStablePtr result, HsStablePtr sync)
{
    app->startHandler = start;
    app->resultHandler = result;
    app->syncHandler = sync;
    app->handlersSet = 1;
}

/* Thread-safe: marshalled to the UI thread via PostMessage. */
void wv2Eval(JSaddleWV2 *app, const char *js)
{
    LPWSTR w = wideFromUtf8(js);
    if (w && !PostMessageW(app->hwnd, WM_JSADDLE_EVAL, 0, (LPARAM)w))
        free(w);
}

void wv2Navigate(JSaddleWV2 *app, const char *url)
{
    LPWSTR w = wideFromUtf8(url);
    if (w && !PostMessageW(app->hwnd, WM_JSADDLE_NAV_URL, 0, (LPARAM)w))
        free(w);
}

void wv2NavigateToString(JSaddleWV2 *app, const char *html)
{
    LPWSTR w = wideFromUtf8(html);
    if (w && !PostMessageW(app->hwnd, WM_JSADDLE_NAV_HTML, 0, (LPARAM)w))
        free(w);
}

/* Called by Haskell inside jsaddleSyncResult (same thread, same dynamic
   extent) with the reply Batch JSON for the pending JSaddleSync prompt. */
void completeSyncWV2(JSaddleWV2 *app, const char *s)
{
    free(app->syncResult);
    app->syncResult = s ? strdup(s) : NULL;
}

typedef HRESULT (STDMETHODCALLTYPE *CreateEnvFn)(
    PCWSTR, PCWSTR, ICoreWebView2EnvironmentOptions *,
    ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler *);

int runJsaddleWebView2(HsStablePtr mainCallback, const char *title,
                       int width, int height, int devTools)
{
    /* Per-monitor v2 DPI awareness, where available (Win10 1703+). */
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    if (user32) {
        typedef BOOL (WINAPI *SetDpiCtxFn)(HANDLE);
        SetDpiCtxFn setCtx = (SetDpiCtxFn)(void *)GetProcAddress(user32, "SetProcessDpiAwarenessContext");
        if (setCtx) setCtx((HANDLE)-4 /* DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 */);
    }

    HMODULE loader = LoadLibraryW(L"WebView2Loader.dll");
    if (!loader) {
        fprintf(stderr, "jsaddle-webview2: WebView2Loader.dll not found; place it next to the executable\n");
        return 1;
    }
    CreateEnvFn createEnv = (CreateEnvFn)(void *)GetProcAddress(loader, "CreateCoreWebView2EnvironmentWithOptions");
    if (!createEnv) {
        fprintf(stderr, "jsaddle-webview2: CreateCoreWebView2EnvironmentWithOptions not found in WebView2Loader.dll\n");
        return 1;
    }

    HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
    if (FAILED(hr)) {
        fprintf(stderr, "jsaddle-webview2: CoInitializeEx failed (0x%08lx)\n", (unsigned long)hr);
        return 1;
    }

    JSaddleWV2 *app = (JSaddleWV2 *)calloc(1, sizeof *app);
    app->mainCallback = mainCallback;
    app->devTools = devTools;

    WNDCLASSW wc;
    memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = wndProc;
    wc.hInstance = GetModuleHandleW(NULL);
    wc.lpszClassName = L"JSaddleWebView2";
    wc.hCursor = LoadCursorW(NULL, MAKEINTRESOURCEW(32512) /* IDC_ARROW */);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    RegisterClassW(&wc);

    LPWSTR wtitle = wideFromUtf8(title);
    app->hwnd = CreateWindowExW(0, wc.lpszClassName, wtitle ? wtitle : L"JSaddle",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
        width > 0 ? width : CW_USEDEFAULT, height > 0 ? height : CW_USEDEFAULT,
        NULL, NULL, wc.hInstance, NULL);
    free(wtitle);
    if (!app->hwnd) {
        fprintf(stderr, "jsaddle-webview2: CreateWindow failed\n");
        return 1;
    }
    SetWindowLongPtrW(app->hwnd, GWLP_USERDATA, (LONG_PTR)app);
    ShowWindow(app->hwnd, SW_SHOWDEFAULT);
    UpdateWindow(app->hwnd);

    /* Default user-data folder is next to the exe, which fails under
       Program Files; use %LOCALAPPDATA%\jsaddle-webview2 instead. */
    WCHAR udf[MAX_PATH];
    PCWSTR udfArg = NULL;
    DWORD n = GetEnvironmentVariableW(L"LOCALAPPDATA", udf, MAX_PATH);
    if (n > 0 && n + 20 < MAX_PATH) {
        wcscat(udf, L"\\jsaddle-webview2");
        CreateDirectoryW(udf, NULL);
        udfArg = udf;
    }

    Handler *envH = newHandler(&envCompletedVtbl, app);
    hr = createEnv(NULL, udfArg, NULL,
        (ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler *)envH);
    h_Release(envH);
    if (FAILED(hr)) {
        fprintf(stderr,
            "jsaddle-webview2: CreateCoreWebView2EnvironmentWithOptions failed (0x%08lx); is the WebView2 runtime installed?\n",
            (unsigned long)hr);
        return 1;
    }

    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    return (int)msg.wParam;
}
