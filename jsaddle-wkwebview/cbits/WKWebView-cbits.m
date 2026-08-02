#include "HsFFI.h"
#import <Foundation/Foundation.h>
#import <WebKit/WebKit.h>

BOOL openApp(NSURL * url);

@interface JSaddleHandler : NSObject<WKNavigationDelegate, WKScriptMessageHandler, WKUIDelegate>

@property (nonatomic, assign) HsStablePtr startHandler;
@property (nonatomic, assign) HsStablePtr resultHandler;
@property (nonatomic, assign) HsStablePtr syncHandler;
@property (nonatomic, assign) void (^completionHandler)(NSString *result);

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler syncHandler:(HsStablePtr)syncHandler;

@end

// The Haskell callbacks come through the registered jsaddleCallbacks table —
// see WKWebView-callbacks.h for why they are not extern symbol references.
#include "WKWebView-callbacks.h"

jsaddle_wk_callbacks jsaddleCallbacks;   // zero-initialised

void jsaddle_wk_set_callbacks(
    void (*jsaddleStart_)(HsStablePtr),
    void (*jsaddleResult_)(HsStablePtr, const char * _Nonnull),
    void (*jsaddleSyncResult_)(HsStablePtr, void * _Nonnull, const char * _Nonnull),
    void (*callIO_)(HsStablePtr),
    void (*callWithCString_)(const char * _Nonnull, HsStablePtr),
    bool (*callWithCStringReturningBool_)(const char * _Nonnull, HsStablePtr),
    void (*callWithWebView_)(void * _Nonnull, HsStablePtr),
    void (*callWithCIntCString_)(int, const char * _Nonnull, HsStablePtr))
{
    jsaddleCallbacks.jsaddleStart = jsaddleStart_;
    jsaddleCallbacks.jsaddleResult = jsaddleResult_;
    jsaddleCallbacks.jsaddleSyncResult = jsaddleSyncResult_;
    jsaddleCallbacks.callIO = callIO_;
    jsaddleCallbacks.callWithCString = callWithCString_;
    jsaddleCallbacks.callWithCStringReturningBool = callWithCStringReturningBool_;
    jsaddleCallbacks.callWithWebView = callWithWebView_;
    jsaddleCallbacks.callWithCIntCString = callWithCIntCString_;
}

@implementation JSaddleHandler

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler syncHandler:(HsStablePtr)syncHandler
{
    self = [super init];
    if (self) {
        _startHandler = startHandler;
        _resultHandler = resultHandler;
        _syncHandler = syncHandler;
    }
    return self;
}

// The stable-pointer checks in the three handler methods below cover the
// window between removeJSaddleHandler freeing them (setting them to NULL) and
// the last WebKit delivery already in flight: dereferencing a freed
// StablePtr is undefined, ignoring a message for a detached context is not.
- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation
{
    if (jsaddleCallbacks.jsaddleStart && self.startHandler)
        jsaddleCallbacks.jsaddleStart(self.startHandler);
}

- (void)userContentController:(WKUserContentController *)userContentController didReceiveScriptMessage:(WKScriptMessage *)message
{
    NSString * s = (NSString *) message.body;
    if (jsaddleCallbacks.jsaddleResult && self.resultHandler)
        jsaddleCallbacks.jsaddleResult(self.resultHandler, [s UTF8String]);
}

- (void)webView:(WKWebView *)webView runJavaScriptTextInputPanelWithPrompt:(NSString *)prompt
    defaultText:(NSString *)defaultText initiatedByFrame:(WKFrameInfo *)frame completionHandler:(void (^)(NSString *result))completionHandler
{
    if([prompt isEqualToString:@"JSaddleSync"]) {
        if (jsaddleCallbacks.jsaddleSyncResult && self.syncHandler) {
            _completionHandler = completionHandler;
            jsaddleCallbacks.jsaddleSyncResult(self.syncHandler, (void *)self, [defaultText UTF8String]);
        }
        else
            completionHandler(@"");   // detached context: unblock the page
    }
    else {
        // TODO decide what if anything should go here
    }
}

- (void)webView:(WKWebView *)webView decidePolicyForNavigationAction:(WKNavigationAction *)navigationAction decisionHandler:(void (^)(WKNavigationActionPolicy))decisionHandler
{
    NSURL *url = navigationAction.request.URL;

    if(!navigationAction.targetFrame && ![url.scheme isEqualToString:@"file"]) {
        if(openApp(url))
            decisionHandler(WKNavigationActionPolicyCancel);
         else
            decisionHandler(WKNavigationActionPolicyAllow);
    }
    else
        decisionHandler(WKNavigationActionPolicyAllow);
}

@end

// Returns the handler (retained: the caller's reference, released by
// removeJSaddleHandler) so a host that tears a webview down can detach it
// again — see removeJSaddleHandler.
JSaddleHandler * addJSaddleHandler(WKWebView *webView, HsStablePtr startHandler, HsStablePtr resultHandler, HsStablePtr syncHandler) {
    JSaddleHandler * handler = [[JSaddleHandler alloc] initHandler:startHandler resultHandler:resultHandler syncHandler:syncHandler];
    [[[webView configuration] userContentController] addScriptMessageHandler:handler name:@"jsaddle"];
#ifdef USE_UIKIT
    webView.scrollView.bounces = NO;
#endif
    webView.navigationDelegate = handler;
    webView.UIDelegate = handler;
    return handler;
}

// Detach a handler installed by addJSaddleHandler and free the three stable
// pointers it holds.  Those pointers keep their closures — and through them the
// context's whole callback graph (for a reflex application, the entire live
// network) — alive for the life of the PROCESS, which in ghci mode means every
// reloaded instance's network stays reachable: measured at ~30MB of live heap
// leaked per reload.
//
// On the MAIN QUEUE, because that is where all three callbacks are delivered
// from: running the detach there is what makes it mutually exclusive with a
// callback that is mid-flight, so the frees cannot race a deRefStablePtr.
// Asynchronously, because the caller may be a Haskell thread that the main
// thread is (or is about to be) waiting on — a dispatch_sync from the jsaddle
// sync round trip's own producer would deadlock.  The block retains both
// objects, so the host may release the webview as soon as this returns.
// @onDetached@ is run (once) after the handler is detached; freeing the three
// stable pointers is its job, not this file's.  Calling the RTS directly here
// is not an option: in ghci mode this file is compiled into a plain dylib with
// no RTS to link against, so hs_free_stable_ptr is an undefined symbol — the
// same constraint that makes every Haskell call out of this file go through the
// registered callback table (see WKWebView-callbacks.h).
void removeJSaddleHandler(WKWebView *webView, JSaddleHandler *handler, HsStablePtr onDetached) {
    dispatch_async(dispatch_get_main_queue(), ^{
        [[[webView configuration] userContentController] removeScriptMessageHandlerForName:@"jsaddle"];
        if (webView.navigationDelegate == handler) webView.navigationDelegate = nil;
        if (webView.UIDelegate == handler) webView.UIDelegate = nil;
        // Clear before the pointers are freed, so a delivery WebKit had already
        // queued behind this block sees a detached handler (the methods above
        // check) rather than a dangling stable pointer.
        handler.startHandler = NULL;
        handler.resultHandler = NULL;
        handler.syncHandler = NULL;
        [handler release];   // balances addJSaddleHandler's +1
        if (jsaddleCallbacks.callIO)
            jsaddleCallbacks.callIO(onDetached);
    });
}

void evaluateJavaScript(WKWebView *webView, const char * _Nonnull js) {
    NSString *jsString = [[NSString alloc] initWithCString:js encoding:NSUTF8StringEncoding];
    dispatch_async(dispatch_get_main_queue(), ^{
        [webView evaluateJavaScript:jsString completionHandler:NULL];
        [jsString release];
    });
}

void completeSync(JSaddleHandler *handler, const char * _Nonnull s) {
    NSString *string = [[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding];
    handler.completionHandler(string);
    handler.completionHandler = NULL;
    [string release];
}

void loadHTMLStringWithBaseURL(WKWebView *webView, const char * _Nonnull html, const char * _Nonnull url) {
  NSString *htmlString = [NSString stringWithCString:html encoding:NSUTF8StringEncoding];
  NSURL *baseURL = [NSURL URLWithString:[NSString stringWithCString:url encoding:NSUTF8StringEncoding]];
  dispatch_async(dispatch_get_main_queue(), ^{
      [webView loadHTMLString:htmlString baseURL:baseURL];
    });
}

const char * mainBundleResourcePathC() {
    NSBundle *main = [NSBundle mainBundle];
    if(!main) return NULL;
    NSString *path = main.resourcePath;
    if(!path) return NULL;
    return [path UTF8String];
}

void loadBundleFile(WKWebView *webView, const char * _Nonnull file, const char * _Nonnull allowing) {
    NSBundle *main = [NSBundle mainBundle];
    if(main) {
        NSString *path = main.resourcePath;
        if(path) {
            NSURL *fileUrl = [NSURL fileURLWithPath:[NSString stringWithFormat: @"%@/%@", path, [NSString stringWithCString:file encoding:NSUTF8StringEncoding]]];
            NSURL *allowingUrl = [NSURL fileURLWithPath:[NSString stringWithFormat: @"%@/%@", path, [NSString stringWithCString:allowing encoding:NSUTF8StringEncoding]]];
            [webView loadFileURL:fileUrl allowingReadAccessToURL:allowingUrl];
        }
    }
}
