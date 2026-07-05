# jsaddle-webview2

Run [jsaddle](../jsaddle) apps in a native Windows window hosting a Microsoft
Edge **WebView2** control — the Windows sibling of
[jsaddle-wkwebview](../jsaddle-wkwebview) (macOS/iOS, WebKit) and
[jsaddle-webkit2gtk](../jsaddle-webkit2gtk) (Linux, WebKitGTK).

```haskell
import qualified Language.Javascript.JSaddle.WebView2 as WV2

main :: IO ()
main = WV2.run myJsmApp
```

## How it works

The C shim (`cbits/WebView2Shim.c`, plain C COM against the official
`WebView2.h`) creates a Win32 window, loads `WebView2Loader.dll` dynamically,
creates the WebView2 environment/controller, and pumps the message loop.
The jsaddle transport mirrors jsaddle-wkwebview exactly:

* **Haskell → JS**: batches are `ExecuteScript`ed (marshalled to the UI
  thread with `PostMessage`, since WebView2 is single-threaded).
* **JS → Haskell (async)**: `window.chrome.webview.postMessage(...)` →
  `WebMessageReceived`.
* **JS → Haskell (sync)**: `window.prompt("JSaddleSync", <json>)`.  Default
  script dialogs are disabled, so the `ScriptDialogOpening` event fires on
  the UI thread while the page's JS is blocked in `prompt()`; the handler
  calls into Haskell, which computes the reply batch, and completes the
  dialog with `put_ResultText` + `Accept`.  (Same trick as wkwebview's
  `runJavaScriptTextInputPanelWithPrompt` — which is why the WKWebView
  `uiDelegate` must never be overridden there, and why default script
  dialogs must stay disabled here.)

## Build requirements

Only `WebView2.h` (and the headers it includes) is needed at **compile**
time; nothing from the WebView2 SDK is linked.  Get it from the
[`Microsoft.Web.WebView2`](https://www.nuget.org/packages/Microsoft.Web.WebView2)
NuGet package (it's a plain zip): `build/native/include/WebView2.h`.
Point the include path at it, e.g. in `cabal.project`:

```
package jsaddle-webview2
  extra-include-dirs: C:\path\to\webview2-sdk\build\native\include
```

With nix, a small `fetchzip` derivation does it (license: unfree but
freely redistributable):

```nix
webview2-sdk = pkgs.fetchzip {
  url = "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4022.49";
  extension = "zip";
  stripRoot = false;
  hash = "...";
};
# then: extra-include-dirs = [ "${webview2-sdk}/build/native/include" ];
```

The header is MIDL-generated flat COM and compiles fine under **mingw-w64**
(GHC's Windows toolchain) — no MSVC needed.

## Runtime requirements

* The **WebView2 runtime** — preinstalled on Windows 11, Evergreen-installed
  on most Windows 10 machines ([bootstrapper](https://developer.microsoft.com/microsoft-edge/webview2/)
  for the rest).
* **`WebView2Loader.dll`** next to the executable — from the same NuGet
  package (`runtimes/win-x64/native/WebView2Loader.dll`).  It is
  `LoadLibrary`'d at startup, so no import library is needed at link time.

## Notes and limitations

* `runHTML` uses `NavigateToString`: page origin is `about:blank` and the
  HTML is capped at 2MB.  Use `runURL` (e.g. a `file://` URL or
  `SetVirtualHostNameToFolderMapping` in a future version) for real apps.
* Page reloads are not supported (jsaddle state does not survive them);
  only the first completed navigation starts the jsaddle app.
* The user-data folder defaults to `%LOCALAPPDATA%\jsaddle-webview2`.
* The sync bridge rides `window.prompt`, so apps must not intercept
  `ScriptDialogOpening` themselves, and very large sync payloads depend on
  Chromium passing dialog text through unclamped.
