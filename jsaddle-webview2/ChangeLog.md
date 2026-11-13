# Revision history for jsaddle-webview2

## 0.1.0.0

* First release.  Hosts a jsaddle app in a native Win32 window containing
  a Microsoft Edge WebView2 control: the Windows sibling of
  jsaddle-wkwebview (macOS/iOS) and jsaddle-webkitgtk (Linux).
* The host window's HWND is exposed, for native menus and dialogs.
* Building needs WebView2.h from the Microsoft.Web.WebView2 NuGet package
  (see README.md); running needs the WebView2 runtime and
  WebView2Loader.dll next to the executable.
