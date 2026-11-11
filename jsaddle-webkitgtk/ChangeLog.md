# Revision history for jsaddle-webkitgtk

## 0.10.0.0

* Ported to GTK 4 and WebKitGTK 6.0 (gi-gtk4, gi-webkit 6.x,
  gi-javascriptcore6), replacing GTK 3 / WebKitGTK 3.  `run` keeps its
  type; `runInWebView` now takes a WebKitGTK 6.0 `WebView`.  Native builds
  are Linux only (the JavaScript backend stub builds anywhere).
* jsaddle-webkit2gtk (GTK 3 / webkit2gtk 4.0) remains for GTK 3 apps.
