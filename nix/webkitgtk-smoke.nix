# Headless runtime smoke test for jsaddle-webkitgtk: runs the demo's
# --smoke mode (async eval round trip + a JS→Haskell callback through the
# synchronous JSaddleSync script-dialog path) under Xvfb and asserts it
# prints SMOKE-OK.
#
# The environment exists to make WebKitGTK viable inside the nix build
# sandbox: no /etc/dbus-1 (point dbus at its store config), no bubblewrap
# (WebKit's own sandbox can't nest), and no /run/opengl-driver (point
# glvnd/mesa loaders at the store and software-render with llvmpipe).
{ lib, runCommand, xvfb-run, dbus, gsettings-desktop-schemas, gtk4, mesa
, jsaddle-webkitgtk-demo }:
let
  mesaDrivers = mesa.drivers or mesa;
in
runCommand "jsaddle-webkitgtk-smoke" {
  nativeBuildInputs = [ xvfb-run dbus jsaddle-webkitgtk-demo ];
} ''
  export HOME=$TMPDIR
  export XDG_RUNTIME_DIR=$TMPDIR
  export XDG_DATA_DIRS=${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk4}/share/gsettings-schemas/${gtk4.name}:''${XDG_DATA_DIRS:-}
  export WEBKIT_DISABLE_SANDBOX_THIS_IS_DANGEROUS=1
  export WEBKIT_DISABLE_DMABUF_RENDERER=1
  export WEBKIT_DISABLE_COMPOSITING_MODE=1
  export GDK_BACKEND=x11
  export GTK_A11Y=none
  export GSK_RENDERER=cairo
  export LIBGL_ALWAYS_SOFTWARE=1
  export GALLIUM_DRIVER=llvmpipe
  export __EGL_VENDOR_LIBRARY_DIRS=${mesaDrivers}/share/glvnd/egl_vendor.d
  export LIBGL_DRIVERS_PATH=${mesaDrivers}/lib/dri
  export GBM_BACKENDS_PATH=${mesaDrivers}/lib/gbm
  xvfb-run -a -s "-screen 0 1024x768x24" \
    dbus-run-session --config-file=${dbus}/share/dbus-1/session.conf -- \
    jsaddle-webkitgtk-demo --smoke 2>&1 | tee log || true
  grep -q "SMOKE-OK" log
  cp log $out
''
