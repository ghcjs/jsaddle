{ mkDerivation, aeson, base, bytestring, directory, gi-gio, gi-glib
, gi-gtk, gi-javascriptcore, gi-webkit2, haskell-gi-base, jsaddle
, stdenv, text, unix, webkit2gtk3-javascriptcore
}:
mkDerivation {
  pname = "jsaddle-webkit2gtk";
  version = "0.9.4.0";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    aeson base bytestring directory gi-gio gi-glib gi-gtk
    gi-javascriptcore gi-webkit2 haskell-gi-base jsaddle text unix
    webkit2gtk3-javascriptcore
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
