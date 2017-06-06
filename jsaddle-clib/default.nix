{ mkDerivation, aeson, base, bytestring, data-default, jsaddle, stdenv, text }:
mkDerivation {
  pname = "jsaddle-clib";
  version = "0.9.0.0";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [ aeson base bytestring data-default jsaddle text ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
