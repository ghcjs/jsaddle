{ mkDerivation, aeson, base, bytestring, jsaddle, stdenv }:
mkDerivation {
  pname = "jsaddle-clib";
  version = "0.8.2.0";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
