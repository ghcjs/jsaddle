{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, deepseq, filepath, ghc-prim, http-types
, lens, primitive, process, random, ref-tf, scientific, stdenv, stm
, text, time, transformers, unordered-containers, unliftio-core
, vector, exceptions, ghcjs-base, ghcjs-prim, ghc
}:
mkDerivation {
  pname = "jsaddle";
  version = "0.9.6.0";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring exceptions lens primitive
    text transformers
  ] ++ (if ghc.isGhcjs or false then [
    ghcjs-base ghcjs-prim
  ] else [
    attoparsec containers deepseq filepath ghc-prim http-types process
    random ref-tf scientific stm time unordered-containers vector
    unliftio-core
  ]);
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
