{ mkDerivation, aeson, base, bytestring, containers, deepseq
, doctest, filepath, ghc-prim, http-types, jsaddle, lens, primitive
, process, QuickCheck, ref-tf, stdenv, stm, text, time
, transformers, wai, wai-websockets, warp, websockets, osx_sdk
, darwin, webdriver, phantomjs, ghc
}:
mkDerivation {
  pname = "jsaddle-warp";
  version = "0.8.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers http-types jsaddle stm text time transformers
    wai wai-websockets warp websockets
  ] ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.isDarwin then [
#    osx_sdk darwin.libobjc darwin.apple_sdk.libs.xpc
  ] else [
  ]);
  testHaskellDepends = [
    base bytestring deepseq doctest filepath ghc-prim jsaddle lens
    primitive process QuickCheck ref-tf webdriver phantomjs
  ];
  testTarget = "--test-option=${jsaddle.src}";
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
