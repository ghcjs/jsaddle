{ mkDerivation, aeson, base, bytestring, jsaddle, stdenv
, osx_sdk, darwin }:
mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.8.2.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle
      osx_sdk darwin.libobjc darwin.apple_sdk.libs.xpc ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
