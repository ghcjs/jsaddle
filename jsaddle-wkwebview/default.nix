{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle
, buildPackages
}:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.8.3.2";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle ]
    # should use `librarySystemDepends` but it is not propagated
    ++ (with buildPackages; [
      darwin.libobjc
      darwin.apple_sdk.libs.xpc
      darwin.apple_sdk.frameworks.Foundation
      darwin.apple_sdk.frameworks.Cocoa
      darwin.apple_sdk.frameworks.WebKit
    ]);
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
