{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle
, buildPackages
}:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.8.2.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle ]
    # should use `librarySystemDepends` but it is not propagated
    ++ (with buildPackages; [ osx_sdk darwin.libobjc darwin.apple_sdk.libs.xpc ]);
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
