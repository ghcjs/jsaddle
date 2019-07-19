{ mkDerivation, lib, stdenv, aeson, base, bytestring, jsaddle, data-default, pkgs }:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.9.6.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle data-default ];

  # Xcode is needed for iOS; macOS SDK is used otherwise
  libraryFrameworkDepends =
    if stdenv.hostPlatform.useiOSPrebuilt then
      "${pkgs.darwin.xcode}/Contents/Developer/Platforms/${stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${stdenv.hostPlatform.xcodePlatform}.sdk/System"
    else (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa WebKit ]);

  # cf-private is needed for NSURL (_OBJC_CLASS_$_NSURL) symbols
  buildDepends = lib.optional (!stdenv.hostPlatform.useiOSPrebuilt) [ pkgs.darwin.cf-private ];

  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
