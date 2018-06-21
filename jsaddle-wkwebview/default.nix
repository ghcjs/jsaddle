{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle, data-default
, buildPackages, hostPlatform
}:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.9.4.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle data-default ];

  # HACK(matthewbauer): Make sure framework is pulled in first so that
  # CoreFoundation is correct. Eventually we will update
  # CoreFoundation in Nixpkgs but that has caused some other issues.
  preBuild = stdenv.lib.optionalString hostPlatform.useiOSPrebuilt ''
    NIX_CFLAGS_COMPILE="-F${buildPackages.darwin.xcode_8_2}/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/System/Library/Frameworks"
  '';

  libraryFrameworkDepends = with buildPackages; if hostPlatform.useiOSPrebuilt then [
    "${darwin.xcode_8_2}/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/System"
  ] else [
    darwin.libobjc
    darwin.apple_sdk.libs.xpc
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Cocoa
    darwin.apple_sdk.frameworks.WebKit
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
