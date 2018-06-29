{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle, data-default
, buildPackages, hostPlatform
}:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.9.4.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle data-default ];

  # HACK(matthewbauer): Can’t figure out why cf-private framework is
  #                     not getting pulled in correctly. Has something
  #                     to with how headers are looked up in xcode.
  preBuild = stdenv.lib.optionalString (!hostPlatform.useiOSPrebuilt) ''
    mkdir include
    ln -s ${buildPackages.darwin.cf-private}/Library/Frameworks/CoreFoundation.framework/Headers include/CoreFoundation
    export NIX_CFLAGS_COMPILE="-I$PWD/include $NIX_CFLAGS_COMPILE"
  '';

  libraryFrameworkDepends =
    stdenv.lib.optional (hostPlatform.useiOSPrebuilt)
      "${buildPackages.darwin.xcode}/Contents/Developer/Platforms/${hostPlatform.xcodePlatform}.platform/Developer/SDKs/${hostPlatform.xcodePlatform}.sdk/System"
    ++ stdenv.lib.optional (!hostPlatform.useiOSPrebuilt)
      (with buildPackages.darwin; with apple_sdk.frameworks; [
        Cocoa
        WebKit
      ]);
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
