{ filterSource ? (x: x) }:
self: super:
let
  pkgs = self.callPackage ({pkgs}: pkgs) {};
  inherit (pkgs) lib;
in
with pkgs.haskell.lib;
overrideCabal (self.callCabal2nix "jsaddle-wkwebview" (filterSource ./.) {}) (drv: {
  libraryFrameworkDepends = (drv.libraryFrameworkDepends or []) ++
    (if pkgs.stdenv.hostPlatform.useiOSPrebuilt then [
        "${pkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${pkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${pkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System"
      ] else (with pkgs.buildPackages.darwin.apple_sdk.frameworks; [ Cocoa WebKit  ]));
  buildDepends = lib.optional (!pkgs.stdenv.hostPlatform.useiOSPrebuilt) [ pkgs.buildPackages.darwin.cf-private ];
})
