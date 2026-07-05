{config, pkgs, ...}: {
  compiler-nix-name = "ghc914";
  flake.variants.ghc96.compiler-nix-name = pkgs.lib.mkForce "ghc96";
  modules = [({pkgs, lib, ...}: let
    # WebView2.h for jsaddle-webview2's C shim.  Only the header is needed
    # (WebView2Loader.dll is loaded dynamically at run time), so nothing
    # from the SDK is linked into the build.
    webview2-sdk = pkgs.pkgsBuildBuild.fetchzip {
      url = "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4022.49";
      extension = "zip";
      stripRoot = false;
      hash = "sha256-RoVh4A/Pg9/40kHtIIsC916QgPkB8TnDeOvN4ptPNM4=";
    };
  in {
    package-keys = ["webkit2gtk3-javascriptcore" "jsaddle-webview2"];
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
    enableStatic = !pkgs.stdenv.hostPlatform.isGhcjs;
    packages.jsaddle-webview2.components.library.configureFlags =
      lib.optionals pkgs.stdenv.hostPlatform.isWindows
        [ "--extra-include-dirs=${webview2-sdk}/build/native/include" ];
  })];
  shell.buildInputs = [ pkgs.pkgsBuildBuild.nodejs ];
  shell.tools.cabal = {};
  shell.tools.haskell-ci.src = pkgs.inputs.haskell-ci;
  shell.tools.haskell-ci.cabalProjectLocal = ''
    allow-newer: *:base
  '';
  crossPlatforms = p: [ p.ghcjs p.ucrt64 ];

  # Use this for checking if `aeson` 2 works (tests will not build because `webdriver` still needs aeson <2)
  cabalProjectLocal = ''
    constraints: aeson >=2
  '';
}
