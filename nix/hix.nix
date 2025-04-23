{config, pkgs, ...}: {
  compiler-nix-name = "ghc912";
  flake.variants.ghc96.compiler-nix-name = pkgs.lib.mkForce "ghc96";
  modules = [({pkgs, ...}: {
    package-keys = ["webkit2gtk3-javascriptcore"];
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
    enableStatic = !pkgs.stdenv.hostPlatform.isGhcjs;
  })];
  shell.buildInputs = [ pkgs.pkgsBuildBuild.nodejs ];
  shell.tools.cabal = {};
  shell.tools.haskell-ci.src = pkgs.inputs.haskell-ci;
  shell.tools.haskell-ci.cabalProjectLocal = ''
    allow-newer: *:base
  '';
  crossPlatforms = p: [ p.ghcjs ];

  # Use this for checking if `aeson` 2 works (tests will not build because `webdriver` still needs aeson <2)
  cabalProjectLocal = ''
    constraints: aeson >=2
  '';
}
