{ pkgs ? import nixpkgs (haskellNixpkgsArgs // (if system == null then {} else { inherit system; }) //
 { overlays = haskellNixpkgsArgs.overlays ++ [(self: super: {
   inherit (self.haskell-nix.haskellPackages) alex happy;
 })];})
, nixpkgs ? haskellNixSrc + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNixSrc
, haskellNixSrc ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/1dd6951d6a56115fd368c5b37b8dab47a90ec45c.tar.gz";
    sha256 = "02nhjy90xwg9wrp81cnlczndxags966c71wlbz6n91423a13mc99";
  }
, haskellCompiler ? "ghc881"
, system ? null
}:
let
  frameworks = pkgs.lib.optionals pkgs.stdenv.isDarwin (
    with pkgs.darwin.apple_sdk.frameworks; [ Cocoa Carbon CoreGraphics ]);
  project = pkgs.haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [
      { reinstallableLibGhc = true; }
      ({ config, ...}: {
        packages.gi-gtk.components.setup.frameworks = frameworks;
        packages.vault.components.library.doHaddock = false;
      })
    ];
  };
in
  project
