{ sourcesOverride ? {}
, sources ? import ./nix/sources.nix {} // sourcesOverride
, pkgs ? (import sources."haskell.nix" {}).pkgs
, compiler-nix-name ? "ghc8101"
, system ? null
}:
  pkgs.haskell-nix.project {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "jsaddle"; };
    modules = [{ reinstallableLibGhc = true; packages.jsaddle-warp.components.tests.test-tool.buildable = pkgs.lib.mkForce false; }];
  }
