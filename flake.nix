{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskell-ci.url = "github:haskell-CI/haskell-ci";
  inputs.haskell-ci.flake = false;
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, haskell-ci }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            inherit inputs;
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                # uncomment with your current system for `nix flake show` to work:
                #evalSystem = "x86_64-darwin";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
        # Runtime smoke test for the WebKitGTK backend (linux-only, headless).
        extraChecks = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          webkitgtk-smoke = pkgs.callPackage ./nix/webkitgtk-smoke.nix {
            jsaddle-webkitgtk-demo =
              flake.packages."jsaddle-webkitgtk:exe:jsaddle-webkitgtk-demo";
          };
        };
      in flake // {
        legacyPackages = pkgs;
        checks = flake.checks // extraChecks;
        hydraJobs = flake.hydraJobs // {
          checks = (flake.hydraJobs.checks or {}) // extraChecks;
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
