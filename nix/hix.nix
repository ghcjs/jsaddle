{ modules = [({pkgs, ...}: {
    reinstallableLibGhc = !pkgs.stdenv.hostPlatform.isGhcjs;
    packages.jsaddle-warp.components.tests.test-tool.buildable = pkgs.lib.mkForce false;
    packages.jsaddle-webkit2gtk.components.library.buildable = pkgs.lib.mkForce pkgs.stdenv.hostPlatform.isLinux;
  })];
  shell.tools.cabal = {};
  shell.crossPlatforms = p: [ p.ghcjs ];
}
