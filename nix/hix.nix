{pkgs, ...}:
{ modules = [({pkgs, ...}: {
    packages.jsaddle-webkit2gtk.components.library.buildable = pkgs.lib.mkForce pkgs.stdenv.hostPlatform.isLinux;
  })];
  shell.buildInputs = [ pkgs.nodejs ];
  shell.tools.cabal = {};
  shell.crossPlatforms = p: [ p.ghcjs ];
}
