{pkgs, ...}:
{ modules = [({pkgs, ...}: {
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
  })];
  shell.buildInputs = [ pkgs.nodejs ];
  shell.tools.cabal = {};
  shell.crossPlatforms = p: [ p.ghcjs ];
}
