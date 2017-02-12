haskellPackages: {
  jsaddle = haskellPackages.callPackage ./jsaddle {};
  jsaddle-warp = haskellPackages.callPackage ./jsaddle-warp {};
  jsaddle-wkwebview = haskellPackages.callPackage ./jsaddle-wkwebview {};
  jsaddle-webkit2gtk = haskellPackages.callPackage ./jsaddle-webkit2gtk {};
  jsaddle-webkitgtk = haskellPackages.callPackage ./jsaddle-webkitgtk {};
  jsaddle-clib = haskellPackages.callPackage ./jsaddle-clib {};
}
