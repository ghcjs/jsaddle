#!/bin/bash -ex

echo $PATH
export LC_ALL=C.UTF-8
GHCVER=`ghc --numeric-version`

JSADDLE_WARP_VERSION=`head -n2 jsaddle-warp/jsaddle-warp.cabal | tail -n1 | sed 's/[^0-9.]//g'`

cabal update
cabal new-build 'jsaddle-warp:lib:jsaddle-warp' 'jsaddle-warp:test:test-tool'
cd jsaddle-warp
GHC_PACKAGE_PATH=/opt/ghc/$GHCVER/lib/ghc-$GHCVER/package.conf.d:~/.cabal/store/ghc-$GHCVER/package.db:../dist-newstyle/packagedb/ghc-$GHCVER ../dist-newstyle/build/x86_64-linux/ghc-$GHCVER/jsaddle-warp-$JSADDLE_WARP_VERSION/t/test-tool/build/test-tool/test-tool ../jsaddle

