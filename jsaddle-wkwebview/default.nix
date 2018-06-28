{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle, data-default
, buildPackages, hostPlatform
}:

let
  platform = if hostPlatform.useiOSPrebuilt
             then hostPlatform.xcodePlatform
             else "MacOSX";
  sdk = "${buildPackages.darwin.xcode}/Contents/Developer/Platforms/${platform}.platform/Developer/SDKs/${platform}.sdk";

in mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.9.4.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bytestring jsaddle data-default ];

  # HACK(matthewbauer): This overrides the builtin frameworks from
  # Nixpkgs. There should be a more elegant way to do this without
  # breaking other cflags - but haven’t found it yet.
  preBuild = ''
    NIX_CFLAGS_COMPILE="-F${sdk}/System/Library/Frameworks"
  '';

  libraryFrameworkDepends = [ "${sdk}/System" ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
