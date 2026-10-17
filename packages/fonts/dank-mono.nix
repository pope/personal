{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation {
  pname = "dank-mono";
  version = "0.0.1";

  src = fetchurl rec {
    name = "dank-mono.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    hash = "sha256-fD7BW+ZIXzwWKNcsOl/Xik1NIFjKPCnyJiJZoM25UMo=";
  };

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  doCheck = false;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/truetype/ OpenType-TT/*.ttf
    runHook postInstall
  '';
}
