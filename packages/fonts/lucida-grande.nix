{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation {
  pname = "lucida-grande";
  version = "0.0.1";

  src = fetchurl rec {
    name = "lucida-grande.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    sha256 = "0l0ba8cz4cm00d0z8qrxq1p141v4vf9ybqm34mznf2n9ya09jav7";
  };

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  doCheck = false;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/truetype/ *.ttf
    runHook postInstall
  '';
}
