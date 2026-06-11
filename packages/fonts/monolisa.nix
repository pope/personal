{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "monolisa";
  version = "2.015";

  src = fetchurl rec {
    name = "monolisa-${version}.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    sha256 = "02a8d8hnqv72fjczi3vxpvb8vdwv9wix02xylg1nnlvk1f7rk4ky";
  };

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  doCheck = false;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/truetype/ ttf/*
    runHook postInstall
  '';
}
