{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "monolisa";
  version = "2.015";

  src = fetchurl rec {
    name = "monolisa-${version}.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    hash = "sha256-fpKZjwtzU2vDo74L0CNPm7eN1r59j/iZdOJsbCFqSAk=";
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
