{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation {
  pname = "lucida-grande";
  version = "0.0.1";

  src = fetchurl rec {
    name = "lucida-grande.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    hash = "sha256-ZyuZgPLJCmd/JaPi5ZPbZAcSbsA9Y/RBA6Ay8hlSC1A=";
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
