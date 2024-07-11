{ stdenvNoCC }:

stdenvNoCC.mkDerivation {
  pname = "lucida-grande";
  version = "0.0.1";

  src = /media/cyberia/nix-files/fonts/lucida-grande;

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
