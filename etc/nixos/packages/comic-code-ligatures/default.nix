{ stdenvNoCC }:

stdenvNoCC.mkDerivation {
  pname = "comic-code-ligatures";
  version = "0.0.1";

  src = /media/cyberia/nix-files/fonts/comic-code-ligatures;

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  doCheck = false;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ *.otf
    runHook postInstall
  '';
}
