{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation {
  pname = "comic-code-ligatures";
  version = "0.0.1";

  src = fetchurl rec {
    name = "comic-code-ligatures.tar.gz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    hash = "sha256-oh96WOajoo0/iRC09jkQdIZn5I2p94bpHBPxiVMgD4c=";
  };

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
