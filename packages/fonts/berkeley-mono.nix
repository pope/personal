{ stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation {
  pname = "berkeley-mono";
  version = "2.002";

  src = fetchurl rec {
    name = "berkeley-mono.tar.xz";
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/fonts/${name}";
    hash = "sha256-CY5t9LojHslr7pm480TEa3I6sarYgDHCBqgNYctp8MI=";
  };

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  doCheck = false;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ TX-02/*.otf
    runHook postInstall
  '';
}
