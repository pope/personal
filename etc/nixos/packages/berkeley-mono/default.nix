{ stdenvNoCC, requireFile }:

stdenvNoCC.mkDerivation {
  pname = "berkeley-mono";
  version = "2.002";

  src = requireFile rec {
    name = "berkeley-mono.tar.xz";
    url = "file:///media/cyberia/nix-files/fonts/${name}";
    sha256 = "1hphd75n23d80v13306qmaqklwkbqi2g7f4rxrmwj7i3pbs6v3h9";
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
