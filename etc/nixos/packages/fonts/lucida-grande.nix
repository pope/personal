{ stdenvNoCC, requireFile }:

stdenvNoCC.mkDerivation {
  pname = "lucida-grande";
  version = "0.0.1";

  src = requireFile rec {
    name = "lucida-grande.tar.gz";
    url = "file:///media/cyberia/nix-files/fonts/${name}";
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
