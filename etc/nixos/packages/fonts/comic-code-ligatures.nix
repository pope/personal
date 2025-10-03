{ stdenvNoCC, requireFile }:

stdenvNoCC.mkDerivation {
  pname = "comic-code-ligatures";
  version = "0.0.1";

  src = requireFile rec {
    name = "comic-code-ligatures.tar.gz";
    url = "file:///media/cyberia/nix-files/fonts/${name}";
    sha256 = "11qg419qkw8k3klqdxx9ipj6g1kl20wzdd0hi4zqv8m3wrc7l7x2";
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
