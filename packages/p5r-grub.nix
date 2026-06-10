{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "p5r-grub";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "SiriusAhu";
    repo = "Persona_5_Royal_Grub_Themes";
    rev = "v${version}";
    hash = "sha256-YnTBgUWWsR0W8eTtg3oa2MIXSqj7HW555xSEhV7/74w=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    cp -r themes $out
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--flake" ];
  };
}
