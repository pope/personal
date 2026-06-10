{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "fish-tokyonight";
  version = "4.8.0";

  src = fetchFromGitHub {
    owner = "vitallium";
    repo = "tokyonight-fish";
    rev = "v${version}";
    hash = "sha256-JI1kTez4CeMpSKcSikFUee15N48zkJJOvLHCi0H2PUc=";
  };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    install -D -t $out/share/fish/themes $src/themes/*
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--flake" ];
  };
}
