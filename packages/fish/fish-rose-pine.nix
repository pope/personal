{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "fish-rose-pine";
  version = "0-unstable-2026-09-23";

  src = fetchFromGitHub {
    owner = "rose-pine";
    repo = "fish";
    rev = "7d3b517adc0eb52b43c4b22bdc774b062271f969";
    hash = "sha256-V+MKT2YqkuIqvWtbuB2/Sdn2siZwBKcZSewRe62DBp0=";
  };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    install -D -t $out/share/fish/themes $src/themes/*
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
