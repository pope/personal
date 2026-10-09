{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "fish-rose-pine";
  version = "0-unstable-2026-10-07";

  src = fetchFromGitHub {
    owner = "rose-pine";
    repo = "fish";
    rev = "b4ccaddaafc91d3c991d542830fdf2fb61128ba0";
    hash = "sha256-nTMaJ2sKa/EyvH5iOndS6Q0mdjFOqIiSh/gnLmdb5t8=";
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
