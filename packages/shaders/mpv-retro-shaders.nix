{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "mpv-retro-shaders";
  version = "0-unstable-2025-07-24";

  src = fetchFromGitHub {
    owner = "hhirtz";
    repo = "mpv-retro-shaders";
    rev = "f4ea211db4e2afb5f5dc5a3daf816749c9cd7f03";
    hash = "sha256-Y3R4GDDJl5Tles4SYRYDXnrjnKQelfXkS2g0McHLIV0=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/${pname}/GLSL
    cp -r *glsl $out/share/${pname}/GLSL
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
