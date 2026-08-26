{
  fetchFromGitHub,
  lib,
  stdenv,
  nix-update-script,
}:

stdenv.mkDerivation rec {
  pname = "iqm";
  version = "0-unstable-2026-08-15";

  src = fetchFromGitHub {
    owner = "lsalzman";
    repo = "iqm";
    rev = "1077b9c195a7f76f9b26266562f6e36bb4d5dac9";
    hash = "sha256-/RIxxhsZrBKAYfkVxd8QLVxwAKBz+XMtiheKXLbRP5g=";
  };

  installPhase = ''
    runHook preInstall
    install -Dm 755 iqm -t $out/bin/
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };

  meta = with lib; {
    homepage = "https://github.com/lsalzman/iqm";
    description = "IQM Developer Kit";
    license = licenses.mit;
    mainProgram = "iqm";
    platforms = platforms.all;
  };
}
