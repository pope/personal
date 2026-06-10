{
  fetchFromGitHub,
  lib,
  stdenv,
  nix-update-script,
}:

stdenv.mkDerivation rec {
  pname = "iqm";
  version = "0-unstable-2025-07-24";

  src = fetchFromGitHub {
    owner = "lsalzman";
    repo = "iqm";
    rev = "8554dca2b86dc3845981fad6758d3829ce0d4a56";
    hash = "sha256-zR3AdHg0f2g14ZHVgTREYheXcHr6evnZxP3HEo7byRI=";
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
