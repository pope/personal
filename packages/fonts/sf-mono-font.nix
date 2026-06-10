{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "sf-mono-font";
  version = "0-unstable-2018-06-07";

  src = fetchFromGitHub {
    owner = "supercomputra";
    repo = "SF-Mono-Font";
    rev = "1409ae79074d204c284507fef9e479248d5367c1";
    hash = "sha256-3wG3M4Qep7MYjktzX9u8d0iDWa17FSXYnObSoTG2I/o=";
  };

  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ *.otf
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
