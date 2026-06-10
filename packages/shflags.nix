{
  fetchFromGitHub,
  getopt,
  runtimeShell,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "shflags";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "kward";
    repo = "shflags";
    rev = "v${version}";
    hash = "sha256-qOFPSYglb6p8GxagXVHdJW2namUCxi3REuR55On8QEo=";
  };

  buildInputs = [ getopt ];

  doCheck = true;
  checkPhase = ''
    ./test_runner -s ${runtimeShell}
  '';

  installPhase = ''
    runHook preInstall
    install -D -m 755 -t $out/bin shflags
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--flake" ];
  };
}
