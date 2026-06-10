{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "artcnn";
  version = "1.6.2";

  src = fetchFromGitHub {
    owner = "Artoriuz";
    repo = "ArtCNN";
    rev = "v${version}";
    hash = "sha256-/cNJj7ah2Jux8pWGngPEjdhKRG1JsPBmb6EsJnQCCAM=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/${pname}
    cp -r GLSL $out/share/${pname}
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--flake" ];
  };
}
