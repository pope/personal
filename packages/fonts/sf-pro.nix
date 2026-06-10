{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "sf-pro";
  version = "0-unstable-2022-09-02";

  src = fetchFromGitHub {
    owner = "prchann";
    repo = "fonts";
    rev = "e99955f794eb1f1375a5cb677d129758f51cbc3d";
    hash = "sha256-GlfGnaXNdvTLpwEssxzPyJwclgVi6iNp8hEGuy/rC5o=";
  };

  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ SF\ Pro/*.otf
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
