{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "ia-writer";
  version = "0-unstable-2026-10-02";

  src = fetchFromGitHub {
    owner = "iaolo";
    repo = "iA-Fonts";
    rev = "c6588670c71e9ac628acc27b72cde4bf12726b7f";
    hash = "sha256-E/PA5cqZmRbaBYKJ/99YYhF/lgN6Stl/QoqTNLRI9bw=";
  };

  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype

    cp -R "iA Writer Duo/Static"/*.ttf $out/share/fonts/truetype/
    cp -R "iA Writer Duo/Variable"/*.ttf $out/share/fonts/truetype/

    cp -R "iA Writer Mono/Static"/*.ttf $out/share/fonts/truetype/
    cp -R "iA Writer Mono/Variable"/*.ttf $out/share/fonts/truetype/

    cp -R "iA Writer Quattro/Static"/*.ttf $out/share/fonts/truetype/
    cp -R "iA Writer Quattro/Variable"/*.ttf $out/share/fonts/truetype/

    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };
}
