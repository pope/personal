{
  fetchFromGitHub,
  stdenvNoCC,
  nix-update-script,
}:

stdenvNoCC.mkDerivation rec {
  pname = "ia-writer";
  version = "0-unstable-2023-06-16";

  src = fetchFromGitHub {
    owner = "iaolo";
    repo = "iA-Fonts";
    rev = "f32c04c3058a75d7ce28919ce70fe8800817491b";
    hash = "sha256-2T165nFfCzO65/PIHauJA//S+zug5nUwPcg8NUEydfc=";
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
