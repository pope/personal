{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.ia-writer;
in

stdenvNoCC.mkDerivation {
  inherit (source) pname src version;

  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype

    cp -R $src/iA\ Writer\ Duo/Static/*.ttf $out/share/fonts/truetype
    cp -R $src/iA\ Writer\ Duo/Variable/*.ttf $out/share/fonts/truetype

    cp -R $src/iA\ Writer\ Mono/Static/*.ttf $out/share/fonts/truetype
    cp -R $src/iA\ Writer\ Mono/Variable/*.ttf $out/share/fonts/truetype

    cp -R $src/iA\ Writer\ Quattro/Static/*.ttf $out/share/fonts/truetype
    cp -R $src/iA\ Writer\ Quattro/Variable/*.ttf $out/share/fonts/truetype

    runHook postInstall
  '';
}
