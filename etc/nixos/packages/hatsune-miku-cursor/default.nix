{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.hatsune-miku-cursor;
in

stdenvNoCC.mkDerivation {
  inherit (source) pname src version;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/icons
    cp -R miku-cursor-linux $out/share/icons
    runHook postInstall
  '';
}

