{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.sf-mono-font;
in

stdenvNoCC.mkDerivation {
  inherit (source) pname src version;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ *.otf
    runHook postInstall
  '';
}
