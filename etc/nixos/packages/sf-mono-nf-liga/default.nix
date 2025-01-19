{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.sf-mono-nf-liga;
in

stdenvNoCC.mkDerivation {
  inherit (source) pname src version;

  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ *.otf
    runHook postInstall
  '';
}
