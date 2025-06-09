{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.prchann-font-collection;
in

stdenvNoCC.mkDerivation {
  inherit (source) src version;
  pname = "sf-pro";

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/opentype/ SF\ Pro/*.otf
    runHook postInstall
  '';
}
