{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.krigBilateral;
in

stdenvNoCC.mkDerivation {
  inherit (source) pname src version;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    install -Dm644 $src $out/KrigBilateral.glsl
  '';
}
