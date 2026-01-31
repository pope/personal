{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.mpv-retro-shaders;
in

stdenvNoCC.mkDerivation {
  name = source.pname;
  inherit (source) src version;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/${source.pname}/GLSL
    cp -r *glsl $out/share/${source.pname}/GLSL
  '';
}
