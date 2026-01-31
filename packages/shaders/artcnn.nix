{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.artcnn;
in

stdenvNoCC.mkDerivation {
  name = source.pname;
  inherit (source) src version;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/${source.pname}
    cp -r GLSL $out/share/${source.pname}
  '';
}
