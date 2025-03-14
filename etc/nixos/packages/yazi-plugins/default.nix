{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.yazi-plugins;
in

stdenvNoCC.mkDerivation {
  name = source.pname;
  inherit (source) src version;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/yazi/plugins
    cp -r $src/LICENSE $out/share/yazi/plugins/
    cp -r $src/*.yazi $out/share/yazi/plugins/
  '';
}

