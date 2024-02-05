{ nvsrcs, stdenvNoCC }:
let
  source = nvsrcs.fish-rose-pine;
in

stdenvNoCC.mkDerivation {
  name = source.pname;
  inherit (source) src version;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    install -D -t $out/share/fish/themes $src/themes/*
  '';
}
