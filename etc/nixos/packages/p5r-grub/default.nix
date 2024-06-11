{ nvsrcs, stdenvNoCC, ... }:

let
  source = nvsrcs.p5r-grub;
in
stdenvNoCC.mkDerivation {
  inherit (source) pname version src;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    cp -r $src/themes $out

    runHook postInstall
  '';
}
