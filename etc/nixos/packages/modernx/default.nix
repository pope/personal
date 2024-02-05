{ nvsrcs, stdenvNoCC, makeFontsConf, ... }:

let
  source = nvsrcs.modernx;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  inherit (source) pname version src;

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -D -t $out/share/mpv/scripts modernx.lua
    install -D -t $out/share/fonts/truetype Material-Design-Iconic-Font.ttf

    runHook postInstall
  '';

  passthru.scriptName = "modernx.lua";
  passthru.extraWrapperArgs = [
    "--set"
    "FONTCONFIG_FILE"
    (toString (makeFontsConf {
      fontDirectories = [ "${finalAttrs.finalPackage}/share/fonts" ];
    }))
  ];
})
