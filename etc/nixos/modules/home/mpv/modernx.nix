{ inputs, stdenvNoCC, makeFontsConf, ... }:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "modernx";
  version = "0.6.0";

  src = inputs.modernx;

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
