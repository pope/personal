{
  nvsrcs,
  lib,
  stdenvNoCC,
  _7zz,
}:

let
  source = nvsrcs.ocenaudio-bin;
in
stdenvNoCC.mkDerivation {
  inherit (source) pname version src;

  sourceRoot = ".";

  nativeBuildInputs = [
    _7zz
  ];

  unpackPhase = ''
    runHook preUnpack
    7zz -snld x $src
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/Applications
    cp -r ocenaudio/*.app $out/Applications
    runHook postInstall
  '';

  meta = {
    platforms = lib.platforms.darwin;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
}
