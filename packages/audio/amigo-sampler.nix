{
  autoPatchelfHook,
  curl,
  fetchurl,
  freetype,
  lib,
  stdenv,
  unzip,
}:

let
  version = "2.0.1";
  srcName = "Amigo_Sampler_v${lib.replaceStrings [ "." ] [ "_" ] version}";
  zipName = "${srcName}.zip";
in
stdenv.mkDerivation {
  pname = "amigo-sampler";
  inherit version;

  src = fetchurl {
    name = zipName;
    url = "https://skrapnel.gumiho-matrix.ts.net/nix-files/software/${zipName}";
    hash = "sha256-pAGFMEG1ODAPW5N0L9X8mafttpxKYLUMJoxhKyyYy5Q=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    unzip
  ];

  buildInputs = [
    curl
    freetype
    stdenv.cc.cc
  ];

  dontConfigure = true;
  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack
    unzip $src -d $TMPDIR
    unzip $TMPDIR/${srcName}/Amigo_Linux.zip -d $TMPDIR/linux
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/vst3
    cp -r linux/Amigo.vst3 $out/lib/vst3
    runHook postInstall
  '';

  meta.platforms = [ "x86_64-linux" ];
}
