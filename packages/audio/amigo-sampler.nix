{
  autoPatchelfHook,
  curl,
  freetype,
  lib,
  requireFile,
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

  src = requireFile {
    name = zipName;
    url = "file:///media/cyberia/nix-files/software/${zipName}";
    sha256 = "156bk0n2nqcc4q6baq2akjvfv9wrzkajyx4kbc7k0f5m84q8a0d4";
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
}
