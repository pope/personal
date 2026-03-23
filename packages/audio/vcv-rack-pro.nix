{
  alsa-lib,
  autoPatchelfHook,
  copyDesktopItems,
  lib,
  libjack2,
  libGL,
  libX11,
  libpulseaudio,
  makeDesktopItem,
  makeWrapper,
  requireFile,
  stdenv,
  unzip,
  zenity,
}:

let
  version = "2.6.6";
  zipName = "RackPro-${version}-lin-x64.zip";
  runtimeBinaryInputs = [ zenity ];
in
stdenv.mkDerivation (finalAttrs: {
  pname = "vcv-rack-pro";
  inherit version;

  src = requireFile {
    name = zipName;
    url = "file:///media/cyberia/nix-files/software/${zipName}";
    sha256 = "0ixbwsx4pfbcg40qnmsjdq87nx99svjh4igv15x15avx4vxp1cc8";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    copyDesktopItems
    makeWrapper
    unzip
  ];

  buildInputs = [
    alsa-lib
    libjack2
    libGL
    libX11
    libpulseaudio
  ];

  dontConfigure = true;
  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack
    unzip $src -d $TMPDIR
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/opt/VCV
    cp -r Rack2Pro/* $out/opt/VCV

    mkdir -p $out/bin
    makeWrapper $out/opt/VCV/Rack $out/bin/Rack \
      --chdir $out/opt/VCV \
      --prefix PATH : ${lib.makeBinPath runtimeBinaryInputs}

    mkdir -p $out/share/pixmaps
    ln -s $out/opt/VCV/res/icon.png $out/share/pixmaps/Rack.png

    mkdir -p $out/lib/{clap,vst,vst3}
    cp 'VCV Rack 2.so' 'VCV Rack 2 FX.so' $out/lib/vst
    cp 'VCV Rack 2.clap' $out/lib/clap
    cp -r 'VCV Rack 2.vst3' $out/lib/vst3

    runHook postInstall
  '';

  desktopItems = [
    (makeDesktopItem {
      type = "Application";
      name = "vcv-rack-pro";
      desktopName = "VCV Rack Pro";
      genericName = "Eurorack simulator";
      comment = "Create music by patching together virtual synthesizer modules";
      exec = finalAttrs.meta.mainProgram;
      icon = "Rack";
      categories = [
        "AudioVideo"
        "AudioVideoEditing"
        "Audio"
      ];
      keywords = [ "music" ];
    })
  ];

  meta.mainProgram = "Rack";
})
