{
  alsa-lib,
  autoPatchelfHook,
  copyDesktopItems,
  curl,
  fetchurl,
  freetype,
  imagemagick,
  lib,
  libGL,
  libice,
  libjack2,
  libsm,
  libx11,
  makeBinaryWrapper,
  makeDesktopItem,
  requireFile,
  stdenv,
  unzip,
  zenity,
}:
let
  icon = fetchurl {
    url = "https://vital.audio/images/apple_touch_icon.png";
    hash = "sha256-NZ/AQ2gjBXUPUj3ITbowD7HuxRmEDuATOWidLqLNrww=";
  };

  version = "1.6.0";
  zipName = "VitalInstaller-${version}.zip";
in

stdenv.mkDerivation (finalAttrs: {
  pname = "vital";
  inherit version;

  src = requireFile {
    name = zipName;
    url = "file:///media/cyberia/nix-files/software/${zipName}";
    sha256 = "0jbcladqdgnycvj7j81kc1kx36fkf567k91ar58jfmwk57y4q829";
  };

  desktopItems = [
    (makeDesktopItem {
      type = "Application";
      name = "vital";
      desktopName = "Vital";
      comment = "Spectral warping wavetable synth";
      icon = "Vital";
      exec = "Vital";
      categories = [
        "Audio"
        "AudioVideo"
      ];
    })
  ];

  nativeBuildInputs = [
    autoPatchelfHook
    copyDesktopItems
    imagemagick
    makeBinaryWrapper
    unzip
  ];

  buildInputs = [
    alsa-lib
    (lib.getLib stdenv.cc.cc)
    freetype
    libGL
    libice
    libjack2
    libsm
    libx11
  ];

  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack
    unzip $src -d $TMPDIR
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons/hicolor/128x128/apps
    magick ${icon} -resize 128x128 $out/share/icons/hicolor/128x128/apps/Vital.png

    # copy each output to its destination (individually)
    mkdir -p $out/{bin,lib/{clap,vst,vst3}}
    for f in bin/Vital lib/{clap/Vital.clap,vst/Vital.so,vst3/Vital.vst3}; do
      cp -r VitalInstaller/$f $out/$f
    done

    wrapProgram $out/bin/Vital \
      --prefix LD_LIBRARY_PATH : "${
        lib.makeLibraryPath [
          curl
          libjack2
        ]
      }" \
      --prefix PATH : "${
        lib.makeBinPath [
          zenity
        ]
      }"

    runHook postInstall
  '';

  meta = {
    description = "Spectral warping wavetable synth";
    homepage = "https://vital.audio/";
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
    maintainers = with lib.maintainers; [
      PowerUser64
      l1npengtul
    ];
    mainProgram = "Vital";
  };
})
