{
  copyDesktopItems,
  lib,
  makeDesktopItem,
  makeWrapper,
  requireFile,
  stdenv,
  steam-run,
  unzip,
}:

let
  version = "2.6.6";
  zipName = "RackPro-${version}-lin-x64.zip";
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
    copyDesktopItems
    makeWrapper
    unzip
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
    makeWrapper ${lib.getExe steam-run} $out/bin/Rack \
      --chdir $out/opt/VCV \
      --add-flag $out/opt/VCV/Rack

    mkdir -p $out/share/pixmaps
    ln -s $out/opt/VCV/res/icon.png $out/share/pixmaps/Rack.png

    # TODO(pope): Figure out VST support

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
