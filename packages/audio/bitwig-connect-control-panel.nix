{
  alsa-lib,
  atk,
  autoPatchelfHook,
  cairo,
  dpkg,
  fetchurl,
  freetype,
  gdk-pixbuf,
  giflib,
  glib,
  gtk3,
  harfbuzz,
  lcms2,
  lib,
  libglvnd,
  libjpeg8,
  libpng,
  libusb1,
  libx11,
  libxcb,
  libxcb-wm,
  libxcursor,
  libxext,
  libxi,
  libxkbcommon,
  libxrender,
  libxtst,
  makeBinaryWrapper,
  pango,
  stdenv,
  systemdLibs,
  vulkan-loader,
  wrapGAppsHook3,
  xcb-imdkit,
  zlib,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "bitwig-connect-control-panel";
  version = "1.0";

  src = fetchurl {
    url = "https://downloads.bitwig.com/connect-control-panel/${finalAttrs.version}/bitwig-connect-control-panel-${finalAttrs.version}.deb";
    hash = "sha256-Nm2hADLOtvl2/rc0v2CWwGZh7g9gDTJJrDC3gDpgb6w=";
  };

  strictDeps = true;

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeBinaryWrapper
    wrapGAppsHook3
  ];

  buildInputs = [
    alsa-lib
    atk
    cairo
    freetype
    gdk-pixbuf
    giflib
    glib
    gtk3
    harfbuzz
    lcms2
    libglvnd
    (lib.getLib stdenv.cc.cc)
    (lib.getLib libjpeg8)
    libpng
    libusb1
    libx11
    libxcb
    libxcb-wm
    libxcursor
    libxext
    libxi
    libxkbcommon
    libxrender
    libxtst
    pango
    systemdLibs
    vulkan-loader
    xcb-imdkit
    zlib
  ];

  dontWrapGApps = true;

  unpackPhase = ''
    runHook preUnpack
    dpkg-deb -x $src .
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    cp -r usr/share "$out"/
    mkdir -p "$out"/libexec
    cp -r opt/bitwig-control-panel "$out"/libexec/bitwig-connect-control-panel

    mkdir -p "$out"/lib/udev/rules.d
    cp etc/udev/rules.d/00-bitwig.rules "$out"/lib/udev/rules.d/

    runHook postInstall
  '';

  postFixup = ''
    for e in "$out"/libexec/bitwig-connect-control-panel/bin/*gtk*; do
      if [ -f "$e" ] && [ -x "$e" ]; then
        wrapProgram "$e" "''${gappsWrapperArgs[@]}"
      fi
    done

    makeWrapper "$out"/libexec/bitwig-connect-control-panel/BitwigConnectControlPanel "$out"/bin/bitwig-control-panel \
      "''${gappsWrapperArgs[@]}" \
      --set-default VK_LOADER_DRIVERS_DISABLE "*"
  '';

  meta = {
    description = "Control panel for Bitwig Connect audio interfaces";
    homepage = "https://www.bitwig.com/";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
    mainProgram = "bitwig-control-panel";
  };
})
