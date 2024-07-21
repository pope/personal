{ nvsrcs
, lib
, bzip2
, cmake
, espeak
, kdePackages
, libusb1
, makeWrapper
, ninja
, pkg-config
, speex
, stdenv
, ...
}:

let
  source = nvsrcs.rbutil;
in
stdenv.mkDerivation {
  inherit (source) pname version src;

  nativeBuildInputs = [
    cmake
    makeWrapper
    ninja
    pkg-config
  ];

  buildInputs = [
    bzip2
    espeak
    kdePackages.full
    kdePackages.qttools
    libusb1
    speex
  ];

  dontWrapQtApps = true;

  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=None"
    "-Wno-dev"
  ];

  postPatch = ''
    sed '/add_subdirectory(themeeditor)/d' -i utils/CMakeLists.txt
  '';

  preConfigure = ''
    cd utils
  '';

  installPhase = ''
    runHook preInstall

    install -Dm 755 rbutilqt/RockboxUtility -t $out/bin/
    install -Dm 644 ../rbutilqt/RockboxUtility.desktop -t $out/share/applications/
    install -Dm 644 ../../docs/logo/rockbox-clef.svg -t $out/share/pixmaps/

    wrapProgram $out/bin/RockboxUtility --prefix PATH ${
      lib.makeBinPath [ espeak ]
    }

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://www.rockbox.org";
    description = "Open source firmware for digital music players";
    license = licenses.gpl2Plus;
    mainProgram = "RockboxUtility";
    platforms = platforms.linux;
  };
}
