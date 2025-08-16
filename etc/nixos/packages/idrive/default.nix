{ stdenv
, autoPatchelfHook
, dpkg
, nss
, nvsrcs
, libdrm
, libgbm
, alsa-lib
, gtk3
, expat
, wrapGAppsHook
}:

let
  source = nvsrcs.idrive;
in
stdenv.mkDerivation {
  inherit (source) pname version src;

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
    wrapGAppsHook
  ];

  buildInputs = [
    alsa-lib
    gtk3
    expat
    libdrm
    libgbm
    nss
  ];

  dontConfigure = true;
  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack
    dpkg -x $src .
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    cp -r usr $out/

    addAutoPatchelfSearchPath opt/IDriveForLinux/
    mkdir -p $out/bin
    cp -r opt $out/opt

    runHook postInstall
  '';
}
