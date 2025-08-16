{ stdenv
, autoPatchelfHook
, nvsrcs
, popt
}:

let
  source = nvsrcs.idrive;
in
stdenv.mkDerivation {
  inherit (source) pname version src;

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    popt
  ];

  dontConfigure = true;
  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack

    tail -n+$(awk '/^__idrive__/ { print NR + 1; exit 0; }' $src) $src | tar xz
    tar xzf IDriveForLinux/bin/Idrivelib/dependencies/linuxbin/k3/x86_64/idrive.tar.gz
    tar xzf IDriveForLinux/bin/Idrivelib/dependencies/evsbin/IDrive_linux_64bit.tar.gz

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp idrive $out/bin/
    cp IDrive_linux_64bit/* $out/bin/

    runHook postInstall
  '';
}
