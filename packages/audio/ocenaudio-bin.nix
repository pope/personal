{
  lib,
  stdenvNoCC,
  _7zz,
  fetchurl,
  writeShellApplication,
  curl,
  gnused,
  python3,
  nix,
}:

stdenvNoCC.mkDerivation rec {
  pname = "ocenaudio-bin";
  version = "3.19.3";

  src = fetchurl {
    url = "https://www.ocenaudio.com/downloads/index.php/ocenaudio_universal.dmg?version=v${version}";
    hash = "sha256-jDvNbRbhASoWxMN1edTOWjIEE5Uask/HBhs2RQ9LCik=";
  };

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

  passthru.updateScript = lib.getExe (writeShellApplication {
    name = "update-ocenaudio-bin";
    runtimeInputs = [
      curl
      gnused
      python3
      nix
    ];
    text = ''
      latest_version=$(curl --silent https://www.ocenaudio.com/changelog | sed -n "s%.*href=\"/download?version=v\\([^\"]*\\).*%\\1%p" | head -1)
      if [ "$latest_version" != "${version}" ]; then
        url="https://www.ocenaudio.com/downloads/index.php/ocenaudio_universal.dmg?version=v$latest_version"
        new_hash=$(nix store prefetch-file --json "$url" | python3 -c "import sys, json; print(json.load(sys.stdin)['hash'])")

        sed -i "s/version = \"[^\"]*\";/version = \"$latest_version\";/" packages/audio/ocenaudio-bin.nix
        sed -i "s|hash = \"[^\"]*\";|hash = \"$new_hash\";|" packages/audio/ocenaudio-bin.nix
      fi
    '';
  });

  meta = {
    platforms = lib.platforms.darwin;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
