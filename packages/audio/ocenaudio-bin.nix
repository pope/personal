{
  lib,
  stdenvNoCC,
  _7zz,
  fetchurl,
  writeShellApplication,
  curl,
  git,
  gnused,
  jq,
  nix,
}:

stdenvNoCC.mkDerivation rec {
  pname = "ocenaudio-bin";
  version = "3.22.1";

  src = fetchurl {
    url = "https://www.ocenaudio.com/downloads/index.php/ocenaudio_universal.dmg?version=v${version}";
    hash = "sha256-AMPvtZvebMgxyRPNP1qd7Apw/czpYI08UqjqCTBBS7Y=";
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
      git
      gnused
      jq
      nix
    ];
    text = ''
      REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
      TARGET_FILE="$REPO_ROOT/packages/audio/ocenaudio-bin.nix"

      latest_version=$(curl --silent https://www.ocenaudio.com/changelog | sed -n "s%.*href=\"/download?version=v\\([^\"]*\\).*%\\1%p" | head -1)
      if [ -n "$latest_version" ] && [ "$latest_version" != "${version}" ]; then
        url="https://www.ocenaudio.com/downloads/index.php/ocenaudio_universal.dmg?version=v$latest_version"
        new_hash=$(nix store prefetch-file --json "$url" | jq -r .hash)

        sed -i "s/version = \"[^\"]*\";/version = \"$latest_version\";/" "$TARGET_FILE"
        sed -i "s|hash = \"[^\"]*\";|hash = \"$new_hash\";|" "$TARGET_FILE"
      fi
    '';
  });

  meta = {
    platforms = lib.platforms.darwin;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
