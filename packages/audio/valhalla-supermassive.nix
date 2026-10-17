{
  fetchurl,
  lib,
  stdenvNoCC,
  writeShellApplication,
  curl,
  git,
  gnused,
  jq,
  nix,
  unzip,
}:

stdenvNoCC.mkDerivation rec {
  pname = "valhalla-supermassive";
  version = "5_0_0";

  src = fetchurl {
    url = "https://valhallaproduction.s3.us-west-2.amazonaws.com/supermassive/ValhallaSupermassiveWin_V${version}.zip";
    hash = "sha256-Z+DO2+2eaAsR37WCYY3SV+LbqLjRAYnVmBHkwQQUkR0=";
  };

  dontUnpack = true;

  nativeBuildInputs = [ unzip ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    unzip $src -d $out
    runHook postInstall
  '';

  passthru.updateScript = lib.getExe (writeShellApplication {
    name = "update-valhalla-supermassive";
    runtimeInputs = [
      curl
      git
      gnused
      jq
      nix
    ];
    text = ''
      REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
      TARGET_FILE="$REPO_ROOT/packages/audio/valhalla-supermassive.nix"

      latest_version=$(curl -s https://valhalladsp.com/demos-downloads/ | sed -n -E 's/.*ValhallaSupermassiveWin_V([0-9_]+)\.zip.*/\1/p' | head -n1)
      if [ -n "$latest_version" ] && [ "$latest_version" != "${version}" ]; then
        url="https://valhallaproduction.s3.us-west-2.amazonaws.com/supermassive/ValhallaSupermassiveWin_V$latest_version.zip"
        new_hash=$(nix store prefetch-file --json "$url" | jq -r .hash)

        sed -i "s/version = \"[^\"]*\";/version = \"$latest_version\";/" "$TARGET_FILE"
        sed -i "s|hash = \"[^\"]*\";|hash = \"$new_hash\";|" "$TARGET_FILE"
      fi
    '';
  });

  meta = with lib; {
    homepage = "https://valhalladsp.com/demos-downloads/";
    description = "Valhalla Supermassive Delay/Reverb Plugin (Windows Installer)";
    license = licenses.unfree;
    platforms = platforms.all;
  };
}
