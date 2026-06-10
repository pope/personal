{
  fetchurl,
  lib,
  stdenvNoCC,
  writeShellApplication,
  curl,
  gnused,
  python3,
  nix,
}:

stdenvNoCC.mkDerivation rec {
  pname = "valhalla-supermassive";
  version = "5_0_0";

  src = fetchurl {
    url = "https://valhallaproduction.s3.us-west-2.amazonaws.com/supermassive/ValhallaSupermassiveWin_V${version}.zip";
    hash = "sha256-Z+DO2+2eaAsR37WCYY3SV+LbqLjRAYnVmBHkwQQUkR0=";
  };

  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp $src $out/
    runHook postInstall
  '';

  passthru.updateScript = lib.getExe (writeShellApplication {
    name = "update-valhalla-supermassive";
    runtimeInputs = [
      curl
      gnused
      python3
      nix
    ];
    text = ''
      latest_version=$(curl -s https://valhalladsp.com/demos-downloads/ | sed -n -E 's/.*ValhallaSupermassiveWin_V([0-9_]+)\.zip.*/\1/p' | head -n1)
      if [ -n "$latest_version" ] && [ "$latest_version" != "${version}" ]; then
        url="https://valhallaproduction.s3.us-west-2.amazonaws.com/supermassive/ValhallaSupermassiveWin_V$latest_version.zip"
        new_hash=$(nix store prefetch-file --json "$url" | python3 -c "import sys, json; print(json.load(sys.stdin)['hash'])")

        sed -i "s/version = \"[^\"]*\";/version = \"$latest_version\";/" packages/audio/valhalla-supermassive.nix
        sed -i "s|hash = \"[^\"]*\";|hash = \"$new_hash\";|" packages/audio/valhalla-supermassive.nix
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
