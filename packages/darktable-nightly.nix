/*
  darktable-nightly: Nightly (git master) build of Darktable.

  Key Features & Customizations:
  - Builds from darktable-org/darktable git master with submodules.
  - AI acceleration enabled by default (onnxruntime + USE_AI=ON).
  - Injects Leica M11-D noise profiles based on M11 data in data/noiseprofiles.json.
  - Automatically registered via umport into self.packages and overlays.default.

  Coexistence & Isolation Best Practices:
  - DATABASE ISOLATION:
    Darktable's SQLite library and data databases (library.db, data.db) undergo
    one-way schema migrations when opened by a newer version. Once upgraded by
    a nightly build, your stable darktable release will NO LONGER be able to
    open them.
    To prevent accidental corruption, the wrapper script in this derivation
    strictly isolates nightly data by passing:
      --configdir "${XDG_CONFIG_HOME:-$HOME/.config}/darktable-nightly"
      --cachedir "${XDG_CACHE_HOME:-$HOME/.cache}/darktable-nightly"

  - SIDECAR (.XMP) CAUTION:
    Nightly builds frequently introduce or update processing modules. If nightly
    writes adjustments to an image's .xmp sidecar, opening that photo in the
    canonical/stable build may trigger unsupported module warnings or visual
    rendering shifts.
    Recommendation:
      - Test nightly on dedicated photo copies/scratch directories, or
      - In darktable nightly preferences (Preferences > Storage), disable
        "write sidecar file for each image" or set to "never".

  - TESTING WITH REAL LIBRARY DATA:
    If you wish to test your existing stable catalog in nightly, copy (never
    symlink or share) your databases:
      cp ~/.config/darktable/library.db ~/.config/darktable-nightly/library.db
      cp ~/.config/darktable/data.db ~/.config/darktable-nightly/data.db

  - LAUNCHING:
    - CLI: `darktable-nightly` (stable remains `darktable`)
    - GUI: Select "Darktable (Nightly)" in your application launcher.

  - UPDATING:
    To update to the latest git master commit:
      nix run .#update-my-packages
    or:
      nix-update --flake --use-update-script darktable-nightly
*/

{
  lib,
  stdenv,
  fetchFromGitHub,
  darktable,
  makeDesktopItem,
  writeShellApplication,
  curl,
  jq,
  gnused,
  git,
  nix-prefetch-github,
  withAi ? true,
}:

let
  version = "0-unstable-2026-10-09";
  rev = "8c64bf7ca9a8b72fba4e426900c924148c7a7309";
  hash = "sha256-jyVjyHS3k0skf8bCk6NEpp/aEpZ4F9kDuoYQNVgJflw=";

  darktable-base = darktable.override { inherit withAi; };

  darktable-unwrapped = darktable-base.overrideAttrs (oldAttrs: {
    pname = "darktable-unwrapped-nightly";
    inherit version;

    src = fetchFromGitHub {
      owner = "darktable-org";
      repo = "darktable";
      inherit rev hash;
      fetchSubmodules = true;
    };

    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ jq ];

    postPatch = (oldAttrs.postPatch or "") + ''
      patchShebangs tools/
      echo '#!/bin/sh' > tools/get_git_version_string.sh
      echo 'echo "${version}"' >> tools/get_git_version_string.sh
      chmod +x tools/get_git_version_string.sh

      jq '(.noiseprofiles[].models) |= . + [ .[] | select(.model == "M11") | .model = "M11-D" ]' \
        data/noiseprofiles.json > data/noiseprofiles.json.tmp
      mv data/noiseprofiles.json.tmp data/noiseprofiles.json
    '';

    doInstallCheck = false;
  });

  desktopItem = makeDesktopItem {
    name = "darktable-nightly";
    desktopName = "Darktable (Nightly)";
    genericName = "Virtual Lighttable and Darkroom";
    comment = "Nightly development build of darktable";
    exec = "darktable-nightly %U";
    icon = "darktable";
    terminal = false;
    categories = [
      "Graphics"
      "Photography"
    ];
    mimeTypes = [
      "image/x-dcraw"
      "image/jpeg"
    ];
  };

  updateScript = lib.getExe (writeShellApplication {
    name = "update-darktable-nightly";
    runtimeInputs = [
      curl
      jq
      gnused
      git
      nix-prefetch-github
    ];
    text = ''
      REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
      TARGET_FILE="$REPO_ROOT/packages/darktable-nightly.nix"

      echo "Checking latest commit for darktable master..."
      latest_commit_info=$(curl -s https://api.github.com/repos/darktable-org/darktable/commits/master)
      latest_rev=$(echo "$latest_commit_info" | jq -r .sha)
      latest_date=$(echo "$latest_commit_info" | jq -r .commit.committer.date | cut -d'T' -f1)
      latest_version="0-unstable-$latest_date"

      if [ "$latest_rev" = "null" ] || [ -z "$latest_rev" ]; then
        echo "Failed to fetch latest commit information from GitHub." >&2
        exit 1
      fi

      if [ "$latest_rev" != "${rev}" ]; then
        echo "New commit found: $latest_rev ($latest_version). Prefetching submodules..."
        new_hash=$(nix-prefetch-github darktable-org darktable --rev "$latest_rev" --fetch-submodules | jq -r .hash)

        echo "Updating $TARGET_FILE..."
        sed -i "s/version = \"[^\"]*\";/version = \"$latest_version\";/" "$TARGET_FILE"
        sed -i "s/rev = \"[^\"]*\";/rev = \"$latest_rev\";/" "$TARGET_FILE"
        sed -i "s|hash = \"[^\"]*\";|hash = \"$new_hash\";|" "$TARGET_FILE"
        echo "Updated darktable-nightly to $latest_version ($latest_rev)"
      else
        echo "darktable-nightly is already up to date ($latest_rev)."
      fi
    '';
  });

in
stdenv.mkDerivation {
  pname = "darktable-nightly";
  inherit version;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/share/applications $out/share/icons

    cat <<'EOF' > $out/bin/darktable-nightly
    #!/usr/bin/env bash
    set -euo pipefail
    CONFIG_DIR="''${XDG_CONFIG_HOME:-$HOME/.config}/darktable-nightly"
    CACHE_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/darktable-nightly"
    mkdir -p "$CONFIG_DIR" "$CACHE_DIR"
    exec "${darktable-unwrapped}/bin/darktable" \
      --configdir "$CONFIG_DIR" \
      --cachedir "$CACHE_DIR" \
      "$@"
    EOF
    chmod +x $out/bin/darktable-nightly

    cat <<'EOF' > $out/bin/darktable-cli-nightly
    #!/usr/bin/env bash
    set -euo pipefail
    CONFIG_DIR="''${XDG_CONFIG_HOME:-$HOME/.config}/darktable-nightly"
    CACHE_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/darktable-nightly"
    exec "${darktable-unwrapped}/bin/darktable-cli" \
      --configdir "$CONFIG_DIR" \
      --cachedir "$CACHE_DIR" \
      "$@"
    EOF
    chmod +x $out/bin/darktable-cli-nightly

    cp ${desktopItem}/share/applications/* $out/share/applications/
    ln -s ${darktable-unwrapped}/share/icons/* $out/share/icons/
    runHook postInstall
  '';

  passthru = {
    inherit darktable-unwrapped updateScript;
  };

  meta = darktable-unwrapped.meta // {
    mainProgram = "darktable-nightly";
    description = "${
      darktable-unwrapped.meta.description or "Virtual lighttable and darkroom"
    } (Nightly Build)";
  };
}
