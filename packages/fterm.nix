{
  alsa-lib,
  at-spi2-atk,
  autoPatchelfHook,
  cairo,
  copyDesktopItems,
  curl,
  fetchurl,
  fontconfig,
  gdk-pixbuf,
  git,
  glib,
  gnused,
  gtk3,
  harfbuzz,
  jq,
  lib,
  libepoxy,
  libglvnd,
  libvorbis,
  makeDesktopItem,
  makeWrapper,
  nix,
  pango,
  stdenv,
  wayland,
  writeShellApplication,
}:

let
  version = "0.8.1";

  sources = {
    x86_64-linux = {
      asset = "fterm-linux-x64.tar.gz";
      hash = "sha256-/9qUoHLldt2NdKIbz/tsXzLP0wn/TydraYTmZ895HgQ=";
    };
    aarch64-darwin = {
      asset = "fterm-macos-arm64.tar.gz";
      hash = "sha256-mshPqJjndHo4FOvH0vZ9ydXp0dpXHcVGXU/ZEmzEY5M=";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");

  runtimeLibs = [
    alsa-lib
    libglvnd
  ];
in
stdenv.mkDerivation (finalAttrs: {
  pname = "fterm";
  inherit version;

  src = fetchurl {
    url = "https://github.com/jtmcdole/termui/releases/download/fterm-v${finalAttrs.version}/${source.asset}";
    inherit (source) hash;
  };

  unpackPhase = ''
    runHook preUnpack
    mkdir -p source
    tar -xzf "$src" -C source
    runHook postUnpack
  '';

  sourceRoot = "source";

  strictDeps = true;

  nativeBuildInputs =
    lib.optionals stdenv.hostPlatform.isDarwin [
      makeWrapper
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      autoPatchelfHook
      copyDesktopItems
    ];

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    alsa-lib
    at-spi2-atk
    cairo
    fontconfig
    gdk-pixbuf
    glib
    gtk3
    harfbuzz
    libepoxy
    libvorbis
    pango
    (lib.getLib stdenv.cc.cc)
    wayland
  ];

  dontConfigure = true;
  dontBuild = true;

  # Ensure dlopen'd Dart native assets (e.g. libpty2_unix.so, libsoloud_cli.so)
  # and runtime audio/GL libraries resolve via RUNPATH without polluting
  # LD_LIBRARY_PATH or GTK/GLib env vars for shells spawned inside fterm.
  appendRunpaths = lib.optionals stdenv.hostPlatform.isLinux (
    [ "${placeholder "out"}/libexec/fterm/lib" ] ++ map (pkg: "${lib.getLib pkg}/lib") runtimeLibs
  );

  installPhase =
    if stdenv.hostPlatform.isDarwin then
      ''
        runHook preInstall

        mkdir -p "$out/Applications" "$out/bin"
        cp -a fterm.app "$out/Applications/"
        makeWrapper "$out/Applications/fterm.app/Contents/MacOS/fterm" "$out/bin/fterm"

        runHook postInstall
      ''
    else
      ''
        runHook preInstall

        mkdir -p "$out/libexec/fterm" "$out/bin"
        cp -a . "$out/libexec/fterm/"
        chmod +x "$out/libexec/fterm/fterm" "$out/libexec/fterm/helpers/impellerc"

        # libdartjni.so is bundled by Dart JNI dependencies, unused on Linux desktop,
        # and has an unresolvable DT_NEEDED on libjvm.so.
        rm -f "$out/libexec/fterm/lib/libdartjni.so"

        # Upstream bundles libvorbis/libvorbisfile without libvorbisenc; symlink to
        # nixpkgs libvorbis so libflutter_soloud_plugin.so doesn't load two copies.
        for f in "$out"/libexec/fterm/lib/libvorbis*.so*; do
          base="$(basename "$f")"
          if [ -e "${lib.getLib libvorbis}/lib/$base" ]; then
            ln -sf "${lib.getLib libvorbis}/lib/$base" "$f"
          fi
        done

        # Flutter resolves data/ and lib/ via /proc/self/exe, so a plain symlink
        # avoids leaking GTK/GLib wrapper env vars into child shells.
        ln -s "$out/libexec/fterm/fterm" "$out/bin/fterm"

        install -Dm644 data/flutter_assets/web/favicon.svg \
          "$out/share/icons/hicolor/scalable/apps/fterm.svg"

        runHook postInstall
      '';

  preFixup = lib.optionalString stdenv.hostPlatform.isLinux ''
    addAutoPatchelfSearchPath "$out/libexec/fterm/lib"
  '';

  desktopItems = lib.optionals stdenv.hostPlatform.isLinux [
    (makeDesktopItem {
      name = "fterm";
      desktopName = "fterm";
      genericName = "Terminal Multiplexer";
      comment = "Flutter native desktop terminal multiplexer for termui";
      exec = finalAttrs.meta.mainProgram;
      icon = "fterm";
      startupWMClass = "org.mcdole.fterm";
      terminal = false;
      categories = [
        "System"
        "TerminalEmulator"
      ];
      keywords = [
        "terminal"
        "multiplexer"
        "pty"
        "termui"
      ];
    })
  ];

  passthru.updateScript = lib.getExe (writeShellApplication {
    name = "update-fterm";
    runtimeInputs = [
      curl
      git
      gnused
      jq
      nix
    ];
    text = ''
      REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
      TARGET_FILE="$REPO_ROOT/packages/fterm.nix"

      curl_args=(-fsSL)
      if [ -n "''${GITHUB_TOKEN:-}" ]; then
        curl_args+=(-H "Authorization: Bearer $GITHUB_TOKEN")
      fi

      latest_tag=$(curl "''${curl_args[@]}" \
        "https://api.github.com/repos/jtmcdole/termui/releases?per_page=100" \
        | jq -r '[.[] | select(.draft == false and .prerelease == false and (.tag_name | startswith("fterm-v")))][0].tag_name // empty')
      latest_version="''${latest_tag#fterm-v}"

      if [ -n "$latest_version" ] && [ "$latest_version" != "${version}" ]; then
        linux_hash=$(nix store prefetch-file --json \
          "https://github.com/jtmcdole/termui/releases/download/fterm-v$latest_version/fterm-linux-x64.tar.gz" \
          | jq -r .hash)
        darwin_hash=$(nix store prefetch-file --json \
          "https://github.com/jtmcdole/termui/releases/download/fterm-v$latest_version/fterm-macos-arm64.tar.gz" \
          | jq -r .hash)

        sed -i "s|^  version = \"[^\"]*\";|  version = \"$latest_version\";|" "$TARGET_FILE"
        sed -i "/fterm-linux-x64\.tar\.gz/{n;s|hash = \"[^\"]*\";|hash = \"$linux_hash\";|}" "$TARGET_FILE"
        sed -i "/fterm-macos-arm64\.tar\.gz/{n;s|hash = \"[^\"]*\";|hash = \"$darwin_hash\";|}" "$TARGET_FILE"
      fi
    '';
  });

  meta = {
    description = "Flutter native desktop terminal multiplexer for termui";
    homepage = "https://github.com/jtmcdole/termui";
    changelog = "https://github.com/jtmcdole/termui/releases/tag/fterm-v${finalAttrs.version}";
    license = lib.licenses.mit;
    mainProgram = "fterm";
    platforms = builtins.attrNames sources;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
})
