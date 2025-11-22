{
  findutils,
  imagemagick,
  opustags,
  parallel,
  writeShellApplication,
}:

writeShellApplication {
  name = "an-album-cover";
  runtimeInputs = [
    findutils
    imagemagick
    opustags
    parallel
  ];
  text = # sh
    ''
      function processDir() {
        local cover="$1/cover.jpg"
        local files

        mapfile -t files < <(find "$1" -maxdepth 1 -name "*.opus")
        if [[ -z "''${files[*]}" ]]; then
          exit 0
        fi

        if [[ -f "$cover" ]]; then
          >&2 echo "$1: cover art already found. Skipping"
          exit 0
        fi

        # Load the album art of the first audio file. We can just re-use that
        # for everything else
        >&2 echo "$1: processing"
        opustags --output-cover - "''${files[0]}" 2>/dev/null \
          | magick - \
            -resize 600x600 \
            -adaptive-sharpen 0x0.5 \
            -strip \
            -interlace none \
            "$cover" 2>/dev/null

        if [[ ! -f "$cover" ]]; then
          >&2 echo "$1: No cover art"
          exit 1
        fi

        opustags --set-cover "$cover" -i "''${files[@]}"
        >&2 echo "$1: finished"
      }
      export -f processDir

      find ~/Music -type d -print0 | parallel -0 processDir
    '';
}
