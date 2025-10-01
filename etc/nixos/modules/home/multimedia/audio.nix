{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.multimedia.audio;

  an-album-cover = pkgs.writeShellApplication {
    name = "an-album-cover";
    runtimeInputs = with pkgs; [
      findutils
      imagemagick
      opustags
      parallel
    ];
    text = ''
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
  };

  convert_48khz = pkgs.writeShellApplication {
    name = "convert_48khz";
    runtimeInputs = with pkgs; [
      coreutils
      gnused
      sox
    ];
    text = ''
      infile="$1"

      outdirname="out/$(dirname "$infile")"
      outbasename="$(basename "$infile" | sed 's/\(.*\)\..*/\1/').wav"
      outfile="$outdirname/$outbasename"

      mkdir -p "$outdirname"
      sox "$infile" -r 48000 -b 16 -e signed-integer "$outfile" 
    '';
  };

  convert_to_opus = pkgs.writeShellApplication {
    name = "convert_to_opus";
    runtimeInputs = with pkgs; [
      coreutils
      fd
      opusTools
      parallel
      sox
    ];
    text = ''
      fd --hidden -e mp3 -e flac \
        | parallel --max-args 1 \
          'sox {} --rate 48k --type flac - | opusenc --bitrate 128 --vbr - {.}.opus'

      # Set the same time properties for the converted file as the original. Then
      # remove the original
      fd --hidden -e mp3 -e flac \
        | parallel --max-args 1 'touch -r {} {.}.opus && rm {}'
    '';
  };
in
{
  options.my.home.multimedia.audio = {
    enable = lib.mkEnableOption "Audio tool home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      convert_48khz
      convert_to_opus
    ] ++ lib.optionals stdenv.isLinux [
      an-album-cover
      bitwig-studio
      deadbeef-with-plugins
      easyaudiosync
      easytag
      fooyin
      lrcget
      puddletag
      qpwgraph
      reaper
      tytools-latest
      vital
    ];
  };
}
