{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.audio;
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
    enable = mkEnableOption "Audio tool home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      convert_48khz
      convert_to_opus
    ] ++ lib.optionals stdenv.isLinux [
      bitwig-studio
      easyaudiosync
      easytag
      reaper
      vital
    ];
  };
}
