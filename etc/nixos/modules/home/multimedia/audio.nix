{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.audio;
  convert_48khz = pkgs.writeScriptBin "convert_48khz" ''
    set -o errexit
    set -o pipefail

    program="$0"
    infile="$1"

    outdirname="out/$(${pkgs.coreutils}/bin/dirname "$infile")"
    outbasename="$(${pkgs.coreutils}/bin/basename "$infile" | sed 's/\(.*\)\..*/\1/').wav"
    outfile="$outdirname/$outbasename"

    ${pkgs.coreutils}/bin/mkdir -p "$outdirname"
    ${pkgs.sox}/bin/sox "$infile" -r 48000 -b 16 -e signed-integer "$outfile" 
  '';

  convert_to_opus = pkgs.writeShellScriptBin "convert_to_opus" ''
    ${lib.getExe pkgs.fd} --hidden -e mp3 -e flac\
      | ${lib.getExe pkgs.parallel} --max-args 1 \
        '${pkgs.sox}/bin/sox {} --rate 48k --type flac - | ${pkgs.opusTools}/bin/opusenc --bitrate 128 --vbr - {.}.opus'

    # Set the same time properties for the converted file as the original. Then
    # remove the original
    ${lib.getExe pkgs.fd} --hidden -e mp3 -e flac \
      | ${lib.getExe pkgs.parallel} --max-args 1 \
        '${pkgs.coreutils}/bin/touch -r {} {.}.opus && ${pkgs.coreutils}/bin/rm {}'
  '';
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
      reaper
    ];
  };
}
