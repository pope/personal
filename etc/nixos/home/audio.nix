{ pkgs, ... }:

let
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
in
{
  home.packages = [
    convert_48khz
    pkgs.parallel
  ];
}
