{
  coreutils,
  gnused,
  sox,
  writeShellApplication,
}:

writeShellApplication {
  name = "convert-48khz";
  runtimeInputs = [
    coreutils
    gnused
    sox
  ];
  text = # sh
    ''
      infile="$1"

      outdirname="out/$(dirname "$infile")"
      outbasename="$(basename "$infile" | sed 's/\(.*\)\..*/\1/').wav"
      outfile="$outdirname/$outbasename"

      mkdir -p "$outdirname"
      sox "$infile" -r 48000 -b 16 -e signed-integer "$outfile" 
    '';
}
