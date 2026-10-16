{
  coreutils,
  fd,
  opus-tools,
  parallel,
  sox,
  writeShellApplication,
}:

writeShellApplication {
  name = "convert-to-opus";
  runtimeInputs = [
    coreutils
    fd
    opus-tools
    parallel
    sox
  ];
  text = # sh
    ''
      # Transcode each file, preserve its timestamp, and only remove the
      # original if conversion succeeds.
      fd --hidden -e mp3 -e flac \
        | parallel --max-args 1 \
          'set -o pipefail; sox {} --rate 48k --type flac - | opusenc --bitrate 128 --vbr - {.}.opus && touch -r {} {.}.opus && rm {}'
    '';
}
