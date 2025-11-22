{
  coreutils,
  fd,
  opusTools,
  parallel,
  sox,
  writeShellApplication,
}:

writeShellApplication {
  name = "convert-to-opus";
  runtimeInputs = [
    coreutils
    fd
    opusTools
    parallel
    sox
  ];
  text = # sh
    ''
      fd --hidden -e mp3 -e flac \
        | parallel --max-args 1 \
          'sox {} --rate 48k --type flac - | opusenc --bitrate 128 --vbr - {.}.opus'

      # Set the same time properties for the converted file as the original. Then
      # remove the original
      fd --hidden -e mp3 -e flac \
        | parallel --max-args 1 'touch -r {} {.}.opus && rm {}'
    '';
}
