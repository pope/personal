{
  coreutils,
  findutils,
  jpeg-archive,
  parallel,
  writeShellApplication,
}:

writeShellApplication {
  name = "my-jpeg-archive";
  runtimeInputs = [
    coreutils
    findutils
    jpeg-archive
    parallel
  ];
  text = # sh
    ''
      dir=''${1:-.}
      readonly dir
      cd "$dir"

      tmpdir=$(mktemp -d)
      readonly tmpdir
      trap 'rm -rf $(printf %q "$tmpdir")' EXIT

      find . -mindepth 1 -maxdepth 1 -iregex '.*\.jpe?g$' \
        | parallel --no-notice "jpeg-recompress --quality veryhigh --no-copy {} $(printf %q "$tmpdir")/{}"
      if find "$tmpdir" -mindepth 1 -maxdepth 1 | read -r
      then
        mv "$(printf %q "$tmpdir")"/* .
      fi
    '';
}
