{
  coreutils,
  exiftool,
  writeShellApplication,
}:

writeShellApplication {
  name = "my-photo-import";
  runtimeInputs = [
    coreutils
    exiftool
  ];
  text = # sh
    ''
      show_help() {
        cat <<'EOF'
      Usage: my-photo-import [options] <source> <destination>
             my-photo-import -h|--help

      Import and organize photos using exiftool based on camera make/model and date.

      Arguments:
        <source>       Source directory containing photos to import
        <destination>  Destination directory where organized photos will be saved

      Options:
        -n, --dry-run  Simulate import without copying or creating files
        -h, --help     Show this help message and exit
      EOF
      }

      dry_run=0
      positional=()

      while [ "$#" -gt 0 ]; do
        case "$1" in
          -h|--help)
            show_help
            exit 0
            ;;
          -n|--dry-run)
            dry_run=1
            shift
            ;;
          -*)
            echo "Error: Unknown option $1" >&2
            show_help >&2
            exit 1
            ;;
          *)
            positional+=("$1")
            shift
            ;;
        esac
      done

      if [ "''${#positional[@]}" -ne 2 ]; then
        echo "Error: Exactly 2 arguments required, got ''${#positional[@]}." >&2
        echo >&2
        show_help >&2
        exit 1
      fi

      src="''${positional[0]}"
      dst="''${positional[1]}"

      extra_opts=("-progress")
      if [ "$dry_run" -eq 1 ]; then
        echo "=== DRY RUN (No files will be copied) ==="
        target_tag="TestName"
      else
        target_tag="FileName"
        extra_opts+=("-o" "dummy")
      fi

      exiftool \
          "-$target_tag=$dst/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/1979-12-31/1979-12-31-%f.%e" \
          "-$target_tag<$dst/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/\''${FileModifyDate}/\''${FileModifyDate}-%f.%e" \
          "-$target_tag<$dst/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/\''${CreateDate}/\''${CreateDate}-%f.%e" \
          "-$target_tag<$dst/\''${Make}-\''${Model}/\''${CreateDate}/\''${CreateDate}-%f.%e" \
          "-$target_tag<$dst/\''${Make}-\''${Model}/\''${DateTimeOriginal}/\''${DateTimeOriginal}-%f.%e" \
          -d '%Y-%m-%d' \
          "''${extra_opts[@]}" \
          -r "$src"
    '';
}
