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
      Usage: my-photo-import <source> <destination>
             my-photo-import -h|--help

      Import and organize photos using exiftool based on camera make/model and date.

      Arguments:
        <source>       Source directory containing photos to import
        <destination>  Destination directory where organized photos will be saved

      Options:
        -h, --help     Show this help message and exit
      EOF
      }

      for arg in "$@"; do
        case "$arg" in
          -h|--help)
            show_help
            exit 0
            ;;
        esac
      done

      if [ "$#" -ne 2 ]; then
        echo "Error: Exactly 2 arguments required, got $#." >&2
        echo >&2
        show_help >&2
        exit 1
      fi

      exiftool \
          "-filename=$2/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/1979-12-31/1979-12-31-%f.%e" \
          "-filename<$2/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/\''${FileModifyDate}/\''${FileModifyDate}-%f.%e" \
          "-filename<$2/UNKNOWN_CAMERA_MFG-UNKNOWN_CAMERA_MODEL/\''${CreateDate}/\''${CreateDate}-%f.%e" \
          "-filename<$2/\''${Make}-\''${Model}/\''${CreateDate}/\''${CreateDate}-%f.%e" \
          "-filename<$2/\''${Make}-\''${Model}/\''${DateTimeOriginal}/\''${DateTimeOriginal}-%f.%e" \
          -d '%Y-%m-%d' \
          -o dummy \
          -r "$1"
    '';
}
