{ pkgs, ... }:

let
  previewer = pkgs.writeShellScript "lf_kitty_preview" ''
    file=$1
    w=$2
    h=$3
    x=$4
    y=$5

    if [[ "$( file -Lb --mime-type "$file")" =~ ^image ]]; then
      kitty +kitten icat \
        --silent \
        --stdin no \
        --transfer-mode file \
        --place "''${w}x''${h}@''${x}x''${y}" \
        "$file" < /dev/null > /dev/tty
      exit 1
    fi

    pistol "$file"
  '';
  cleaner = pkgs.writeShellScript "lf_kitty_clean" ''
    kitty +kitten icat \
      --clear --stdin no --silent --transfer-mode file < /dev/null > /dev/tty
  '';
in
{
  home.packages = with pkgs; [
    file
    pistol
  ];

  programs = {
    lf = {
      enable = true;
      settings = {
        previewer = "${previewer}";
        cleaner = "${cleaner}";
      };
    };
  };
}
