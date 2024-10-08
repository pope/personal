{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.fonts;
in
{
  options.my.nixos.fonts = {
    enable = mkEnableOption "font system options";
  };

  config = mkIf cfg.enable {
    fonts = {
      fontDir.enable = true;
      packages = with pkgs; [
        comic-mono
        fira
        fira-go
        go-font
        hasklig
        helvetica-neue-lt-std
        inter
        iosevka
        iosevka-comfy.comfy
        jetbrains-mono
        joypixels
        maple-mono
        noto-fonts-emoji
        open-sans
        roboto
        source-serif
        terminus_font
        terminus_font_ttf
        victor-mono
        work-sans
        (nerdfonts.override {
          fonts = [
            "FiraCode"
            "NerdFontsSymbolsOnly"
            "Lilex"
          ];
        })
      ];

      enableDefaultPackages = true;

      fontconfig = {
        enable = true;

        antialias = true;
        defaultFonts = {
          emoji = [ "Joypixels" "Noto Color Emoji" ];
          monospace = [ "Iosevka" "FiraCode Nerd Font Mono" ];
          sansSerif = [ "Work Sans" "Fira Sans" "FiraGO" ];
          serif = [ "Source Serif" ];
        };
        hinting = {
          enable = true;
          autohint = false;
          style = "slight";
        };
        subpixel = {
          rgba = "rgb";
          lcdfilter = "light";
        };
      };
    };
  };
}
