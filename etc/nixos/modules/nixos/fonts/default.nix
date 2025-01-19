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
        geist-font
        go-font
        hasklig
        helvetica-neue-lt-std
        ia-writer
        inter
        iosevka
        iosevka-comfy.comfy
        jetbrains-mono
        joypixels
        maple-mono
        nerd-fonts.fira-code
        nerd-fonts.jetbrains-mono
        nerd-fonts.lilex
        nerd-fonts.symbols-only
        noto-fonts-emoji
        open-sans
        roboto
        sf-mono-nf-liga
        sf-mono-font
        source-serif
        terminus_font
        terminus_font_ttf
        victor-mono
        work-sans
      ];

      enableDefaultPackages = true;

      fontconfig = {
        enable = true;

        antialias = true;
        defaultFonts = {
          emoji = [ "Joypixels" "Noto Color Emoji" ];
          monospace = [ "Liga SFMono Nerd Font" "FiraCode Nerd Font Mono" ];
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
