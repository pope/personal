{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.nixos.fonts;
in
{
  options.my.nixos.fonts = {
    enable = mkEnableOption "font system options";
    resolution = mkOption {
      default = "default";
      description = lib.mkDoc ''
        Specifies the type of monitor resolution.

        With smaller monitor resolutions, font hinting values will be increased
        for a crisper font. For larger resolutions, the hinting can be turned
        off completely.
      '';
      example = "low";
      type = types.enum [ "default" "low" "high" ];
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      fontDir.enable = true;

      packages = with pkgs; let
        maple-mono-font =
          if cfg.resolution == "high" then maple-mono.NF-unhinted
          else maple-mono.NF;
      in
      [
        comic-mono
        fragment-mono
        geist-font
        go-font
        hasklig
        helvetica-neue-lt-std
        ia-writer
        inter
        iosevka-comfy.comfy
        joypixels
        maple-mono-font
        nerd-fonts.fira-code
        nerd-fonts.jetbrains-mono
        nerd-fonts.lilex
        nerd-fonts.symbols-only
        open-sans
        roboto
        roboto-mono
        roboto-slab
        sf-mono-nf-liga
        source-serif
        terminus_font
        victor-mono
        work-sans
      ];

      enableDefaultPackages = true;

      fontconfig = {
        enable = true;

        antialias = true;
        defaultFonts = {
          emoji = [ "Joypixels" "Noto Color Emoji" ];
          monospace = [ "Liga SFMono Nerd Font" ];
          sansSerif = [ "Work Sans" ];
          serif = [ "Source Serif" ];
        };
        hinting = {
          enable = cfg.resolution != "high";
          autohint = false;
          style = if cfg.resolution == "low" then "full" else "slight";
        };
        subpixel = {
          rgba = "rgb";
          lcdfilter = "default";
        };
      };
    };

    environment.sessionVariables = {
      # https://www.reddit.com/r/linux_gaming/comments/16lwgnj/comment/k1536zb/?utm_source=reddit&utm_medium=web2x&context=3
      FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
      # https://reddit.com/r/kde/comments/1bjgajv/fractional_scaling_still_seems_to_look_worse_than/kvshkoz/?context=3
      QT_SCALE_FACTOR_ROUNDING_POLICY = "RoundPreferFloor";
    };
  };
}
