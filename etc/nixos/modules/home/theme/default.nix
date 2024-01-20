{ config, lib, inputs, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.my.home.theme;
in
{
  options.my.home.theme = {
    colorScheme = mkOption {
      type = with types; attrs;
      default = inputs.nix-colors.colorSchemes.rose-pine;
    };
    colors = mkOption {
      type = with types; attrs;
    };
  };

  config =
    let
      colors = {
        inherit (cfg.colorScheme.colors) base00 base01 base02 base03 base04 base05 base06 base07;
        inherit (cfg.colorScheme.colors) base08 base09 base0A base0B base0C base0D base0E base0F;
        withHash = builtins.mapAttrs (_k: v: "#${v}") cfg.colorScheme.colors;
        withHex = builtins.mapAttrs (_k: v: "0x${v}") cfg.colorScheme.colors;
      };
    in
    {
      my.home.theme.colors = colors;

      # A helper file to view the colors to names. Borrowed from stylix.
      xdg.configFile = {
        "nix-theme-colors.html".text = import ./colors.html.nix { inherit colors; };
      };
    };
}
