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
      colors = cfg.colorScheme.colors // {
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
