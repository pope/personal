{ config, lib, inputs, ... }:

let
  cfg = config.my.home.theme;
in
{
  options.my.home.theme = {
    colorScheme = lib.mkOption {
      type = lib.types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which theme to use with UI elements.
      '';
    };
    colors = lib.mkOption {
      type = lib.types.attrs;
    };
  };

  # Using `mkIf` here since the usage is predicated on a lazy-evaluated
  # colorScheme option. So while the `colorScheme` should never be null (not
  # allowed as a type), this check works for lazy eval.
  config = lib.mkIf (cfg.colorScheme != null) (
    let
      colorScheme =
        if cfg.colorScheme == "rose-pine" then
          inputs.nix-colors.colorSchemes.rose-pine
        else if cfg.colorScheme == "catppuccin" then
          inputs.nix-colors.colorSchemes.catppuccin-mocha
        else if cfg.colorScheme == "dracula" then
          inputs.nix-colors.colorSchemes.dracula
        else if cfg.colorScheme == "tokyonight" then
          inputs.nix-colors.colorSchemes.tokyo-night-storm
        else abort "colorScheme is invalid";
      inherit (colorScheme) palette;
      colors = palette // {
        withHash = builtins.mapAttrs (_k: v: "#${v}") palette;
        withHex = builtins.mapAttrs (_k: v: "0x${v}") palette;
      };
    in
    {
      my.home.theme.colors = colors;

      # A helper file to view the colors to names. Borrowed from stylix.
      xdg.configFile = {
        "nix-theme-colors.html".text = import ./colors.html.nix { inherit colors; };
      };
    }
  );
}
