{ config, lib, inputs, ... }:

let
  inherit (lib) mkOption types mkIf;
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

  # Using `mkIf` here since the usage is predicated on a lazy-evaluated
  # colorScheme option. So while the `colorScheme` should never be null (not
  # allowed as a type), this check works for lazy eval.
  config = mkIf (cfg.colorScheme != null) (
    let
      colors = cfg.colorScheme.palette // {
        withHash = builtins.mapAttrs (_k: v: "#${v}") cfg.colorScheme.palette;
        withHex = builtins.mapAttrs (_k: v: "0x${v}") cfg.colorScheme.palette;
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
