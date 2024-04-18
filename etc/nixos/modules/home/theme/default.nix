{ config, lib, inputs, ... }:

let
  inherit (lib) mkOption types mkIf;
  cfg = config.my.home.theme;
in
{
  options.my.home.theme = {
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which theme to use with UI elements.
      '';
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
      colorScheme =
        if cfg.colorScheme == "rose-pine"
        then inputs.nix-colors.colorSchemes.rose-pine
        else inputs.nix-colors.colorSchemes.catppuccin-mocha;
      inherit (colorScheme) palette;
      colors = palette // {
        withHash = builtins.mapAttrs (_k: v: "#${v}") palette;
        withHex = builtins.mapAttrs (_k: v: "0x${v}") palette;
      };
    in
    {
      my.home.theme.colors = colors;

      my.home.editors.neovim.colorScheme = cfg.colorScheme;
      my.home.gtk.theme = cfg.colorScheme;
      my.home.shell.fish.colorScheme = cfg.colorScheme;
      my.home.terminals.kitty.colorScheme = cfg.colorScheme;
      my.home.terminals.wezterm.colorScheme = cfg.colorScheme;
      my.home.tmux.colorScheme = cfg.colorScheme;

      # A helper file to view the colors to names. Borrowed from stylix.
      xdg.configFile = {
        "nix-theme-colors.html".text = import ./colors.html.nix { inherit colors; };
      };
    }
  );
}
