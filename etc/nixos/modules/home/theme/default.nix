{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:

let
  cfg = config.my.home.theme;
in
{
  options.my.home.theme = {
    colorScheme = lib.mkOption {
      type = lib.types.enum [
        "rose-pine"
        "catppuccin"
        "dracula"
        "tokyonight"
      ];
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
        else
          abort "colorScheme is invalid";
      inherit (colorScheme) palette;
      colors = palette // {
        withHash = builtins.mapAttrs (_k: v: "#${v}") palette;
        withHex = builtins.mapAttrs (_k: v: "0x${v}") palette;
      };
    in
    {
      my.home.theme.colors = colors;

      home.activation.createThemePlaceholders =
        lib.hm.dag.entryAfter [ "writeBoundary" ] # sh
          ''
            run mkdir -p $VERBOSE_ARG \
                ~/.cache/wal/ \
                ~/.config/foot/ \
                ~/.config/hypr/

            run touch -a \
                ~/.cache/wal/colors-wal.vim \
                ~/.config/foot/wallust.ini \
                ~/.config/hypr/hyprland.wallust.conf
          '';

      home.packages = with pkgs; [
        (writeShellApplication {
          name = "wal-reset";
          runtimeInputs = [ coreutils ];
          text = # sh
            ''
              echo "" > ~/.cache/wal/colors-wal.vim
              echo "" > ~/.config/foot/wallust.ini
              echo "" > ~/.config/ghostty/wallust
              echo "" > ~/.config/hypr/hyprland.wallust.conf

              rm ~/.cache/colors.json
            '';
        })
      ];

      programs = {
        ghostty.settings.config-file = lib.mkBefore [ "?wallust" ];
        foot.settings.main.include = lib.mkAfter [ "~/.config/foot/wallust.ini" ];

        wallust = {
          enable = true;
          settings = {
            backend = "wal";
            check_contrast = true;
            color_space = "lch";
            fallback_generator = "complementary";

            templates = {
              colors = {
                template = "colors.json";
                target = "~/.cache/colors.json";
              };
              foot = {
                template = "foot.ini";
                target = "~/.config/foot/wallust.ini";
              };
              ghostty = {
                template = "ghostty";
                target = "~/.config/ghostty/wallust";
              };
              hyprland = {
                template = "hyprland.conf";
                target = "~/.config/hypr/hyprland.wallust.conf";
              };
              neopywal = {
                template = "colors.vim";
                target = "~/.cache/wal/colors-wal.vim";
              };
            };
          };
        };
      };

      wayland.windowManager.hyprland = {
        sourceFirst = false;
        settings.source = "~/.config/hypr/hyprland.wallust.conf";
      };

      xdg.configFile = {
        # A helper file to view the colors to names. Borrowed from stylix.
        "nix-theme-colors.html".text = import ./colors.html.nix { inherit colors; };

        "wallust/templates".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/personal/etc/nixos/modules/home/theme/templates";
      };
    }
  );
}
