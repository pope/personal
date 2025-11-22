{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.terminals.wezterm;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.terminals.wezterm = {
    enable = lib.mkEnableOption "WezTerm terminal home options";
    installExtraFonts = lib.mkEnableOption "the installation of extra fonts used by the config";
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.mkIf cfg.installExtraFonts (
      with pkgs;
      [
        iosevka
        joypixels
        nerd-fonts.symbols-only
        noto-fonts-color-emoji
      ]
    );

    programs.wezterm = {
      enable = true;
      extraConfig =
        let
          opacity = 0.94;
          line_height = if pkgs.stdenv.isDarwin then 1.6 else 1.25;
          shell = if config.my.home.shell.zsh.enable then "${pkgs.zsh}/bin/zsh" else "${pkgs.fish}/bin/fish";
          cs =
            if colorScheme == "rose-pine" then
              "rose-pine"
            else if colorScheme == "catppuccin" then
              "catppuccin-mocha"
            else if colorScheme == "dracula" then
              "Dracula (Official)"
            else if colorScheme == "tokyonight" then
              "Tokyo Night"
            else
              abort "invalid colorScheme";
        in
        # lua
        ''
          local config = wezterm.config_builder()

          config.color_scheme = '${cs}'
          config.default_cursor_style = "SteadyBar"
          config.default_prog = { '${shell}', '-l'}

          config.font = wezterm.font_with_fallback {
            { family = 'monospace' },
            { family = 'Iosevka' },
            { family = 'JoyPixels' },
            { family = 'Noto Color Emoji' },
            { family = 'Symbols Nerd Font Mono' },
          }
          config.font_size = 11.5
          config.hide_tab_bar_if_only_one_tab = true
          config.initial_cols = 160
          config.initial_rows = 30
          config.line_height = ${builtins.toString line_height}
          config.macos_window_background_blur = 20
          config.use_resize_increments = true
          config.warn_about_missing_glyphs = false
          config.window_background_opacity = ${builtins.toString opacity}

          local overrides_exists, overrides = pcall(require, "overrides")
          if overrides_exists then
            overrides.config(config)
          end

          return config
        '';
    };
  };
}
