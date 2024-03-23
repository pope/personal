{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.terminals.wezterm;
in
{
  options.my.home.terminals.wezterm = {
    enable = mkEnableOption "WezTerm terminal home options";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.iosevka ];

    programs.wezterm = {
      enable = true;
      extraConfig =
        let
          opacity = if pkgs.stdenv.isDarwin then 0.94 else 0.85;
          line_height = if pkgs.stdenv.isDarwin then 1.6 else 1.5;
          shell =
            if config.my.home.shell.zsh.enable
            then "${pkgs.zsh}/bin/zsh"
            else "${pkgs.fish}/bin/fish";
          # Add lua syntax highlighting via TreeSitter
          lua = txt: txt;
        in
        lua ''
          local config = wezterm.config_builder()

          -- config.color_scheme = 'Rosé Pine (base16)'
          -- config.color_scheme = 'Rosé Pine (Gogh)'
          config.color_scheme = 'rose-pine'
          config.default_cursor_style = "SteadyBar"
          config.default_prog = { '${shell}', '-l'}
          config.enable_wayland = true
          config.font = wezterm.font_with_fallback {
            { family = 'Iosevka' },
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
