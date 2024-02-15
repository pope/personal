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
    programs.wezterm = {
      enable = true;
      extraConfig = ''
        local wezterm = require 'wezterm'

        local config = wezterm.config_builder()

        config.color_scheme = 'Rosé Pine (base16)'
        --config.color_scheme = 'rose-pine'
        config.default_prog = { '${pkgs.fish}/bin/fish', '-l'}
        config.enable_wayland = true
        config.font = wezterm.font('Iosevka')
        config.font_size = 11.0
        config.hide_tab_bar_if_only_one_tab = true
        config.line_height = 1.25
        config.use_resize_increments = true
        config.window_background_opacity = 0.85

        return config
      '';
    };
  };
}
