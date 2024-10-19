{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.terminals.wezterm;
in
{
  options.my.home.terminals.wezterm = {
    enable = mkEnableOption "WezTerm terminal home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
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
          colorScheme =
            if cfg.colorScheme == "rose-pine"
            then "rose-pine"
            else if cfg.colorScheme == "catppuccin"
            then "catppuccin-mocha"
            else if cfg.colorScheme == "dracula"
            then "Dracula (Official)"
            else if cfg.colorScheme == "tokyonight"
            then "Tokyo Night"
            else abort "invalid colorScheme";
        in
          /* lua */ ''
          local config = wezterm.config_builder()

          config.color_scheme = '${colorScheme}'
          config.default_cursor_style = "SteadyBar"
          config.default_prog = { '${shell}', '-l'}
          -- Disabled. See https://www.reddit.com/r/archlinux/comments/18rf5t1/psa_on_hyprland_wezterm_will_not_start_anymore/
          config.enable_wayland = false

          -- Without front_end, just blocks appear. https://github.com/wez/wezterm/issues/5990
          -- So pick the a GPU and enable the WebGpu frontend.
          for _, gpu in ipairs(wezterm.gui.enumerate_gpus()) do
            if (gpu.backend == 'Vulkan' or gpu.backend == 'Metal') and gpu.device_type == 'IntegratedGpu' then
              config.webgpu_preferred_adapter = gpu
              config.front_end = 'WebGpu'
              break
            end
          end

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
