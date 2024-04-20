{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption optionals types;
  cfg = config.my.home.tmux;
in
{
  options.my.home.tmux = {
    enable = mkEnableOption "tmux home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;

      clock24 = true;
      customPaneNavigationAndResize = true;
      escapeTime = 50;
      historyLimit = 30000;
      keyMode = "vi";
      mouse = true;
      newSession = true;
      prefix = "C-z";
      terminal = "tmux-256color";
      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = fuzzback;
          extraConfig = ''
            set -g @fuzzback-popup 1
            set -g @fuzzback-popup-size '90%'
          '';
        }
        { plugin = fzf-tmux-url; }
        { plugin = tmux-fzf; }
        { plugin = yank; }
        { plugin = tmux-thumbs; extraConfig = "set -g @thumbs-osc52 1"; }
      ] ++ optionals (cfg.colorScheme == "rose-pine") [
        {
          plugin = rose-pine;
          extraConfig = ''
            set -g @rose_pine_variant 'main'
            set -g @rose_pine_date_time '%Y-%m-%d %H:%M'
            set -g @rose_pine_disable_active_window_menu 'on'
            set -g @rose_pine_bar_bg_disable 'on'
            set -g @rose_pine_bar_bg_disabled_color_option 'default'
          '';
        }
      ] ++ optionals (cfg.colorScheme == "catppuccin") [
        {
          plugin = catppuccin;
          extraConfig = ''
            set -g @catppuccin_window_left_separator ""
            set -g @catppuccin_window_right_separator " "
            set -g @catppuccin_window_middle_separator " █"
            set -g @catppuccin_window_number_position "right"

            set -g @catppuccin_window_default_fill "number"
            set -g @catppuccin_window_default_text "#W"

            set -g @catppuccin_window_current_fill "number"
            set -g @catppuccin_window_current_text "#W"

            set -g @catppuccin_status_modules_right "directory user host session"
            set -g @catppuccin_status_left_separator  " "
            set -g @catppuccin_status_right_separator ""
            set -g @catppuccin_status_right_separator_inverse "no"
            set -g @catppuccin_status_fill "icon"
            set -g @catppuccin_status_connect_separator "no"

            set -g @catppuccin_directory_text "#{pane_current_path}"
          '';
        }
      ] ++ (with pkgs.tmuxPlugins; [
        # These come last because continuum updates the `status-left` &
        # `status-right` variables - and thus need to come after the themeing
        # stuff above.
        { plugin = resurrect; extraConfig = "set -g @resurrect-strategy-nvim 'session'"; }
        { plugin = continuum; extraConfig = "set -g @continuum-restore 'on'"; }
      ]);
      extraConfig = ''
        set-option -sa terminal-features ',alacritty*:RGB,foot*:RGB,xterm-kitty*:RGB,xterm-256color:RGB'
        
        set -g update-environment -r
        set -g renumber-windows on
        set -g status-position top

        bind '"' split-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
        bind R source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded..."
      '';
    };
  };
}

