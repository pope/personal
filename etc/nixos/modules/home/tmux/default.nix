{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.tmux;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.tmux = {
    enable = lib.mkEnableOption "tmux home options";
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;

      clock24 = true;
      customPaneNavigationAndResize = true;
      escapeTime = 50;
      focusEvents = true;
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
      ] ++ lib.optionals (colorScheme == "rose-pine") [
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
      ] ++ lib.optionals (colorScheme == "catppuccin") [
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
      ] ++ lib.optionals (colorScheme == "dracula") [
        {
          plugin = dracula;
          extraConfig = ''
            set -g @dracula-show-powerline true
            set -g @dracula-show-left-sep ""
            set -g @dracula-show-right-sep ""
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
        set-option -sa terminal-features ',alacritty*:RGB,foot*:RGB,xterm-*:RGB'

        set -g allow-passthrough on
        set -ga update-environment TERM
        set -ga update-environment TERM_PROGRAM

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
