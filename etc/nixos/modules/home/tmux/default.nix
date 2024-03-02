{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.tmux;
in
{
  options.my.home.tmux = {
    enable = mkEnableOption "tmux home options";
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
        { plugin = fuzzback; }
        { plugin = fzf-tmux-url; }
        { plugin = tmux-fzf; }
        { plugin = yank; }
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
        { plugin = tmux-thumbs; extraConfig = "set -g @thumbs-osc52 1"; }
        { plugin = resurrect; extraConfig = "set -g @resurrect-strategy-nvim 'session'"; }
        { plugin = continuum; extraConfig = "set -g @continuum-restore 'on'"; }
      ];
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

