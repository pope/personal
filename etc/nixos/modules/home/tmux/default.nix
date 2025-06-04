{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.tmux;
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
        { plugin = resurrect; extraConfig = "set -g @resurrect-strategy-nvim 'session'"; }
        { plugin = continuum; extraConfig = "set -g @continuum-restore 'on'"; }
      ];
      extraConfig = ''
        set-option -sa terminal-features ',alacritty*:RGB,foot*:RGB,xterm-*:RGB'

        set -g allow-passthrough on
        set -ga update-environment TERM
        set -ga update-environment TERM_PROGRAM

        set -g update-environment -r
        set -g renumber-windows on
        set -g status-position top

        # Theme: borders
        set -g pane-border-indicators both
        set -g pane-border-lines single
        set -g pane-border-style fg=black,bright
        set -g pane-active-border-style fg=magenta

        # Theme: status
        set -g status-style bg=default,fg=white
        set -g status-left ""
        set -g status-right "#[fg=white,bright]#S"

        # Theme: status (windows)
        set -g window-status-format " ○ #W "
        set -g window-status-current-format " ● #W "
        set -g window-status-current-style "#{?window_zoomed_flag,fg=yellow,fg=magenta,nobold}"
        set -g window-status-bell-style "fg=red,nobold"

        bind '"' split-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
        bind R source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded..."
      '';
    };
  };
}
