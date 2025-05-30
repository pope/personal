{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.packages;
  glowConfig = lib.generators.toYAML { } {
    style = "dracula"; # style name or JSON path (default "auto")
    local = false; # show local files only; no network (TUI-mode only)
    mouse = true; # mouse support (TUI-mode only)
    width = 100;
  };
  home-manager-diff = pkgs.writeShellApplication {
    name = "home-manager-diff";
    runtimeInputs = with pkgs; [
      coreutils
      fzf
      gnugrep
      home-manager
      nvd
    ];
    text = /* sh */ ''
      # A script to select two home-manager generations and find the differences
      # between them.

      GEN_CUR=$(home-manager generations \
        | fzf --border --border-label "Select current generation" \
        | cut -d' ' -f7)
      GEN_PREV=$(home-manager generations \
        | grep -v "$GEN_CUR" \
        | fzf --border --border-label "Select previous generation" \
        | cut -d' ' -f7)

      nvd diff "$GEN_PREV" "$GEN_CUR"
    '';
  };
  trash-helper = pkgs.writeShellApplication {
    name = "trash-helper";
    runtimeInputs = with pkgs; [
      coreutils
      findutils
      fzf
      gawk
      trashy
      util-linux
    ];
    text = /* sh */ ''
      ACTION=$(printf "empty\nrestore" | fzf \
          --border --border-label="Trashy Helper" \
          --header="Which action do you want to take?" \
          --list-border --list-label="Actions")
      trash list \
        | fzf --multi \
            --border --border-label="Trashy Helper" \
            --header="Select trash to $ACTION" \
            --list-border --list-label="Paths" \
        | awk '{$1=$1;print}' \
        | rev \
        | cut -d ' ' -f1 \
        | rev \
        | xargs trash "$ACTION" --match=exact --force
    '';
  };
in
{
  options.my.home.packages = {
    enable = lib.mkEnableOption "Common packages/app home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cheat
      choose # cut + awk
      curlie # curl + httpie
      du-dust # du
      dua # du
      duf # df 
      eza # ls and tree
      fd # find
      ffmpeg_6
      glow
      home-manager-diff
      htop
      hyperfine # benchmark util
      imagemagick
      jq
      nh
      nix-output-monitor
      nvd
      parallel
      procs # ps
      ripgrep
      sd # sed
      timg
      tldr
      tree
    ] ++ lib.optionals stdenv.isLinux [
      dysk
      man-pages
      man-pages-posix
      systemctl-tui
      trash-helper
      trashy
    ];

    programs = {
      bat = {
        enable = true;
        config = {
          theme = "base16";
        };
      };

      btop = {
        enable = true;
        settings = {
          color_theme = "TTY";
          theme_background = false;
        };
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fzf = {
        enable = true;
        colors = with config.my.home.theme.colors.withHash; {
          "bg+" = base02;
          "fg+" = base05;
          "hl+" = base0A;
          bg = base00;
          border = base07;
          fg = base04;
          gutter = base00;
          header = base0B;
          hl = base08;
          info = base0C;
          marker = base0E;
          pointer = base0D;
          prompt = base04;
          separator = base07;
          spinner = base0E;
        };
        defaultCommand = "${lib.getExe pkgs.fd} --type file --hidden";
        defaultOptions = [
          "--height='80%'"
          "--marker='* '"
          "--pointer='▶'"
          "--preview-window='right:60%'"
          "--bind='ctrl-p:toggle-preview'"
          "--bind='alt-a:select-all'"
          "--bind='alt-n:deselect-all'"
          "--bind='ctrl-f:jump'"
          "--bind='ctrl-/:change-preview-window(down|hidden|)'"
        ];
        fileWidgetCommand = "${lib.getExe pkgs.fd} --type file --hidden";
        fileWidgetOptions = [
          "--preview '${lib.getExe pkgs.bat} --number --color=always --line-range=:200 {}'"
        ];
        changeDirWidgetCommand = "${lib.getExe pkgs.fd} --hidden --type d";
        changeDirWidgetOptions = [
          "--preview '${lib.getExe pkgs.eza} --tree --color=always --icons {} | head -200'"
        ];
        historyWidgetOptions = [
          ''
            --preview 'echo {} | ${lib.getExe pkgs.gnused} \"s/^ *[0-9*]\+ *//\" | ${lib.getExe pkgs.bat} --language=sh --color=always --plain'
          ''
          "--preview-window up:3:hidden:wrap"
        ];
        tmux.enableShellIntegration = config.my.home.tmux.enable;
        tmux.shellIntegrationOptions = [
          "-p"
          "75%"
        ];
      };
      zoxide.enable = true;
    } // lib.optionalAttrs pkgs.stdenv.isLinux {
      cava = {
        enable = true;
        settings = {
          color = with config.my.home.theme.colors.withHash; let
            toStr = color: "\"${color}\"";
          in
          {
            background = "default";
            foreground = "default";
            gradient = 1;
            gradient_count = 6;
            gradient_color_1 = toStr base0B;
            gradient_color_2 = toStr base0C;
            gradient_color_3 = toStr base0D;
            gradient_color_4 = toStr base08;
            gradient_color_5 = toStr base0E;
            gradient_color_6 = toStr base0A;
          };
          general = {
            framerate = 60;
            lower_cutoff_freq = 20;
            higher_cutoff_freq = 20000;
          };
          input = {
            method = "pipewire";
            source = "auto";
          };
          output = {
            method = "noncurses";
            channels = "mono";
            mono_option = "average";
          };
        };
      };
    };

    xdg.configFile = lib.optionalAttrs pkgs.stdenv.isLinux {
      "glow/glow.yml".text = glowConfig;
    };
    home.file = lib.optionalAttrs pkgs.stdenv.isDarwin {
      "/Library/Preferences/glow/glow.yml".text = glowConfig;
    };
  };
}
