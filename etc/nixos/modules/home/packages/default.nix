{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption optionalAttrs;
  cfg = config.my.home.packages;
  glowConfig = lib.generators.toYAML { } {
    # style name or JSON path (default "auto")
    style = "dracula";
    # show local files only; no network (TUI-mode only)
    local = false;
    # mouse support (TUI-mode only)
    mouse = true;
    # use pager to display markdown
    pager = true;
    # word-wrap at width
    width = 100;
  };
in
{
  options.my.home.packages = {
    enable = mkEnableOption "Common packages/app home options";
  };

  config = mkIf cfg.enable {
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
      htop
      hyperfine # benchmark util
      imagemagick
      jq
      nh
      nvd
      procs # ps
      ripgrep
      sd # sed
      tldr
      tree
      yazi
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
        defaultCommand = "${pkgs.fd}/bin/fd --type file --hidden";
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
        fileWidgetCommand = "${pkgs.fd}/bin/fd --type file --hidden";
        fileWidgetOptions = [
          "--preview '${pkgs.bat}/bin/bat --number --color=always --line-range=:200 {}'"
        ];
        changeDirWidgetCommand = "${pkgs.fd}/bin/fd --hidden --type d";
        changeDirWidgetOptions = [
          "--preview '${pkgs.eza}/bin/eza --tree --color=always --icons {} | head -200'"
        ];
        # TODO(pope): Why did I set the language to Fish here?
        historyWidgetOptions = [
          "--preview 'echo {} | ${pkgs.bat}/bin/bat --language Fish --color=always --plain'"
          "--preview-window up:3:hidden:wrap"
        ];
        tmux.enableShellIntegration = config.my.home.tmux.enable;
        tmux.shellIntegrationOptions = [
          "-p"
          "75%"
        ];
      };
      zoxide.enable = true;
    } // optionalAttrs pkgs.stdenv.isLinux {
      cava = {
        enable = true;
        settings.color = with config.my.home.theme.colors.withHash; let
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
      };
    };

    xdg.configFile = optionalAttrs pkgs.stdenv.isLinux {
      "glow/glow.yml".text = glowConfig;
    };
    home.file = optionalAttrs pkgs.stdenv.isDarwin {
      "/Library/Preferences/glow/glow.yml".text = glowConfig;
    };
  };
}
