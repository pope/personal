{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.packages;
  glowConfig = lib.generators.toYAML { } {
    style = "dracula"; # style name or JSON path (default "auto")
    local = false; # show local files only; no network (TUI-mode only)
    mouse = true; # mouse support (TUI-mode only)
    width = 100;
  };
  fzfDirPreviewOpt = "--preview='${lib.getExe pkgs.eza} --tree --color=always --icons {} | head -200'";
  fzfFilePreviewOpt = "--preview='${lib.getExe pkgs.fzf-preview} {}'";
in
{
  options.my.home.packages = {
    enable = lib.mkEnableOption "Common packages/app home options";
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages =
        with pkgs;
        [
          add-files-to-nix-store
          cheat
          choose # cut + awk
          curlie # curl + httpie
          du-dust # du
          dua # du
          duf # df
          fd # find
          ffmpeg
          fzf-preview
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
        ]
        ++ lib.optionals stdenv.isLinux [
          dysk
          man-pages
          man-pages-posix
          systemctl-tui
          trash-helper
          trashy
          wiremix
        ];

      sessionVariables = {
        FZF_COMPLETION_DIR_OPTS = fzfDirPreviewOpt;
        FZF_COMPLETION_PATH_OPTS = fzfFilePreviewOpt;
        FZF_PREVIEW_IMAGE_HANDLER = "symbols";
      };
    };

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

      eza = {
        enable = true;
        colors = "auto";
        extraOptions = [
          "--group"
          "--group-directories-first"
          "--header"
        ];
        icons = "auto";
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
          "--bind='ctrl-p:toggle-preview'"
          "--bind='alt-a:select-all'"
          "--bind='alt-n:deselect-all'"
          "--bind='ctrl-f:jump'"
          "--bind='ctrl-/:change-preview-window(down|hidden|)'"
        ];
        fileWidgetCommand = config.programs.fzf.defaultCommand;
        fileWidgetOptions = [ fzfFilePreviewOpt ];
        changeDirWidgetCommand = "${lib.getExe pkgs.fd} --type directory --hidden";
        changeDirWidgetOptions = [ fzfDirPreviewOpt ];
        historyWidgetOptions = [
          ''
            --preview='echo {} | ${lib.getExe pkgs.gnused} \"s/^ *[0-9*]\+ *//\" | ${lib.getExe pkgs.bat} --language=sh --color=always --plain'
          ''
          "--preview-window=up:3:hidden:wrap"
        ];
        tmux.enableShellIntegration = config.my.home.tmux.enable;
        tmux.shellIntegrationOptions = [
          "-p"
          "75%"
        ];
      };
      zoxide.enable = true;
    }
    // lib.optionalAttrs pkgs.stdenv.isLinux {
      cava = {
        enable = true;
        settings = {
          color =
            with config.my.home.theme.colors.withHash;
            let
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
