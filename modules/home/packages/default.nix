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
in
{
  options.my.home.packages = {
    enable = lib.mkEnableOption "Common packages/app home options";
    enableBashIntegration = lib.hm.shell.mkBashIntegrationOption { inherit config; };
    enableFishIntegration = lib.hm.shell.mkFishIntegrationOption { inherit config; };
    enableZshIntegration = lib.hm.shell.mkZshIntegrationOption { inherit config; };
  };

  imports = [
    ./fzf.nix
  ];

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        add-files-to-nix-store
        cheat
        choose # cut + awk
        curlie # curl + httpie
        dua # du
        duf # df
        dust # du
        fd # find
        ffmpeg
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
