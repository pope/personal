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
        nixd
        nixfmt
        nvd
        ouch # compress/decompress
        parallel
        procs # ps
        ripgrep
        sd # sed
        timg
        tldr
      ]
      ++ lib.optionals stdenv.hostPlatform.isLinux [
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
    };

    xdg.configFile = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      "glow/glow.yml".text = glowConfig;
    };
    home.file = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      "/Library/Preferences/glow/glow.yml".text = glowConfig;
    };
  };
}
