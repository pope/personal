{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.packages;
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
      glow
      htop
      hyperfine # benchmark util
      imagemagick
      jq
      neofetch
      procs # ps
      ripgrep
      sd # sed
      tldr
      tree
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
        colors = {
          "bg+" = "#26233a";
          "fg+" = "#e0def4";
          "hl+" = "#ebbcba";
          bg = "#191724";
          border = "#403d52";
          fg = "#908caa";
          gutter = "#191724";
          header = "#31748f";
          hl = "#ebbcba";
          info = "#9ccfd8";
          marker = "#eb6f92";
          pointer = "#c4a7e7";
          prompt = "#908caa";
          separator = "#403d52";
          spinner = "#f6c177";
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
      };
    };

    xdg.configFile."glow/glow.yml".text = ''
      # style name or JSON path (default "auto")
      style: "dracula"
      # show local files only; no network (TUI-mode only)
      local: false
      # mouse support (TUI-mode only)
      mouse: true
      # use pager to display markdown
      pager: true
      # word-wrap at width
      width: 80
    '';
  };
}
