{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    fd
    htop
    imagemagick
    neofetch
    nil
    ripgrep
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

    fish = {
      enable = true;
      interactiveShellInit = ''
        fish_config theme choose "Rosé Pine"
      '';
      plugins = [
        { name = "tide"; inherit (pkgs.fishPlugins.tide) src; }
      ];
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
    };
  };

  xdg.configFile."fish/themes" = {
    source = "${inputs.fish-rose-pine}/themes";
    recursive = true;
  };
}
