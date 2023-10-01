{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    fd
    fzf
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
  };

  xdg.configFile."fish/themes" = {
    source = "${inputs.fish-rose-pine}/themes";
    recursive = true;
  };
}
