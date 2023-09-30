{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    fd
    fzf
    htop
    imagemagick
    neofetch
    nil
    nvtop
    ripgrep
    tldr
    tree
  ];

  programs = {
    btop = {
      enable = true;
      settings = {
        color_theme = "TTY";
        theme_background = false;
      };
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
