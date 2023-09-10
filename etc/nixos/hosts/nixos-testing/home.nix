{ ... }:

{
  imports = [
    ../../home/gnome.nix
    ../../home/lf
    ../../home/packages.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    btop = {
      enable = true;
      settings = {
        color_theme = "TTY";
        theme_background = false;
      };
    };

    home-manager.enable = true;
  };
}
