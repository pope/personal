{ ... }:

{
  imports = [
  ];

  home = {
    username = "pi";
    homeDirectory = "/home/pi";

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
