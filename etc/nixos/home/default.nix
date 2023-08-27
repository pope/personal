{ ... }:

{
  imports = [
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  programs = {
    btop.enable = true;

    home-manager.enable = true;
  };
}
