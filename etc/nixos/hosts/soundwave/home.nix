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
    home-manager.enable = true;
  };
}
