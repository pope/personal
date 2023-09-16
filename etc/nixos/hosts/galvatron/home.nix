{ ... }:

{
  imports = [
    ../../home/lf
    ../../home/packages.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };
}
