{ ... }:

{
  imports = [
    ../../home/packages.nix
  ];

  home = {
    username = "pi";
    homeDirectory = "/home/pi";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };
}
