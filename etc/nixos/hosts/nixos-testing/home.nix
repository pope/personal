{ ... }:

{
  imports = [
    ../../home/gnome.nix
    ../../home/lf.nix
    ../../home/packages.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    file.".face".source = ../../face.png;

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };
}
