{ ... }:

{
  imports = [
    ../../home
    ../../home/gnome.nix
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

  my.home = {
    lf.enable = true;
    packages.enable = true;
    shell.enable = true;
  };
}
