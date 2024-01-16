{ inputs, ... }:

{
  imports = [
    ../../home
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

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  my.home = {
    gnome.enable = true;
    lf.enable = true;
    packages.enable = true;
    shell.enable = true;
  };
}
