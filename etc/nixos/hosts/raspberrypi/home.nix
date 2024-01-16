{ inputs, ... }:

{
  imports = [
    ../../home
  ];

  home = {
    username = "pi";
    homeDirectory = "/home/pi";

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };

  colorScheme = inputs.nix-colors.colorSchemes.rose-pine;

  my.home = {
    packages.enable = true;
    shell.enable = true;
  };
}
