{ pkgs, ... }:

{
  imports = [
    ../../home/gtk.nix
    ../../home/gnome.nix
    ../../home/kitty
    ../../home/lf
    ../../home/packages.nix
    ../../home/xdg.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    packages = with pkgs; [
      firefox
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };
}
