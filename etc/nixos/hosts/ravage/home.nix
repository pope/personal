{ pkgs, ... }:

{
  imports = [
    ../../home/git.nix
    ../../home/lf
    ../../home/packages.nix
  ];

  home = {
    username = "pope";
    homeDirectory = "/home/pope";

    stateVersion = "23.05";
  };

  home.packages = with pkgs; [
    # TODO(pope): Move this to maybe a browsers file.
    firefox
    stow
  ];

  programs = {
    home-manager.enable = true;
  };
}

