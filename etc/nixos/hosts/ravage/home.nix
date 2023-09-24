{ pkgs, ... }:

{
  imports = [
    ../../home/git.nix
    ../../home/hyprland
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
    intel-gpu-tools
    stow
  ];

  programs = {
    home-manager.enable = true;
  };
}

