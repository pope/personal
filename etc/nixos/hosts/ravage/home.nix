{ config, pkgs, ... }:

{
  imports = [
    ../../home/git.nix
    ../../home/gtk.nix
    # ../../home/hyprland
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

  xdg = {
    cacheHome = config.home.homeDirectory + "/.cache";

    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Screenshots";
      };
    };
  };

  programs = {
    home-manager.enable = true;
  };
}

