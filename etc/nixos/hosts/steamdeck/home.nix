{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "deck";
    homeDirectory = "/home/deck";

    packages = with pkgs; [
      _1password-cli
      _1password-gui
    ];

    stateVersion = "25.11";
  };

  programs = {
    home-manager.enable = true;
  };

  # targets.genericLinux.enable = true;

  my.home = {
    editors.neovim.enable = true;
    git.enable = true;
    packages.enable = true;
    ssh.enable = true;
    yazi.enable = true;
  };
}
