{ pkgs, ... }:

{
  imports = [
    ../../modules/home
  ];

  home = {
    username = "deck";
    homeDirectory = "/home/deck";

    packages = with pkgs; [
      _1password
      _1password-gui
    ];

    stateVersion = "23.11";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    editors.neovim.enable = true;
    git = {
      enable = true;
    };
    languages = {
      c.enable = true;
    };
    packages.enable = true;
    yazi.enable = true;
  };
}
