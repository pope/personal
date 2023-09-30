{ pkgs, ... }:

let
  overlays = import ../../overlays;
in
{
  imports = [
    ../../home/lf
  ];

  nixpkgs.overlays = with overlays; [
    ctpv
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    packages = with pkgs; [
      fd
      fzf
      imagemagick
      neofetch
      nil
      ripgrep
      tldr
      tree
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;

    btop = {
      enable = true;
      settings = {
        color_theme = "TTY";
        theme_background = false;
      };
    };
  };
}
