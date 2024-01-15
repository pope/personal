{ pkgs, ... } @ args:

let
  overlays = import ../../overlays args;
in
{
  imports = [
    ../../home
    ../../home/audio.nix
  ];

  nixpkgs.overlays = with overlays; [
    ctpv
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    packages = with pkgs; [
      cmake-language-server
      fd
      fzf
      imagemagick
      jq
      lua-language-server
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

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  my.home = {
    # TODO(pope) : Enable nvim here.
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
  };
}
