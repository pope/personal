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
      nodePackages.typescript-language-server
      python311Packages.pyls-isort
      python311Packages.python-lsp-server
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
    lf.enable = true;
  };
}
