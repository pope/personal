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
      lua-language-server
      nil
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    # TODO(pope) : Enable nvim here.
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    packages.enable = true;
  };
}
