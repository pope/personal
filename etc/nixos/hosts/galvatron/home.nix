{ pkgs, ... } @ args:

let
  overlays = import ../../overlays args;
in
{
  imports = [
    ../../home
  ];

  nixpkgs.overlays = with overlays; [
    ctpv
  ];

  home = {
    username = "pope";
    homeDirectory = "/Users/pope";

    packages = with pkgs; [
      clang-tools
      cmake-language-server
      gh
      hey
      jpeg-archive
      lua-language-server
      marksman
      nil
      tut
      vale
    ];

    stateVersion = "23.05";
  };

  programs = {
    home-manager.enable = true;
  };

  my.home = {
    audio.enable = true;
    # TODO(pope) : Enable nvim here.
    languages = {
      javascript.enable = true;
      python.enable = true;
    };
    lf.enable = true;
    packages.enable = true;
  };
}
