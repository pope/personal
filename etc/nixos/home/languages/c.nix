{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.c;
in
{
  options.my.home.languages.c = {
    enable = mkEnableOption "C language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cmake
      cmake-language-server
      gcc
      gnumake
      ninja
      stdenv
    ];
  };
}
