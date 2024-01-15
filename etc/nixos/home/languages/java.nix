{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.java;
in
{
  options.my.home.languages.java = {
    enable = mkEnableOption "Java language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      android-studio
      gradle
    ];

    programs = {
      java.enable = true;
    };
  };
}

