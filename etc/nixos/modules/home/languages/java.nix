{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.languages.java;
in
{
  options.my.home.languages.java = {
    enable = lib.mkEnableOption "Java language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      android-studio
      gradle
    ];

    programs = {
      java.enable = true;
    };
  };
}

