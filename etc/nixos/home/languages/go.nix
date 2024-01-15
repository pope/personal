{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.go;
in
{
  options.my.home.languages.go = {
    enable = mkEnableOption "Go language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      go
      gopls
    ];
  };
}

