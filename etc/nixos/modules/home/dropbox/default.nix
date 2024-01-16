{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.dropbox;
in
{
  options.my.home.dropbox = {
    enable = mkEnableOption "Dropbox support home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      maestral
      maestral-gui
    ];
  };
}
