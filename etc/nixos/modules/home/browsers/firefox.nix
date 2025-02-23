{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.browsers.firefox;
in
{
  options.my.home.browsers.firefox = {
    enable = mkEnableOption "Firefox browser home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      firefox
    ];
  };
}
