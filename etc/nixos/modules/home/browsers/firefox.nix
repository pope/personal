{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.browsers.firefox;
in
{
  options.my.home.browsers.firefox = {
    enable = lib.mkEnableOption "Firefox browser home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      firefox
    ];
  };
}
