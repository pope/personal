{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.video;
in
{
  options.my.home.multimedia.video = {
    enable = mkEnableOption "Video multimedia home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      davinci-resolve
    ];
  };
}
