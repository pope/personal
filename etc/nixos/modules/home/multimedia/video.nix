{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.multimedia.video;
in
{
  options.my.home.multimedia.video = {
    enable = lib.mkEnableOption "Video multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      davinci-resolve
    ];
  };
}
