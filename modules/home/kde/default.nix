{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.kde;
in
{
  options.my.home.kde = {
    enable = lib.mkEnableOption "KDE home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      kdePackages.aurorae
      kdePackages.oxygen
      kdePackages.oxygen-icons
      kdePackages.oxygen-sounds
      oxygenfonts
    ];
  };
}
