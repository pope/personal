{ lib, ... }:

{
  options.my.home.kde = {
    enable = lib.mkEnableOption "KDE home options";
  };
}
