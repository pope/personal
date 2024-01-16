{ pkgs, ... }:

{
  home.packages = with pkgs; [
    goverlay
    lutris
    mangohud
    vkbasalt
  ];
}
