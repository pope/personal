{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    goverlay
    lutris
    mangohud
    obs-studio
    vkbasalt
  ];
}

