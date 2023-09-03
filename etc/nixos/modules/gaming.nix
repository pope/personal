{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    goverlay
    lutris
    mangohud
    obs-studio
    vkbasalt
  ];

  hardware = {
    steam-hardware.enable = true;
  };
}

