{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    goverlay
    lutris
    mangohud
    vkbasalt
  ];

  hardware = {
    steam-hardware.enable = true;
  };
}

