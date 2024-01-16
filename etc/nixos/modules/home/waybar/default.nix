{ config, pkgs, ... }:

let
  waybar_config = import ./config.nix { inherit config; };
  waybar_style = import ./style.nix { inherit config; };
in
{
  home.packages = with pkgs; [
    playerctl
    libcanberra-gtk3
    pavucontrol
    pamixer
  ];

  programs = {
    waybar = {
      enable = true;
      settings = waybar_config;
      style = waybar_style;
      systemd.enable = false;
    };
  };
}
