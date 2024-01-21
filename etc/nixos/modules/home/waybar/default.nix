{ config, pkgs, inputs, ... }:

let
  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
  waybar_config = import ./config.nix { inherit config pkgs hyprland; };
  waybar_style = import ./style.nix { inherit config; };
in
{
  home.packages = with pkgs; [
    libcanberra-gtk3
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
