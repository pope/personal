{ config, pkgs, lib, inputs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.waybar;

  inherit (inputs.hyprland.packages.${pkgs.system}) hyprland;
  waybar_config = import ./config.nix { inherit config pkgs hyprland; };
  waybar_style = import ./style.nix { inherit config; };
in
{
  options.my.home.waybar = {
    enable = mkEnableOption "waybar home options";
  };

  config = mkIf cfg.enable {

    home.packages = with pkgs; [
      iosevka
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
  };
}
