{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.nixos.xserver;
in
{
  imports = [
    ./autologin
    ./gnome
    ./hyprland
    ./kde
    ./pantheon
  ];

  options.my.nixos.xserver = {
    enable = mkEnableOption "xserver system options";

    desktop = mkOption {
      default = "gnome";
      description = "The main desktop environment to use";
      example = "kde";
      type = types.enum [ "gnome" "kde" "pantheon" ];
    };

    enableAutoLogin = mkEnableOption "auto login of display manager";

    enableHyprland = mkEnableOption "hyprland";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;

      # Configure keymap in X11
      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };
}
