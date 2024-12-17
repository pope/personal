{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.nixos.xserver;
in
{
  imports = [
    ./autologin
    ./dwl
    ./hyprland
    ./tile-manager-common
  ];

  options.my.nixos.xserver = {
    enable = mkEnableOption "xserver system options";

    displayManager = mkOption {
      default = "gdm";
      description = "Which display manager to use";
      example = "kde";
      type = types.enum [ "gdm" "sddm" "lightdm" ];
    };

    enableAutoLogin = mkEnableOption "auto login of display manager";

    dwl.enable = mkEnableOption "DWL";
    hyprland.enable = mkEnableOption "Hyprland";
    gnome.enable = mkEnableOption "GNOME";
    kde.enable = mkEnableOption "KDE";
    pantheon.enable = mkEnableOption "Pantheon";
  };

  config = mkIf cfg.enable {
    services = {
      displayManager.sddm.enable = cfg.displayManager == "sddm";
      desktopManager.plasma6.enable = cfg.kde.enable;

      xserver = {
        enable = true;

        displayManager = {
          gdm = {
            enable = cfg.displayManager == "gdm";
            wayland = true;
          };
          lightdm = {
            enable = cfg.displayManager == "lightdm";
            greeters.pantheon.enable = cfg.pantheon.enable;
          };
        };

        desktopManager = {
          gnome.enable = cfg.gnome.enable;
          pantheon.enable = cfg.pantheon.enable;
        };

        # Configure keymap in X11
        xkb = {
          layout = "us";
          variant = "";
        };
      };
    };
  };
}
