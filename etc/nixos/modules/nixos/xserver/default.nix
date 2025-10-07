{ config, lib, ... }:

let
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
    enable = lib.mkEnableOption "xserver system options";

    displayManager = lib.mkOption {
      default = "gdm";
      description = "Which display manager to use";
      example = "kde";
      type = lib.types.enum [
        "gdm"
        "sddm"
        "lightdm"
        "none"
      ];
    };

    enableAutoLogin = lib.mkEnableOption "auto login of display manager";

    dwl.enable = lib.mkEnableOption "DWL";
    hyprland.enable = lib.mkEnableOption "Hyprland";
    gnome.enable = lib.mkEnableOption "GNOME";
    kde.enable = lib.mkEnableOption "KDE";
    pantheon.enable = lib.mkEnableOption "Pantheon";
  };

  config = lib.mkIf cfg.enable {
    # Hint electron apps to use wayland. Otherwise Discord will be janky.
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    programs = lib.mkIf cfg.kde.enable {
      kdeconnect.enable = true;
      partition-manager.enable = true;
    };

    services = {
      displayManager = {
        gdm = {
          enable = cfg.displayManager == "gdm";
          wayland = true;
        };
        sddm = {
          enable = cfg.displayManager == "sddm";
          wayland.enable = true;
        };
      };
      desktopManager = {
        gnome.enable = cfg.gnome.enable;
        pantheon.enable = cfg.pantheon.enable;
        plasma6.enable = cfg.kde.enable;
      };

      xserver = {
        enable = true;

        displayManager.lightdm = {
          enable = cfg.displayManager == "lightdm";
          greeters.pantheon.enable = cfg.pantheon.enable;
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
