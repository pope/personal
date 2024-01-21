{ inputs, pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.hyprland;
in
{
  options.my.nixos.hyprland = {
    enable = mkEnableOption "hyprland system options";
  };

  config = mkIf cfg.enable {
    services = {
      dbus = {
        enable = true;
        packages = with pkgs; [ gcr dconf ];
      };

      geoclue2.enable = true;

      gnome.gnome-keyring.enable = true;

      gvfs.enable = true;

      power-profiles-daemon.enable = lib.mkDefault true;

      tumbler.enable = true;

      upower.enable = true;

      udev = {
        packages = with pkgs; [
          gnome.gnome-settings-daemon
        ];
      };

      udisks2.enable = true;

      xserver = {
        enable = true;

        desktopManager = {
          xterm.enable = false;
        };
      };
    };

    security = {
      pam.services = {
        greetd.enableGnomeKeyring = true;
        swaylock = { };
      };
      polkit.enable = true;
    };

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    xdg = {
      portal = {
        enable = true;
        wlr.enable = true;
      };
    };

    programs = {
      hyprland = {
        enable = true;
        package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      };

      light.enable = true;

      thunar = {
        enable = true;
        plugins = with pkgs.xfce; [
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
        ];
      };
    };

    environment = {
      systemPackages = with pkgs; [
        xfce.ristretto
        xfce.thunar
      ];
    };
  };
}
