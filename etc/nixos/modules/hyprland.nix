{ inputs, pkgs, ... }:

let
  overlays = import ../overlays;
in
{
  nixpkgs.overlays = with overlays; [
    waybar
  ];

  services = {
    dbus = {
      enable = true;
      packages = with pkgs; [ gcr dconf ];
    };

    geoclue2.enable = true;

    gnome.gnome-keyring.enable = true;

    gvfs.enable = true;

    power-profiles-daemon.enable = true;

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

      displayManager = {
        # defaultSession = "hyprland";
        lightdm.enable = true;
        # gdm = {
        #   enable = true;
        #   wayland = true;
        # };
      };
    };
  };

  security = {
    pam.services.greetd.enableGnomeKeyring = true;
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

      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr # for wlroots based compositors(hyprland/sway)
        xdg-desktop-portal-gtk # for gtk
      ];
    };
  };

  programs = {
    dconf.enable = true;

    hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };

    light.enable = true;

    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  environment = {
    pathsToLink = [ "/libexec" ];

    systemPackages = with pkgs; [
      # xdg-utils
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland

      alsa-utils
      dunst
      gnome.adwaita-icon-theme
      gnome.gnome-themes-extra
      grim
      gsettings-desktop-schemas
      # hyprland-protocols
      hyprpicker
      killall
      libnotify
      networkmanagerapplet
      pavucontrol
      polkit_gnome
      libsForQt5.qt5.qtwayland
      qt6.qtwayland
      rofi-wayland
      rose-pine-gtk-theme
      rose-pine-icon-theme
      slurp
      swww
      # swayidle
      # swaylock
      udiskie
      wayland
      wf-recorder
      wl-clipboard
      wlogout
      wlr-randr
      xfce.thunar
      waybar
    ];
  };
}
