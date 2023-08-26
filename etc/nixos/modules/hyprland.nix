{ hyprland, pkgs, ... }:

{
  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.gcr ];
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
        defaultSession = "hyprland";
        lightdm.enable = false;
        gdm = {
          enable = true;
          wayland = true;
        };
      };
    };
  };

  security = {
    pam.services.greetd.enableGnomeKeyring = true;
    polkit.enable = true;
  };

  xdg = {
    # autostart.enable = true;
    portal = {
      enable = true;
      wlr.enable = true;
      # Sets environment variable NIXOS_XDG_OPEN_USE_PORTAL to 1
      # This will make xdg-open use the portal to open programs,
      # which resolves bugs involving programs opening inside FHS envs or with unexpected env vars set from wrappers.
      # xdg-open is used by almost all programs to open a unknown file/uri
      # alacritty as an example, it use xdg-open as default, but you can also custom this behavior
      # and vscode has open like `External Uri Openers`
      xdgOpenUsePortal = false;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr  # for wlroots based compositors(hyprland/sway)
        xdg-desktop-portal-gtk  # for gtk
      ];
    };
  };

  programs = {
    dconf.enable = true;

    hyprland = {
      enable = true;
      package = hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
      enableNvidiaPatches = true;
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

    sessionVariables = {
      # If your cursor becomes invisible
      WLR_NO_HARDWARE_CURSORS = "1";
      # Hint electron apps to use wayland
      NIXOS_OZONE_WL = "1";

      # https://www.reddit.com/r/NixOS/comments/137j18j/need_guide_on_installing_hyprland/
      # CLUTTER_BACKEND = "wayland";
      # GSETTINGS_SCHEMA_DIR = "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}/glib-2.0/schemas";
      # GTK_USE_PORTAL = "1";
      # NIXOS_XDG_OPEN_USE_PORTAL = "1";
      # POLKIT_AUTH_AGENT = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      # XDG_CURRENT_DESKTOP = "Hyprland";
      # XDG_SESSION_DESKTOP = "Hyprland";

      # WLR_RENDERER = "vulkan";
      # _JAVA_AWT_WM_NONREPARENTING = "1";
      # MOZ_ENABLE_WAYLAND = "1";

      POPE_XDP = "${pkgs.xdg-desktop-portal}";
      POPE_XDP_GTK = "${pkgs.xdg-desktop-portal-gtk}";
      POPE_XDP_HYPR = "${pkgs.xdg-desktop-portal-hyprland}";
    };

    systemPackages = with pkgs; [
      # xdg-utils
      # xdg-desktop-portal
      # xdg-desktop-portal-gtk
      # xdg-desktop-portal-hyprland
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
      (waybar.overrideAttrs (oldAttrs: {
          mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
        })
      )
    ];
  };
}
