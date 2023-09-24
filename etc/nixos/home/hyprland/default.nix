{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.hyprland.homeManagerModules.default
    ./config.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    disableAutoreload = false;
    recommendedEnvironment = true;
    systemdIntegration = true;
    xwayland.enable = true;
  };

  programs = {
    waybar = {
      enable = true;
      systemd = {
        enable = false;
        target = "graphical-session.target";
      };
    };
  };

  # allow fontconfig to discover fonts and configurations installed through home.packages
  fonts.fontconfig.enable = true;

  systemd.user.sessionVariables = {
    "NIXOS_OZONE_WL" = "1"; # for any ozone-based browser & electron apps to run on wayland
    "MOZ_ENABLE_WAYLAND" = "1"; # for firefox to run on wayland
    "MOZ_WEBRENDER" = "1";

    "XDG_SESSION_TYPE" = "wayland";
    "WLR_NO_HARDWARE_CURSORS" = "1";
    "WLR_EGL_NO_MODIFIRES" = "1";
  };

  home = {
    packages = with pkgs; [
      xdg-utils
      xdg-user-dirs
    ];

    # TODO(pope): Figure out how to do a "live" symlink here.
    # What's happening is that the hyprland.conf file, when saved, doesn't
    # update automatically.
    # file = {
    #   ".config/hypr" = {
    #     source = config.lib.file.mkOutOfStoreSymlink "/home/pope/Code/hyprland";
    #   };
    # };
  };

  xdg = {
    cacheHome = config.home.homeDirectory + "/.cache";

    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Screenshots";
      };
    };
  };

  systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
}
