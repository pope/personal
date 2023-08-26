{ config, pkgs, hyprland, ...}:

{
  imports = [
    hyprland.homeManagerModules.default
    ./config.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    enableNvidiaPatches = true;
    recommendedEnvironment = true;
    xwayland.enable = true;
    disableAutoreload = false;
    systemdIntegration = true;
  };

  # allow fontconfig to discover fonts and configurations installed through home.packages
  fonts.fontconfig.enable = true;

  systemd.user.sessionVariables = {
    "NIXOS_OZONE_WL" = "1"; # for any ozone-based browser & electron apps to run on wayland
    "MOZ_ENABLE_WAYLAND" = "1"; # for firefox to run on wayland
    "MOZ_WEBRENDER" = "1";

    # for hyprland with nvidia gpu, ref https://wiki.hyprland.org/Nvidia/
    "LIBVA_DRIVER_NAME" = "nvidia";
    "XDG_SESSION_TYPE" = "wayland";
    "GBM_BACKEND" = "nvidia-drm";
    "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
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
}
