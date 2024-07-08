{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.xdg;
  mimetypes = {
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "image/gif" = "imv.desktop";
    "image/jpeg" = "imv.desktop";
    "image/jpg" = "imv.desktop";
    "image/png" = "imv.desktop";
    "image/webp" = "imv.desktop";
    "text/html" = "firefox.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };
in
{
  options.my.home.xdg = {
    enable = mkEnableOption "XDG home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      xdg-utils
      xdg-user-dirs
    ];

    xdg = {
      cacheHome = config.home.homeDirectory + "/.cache";

      mimeApps.enable = true;
      mimeApps.associations.added = mimetypes;
      mimeApps.defaultApplications = mimetypes;

      userDirs = {
        enable = true;
        createDirectories = true;
        extraConfig = {
          XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Screenshots";
        };
      };
    };
  };
}
