{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.xdg;
  mimetypes = {
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "video/mp4" = "mpv.desktop";
    "video/webm" = "mpv.desktop";
    "video/x-matroska" = "mpv.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };
in
{
  options.my.home.xdg = {
    enable = lib.mkEnableOption "XDG home options";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isLinux;
        message = "my.home.xdg.enable is only supported on Linux.";
      }
    ];

    home.packages = with pkgs; [
      xdg-utils
      xdg-user-dirs
    ];

    xdg = {
      cacheHome = config.home.homeDirectory + "/.cache";

      mimeApps.enable = true;
      mimeApps.associations.added = mimetypes;
      mimeApps.defaultApplications = mimetypes;

      configFile."mimeapps.list".force = true;

      userDirs = {
        enable = true;
        createDirectories = true;
        extraConfig = {
          SCREENSHOTS = "${config.xdg.userDirs.pictures}/Screenshots";
        };
        setSessionVariables = true;
      };
    };
  };
}
