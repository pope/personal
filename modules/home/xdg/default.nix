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
    "application/vnd.comicbook+zip" = "peng.desktop";
    "application/x-cbz" = "peng.desktop";
    "image/avif" = "peng.desktop";
    "image/bmp" = "peng.desktop";
    "image/gif" = "peng.desktop";
    "image/jpeg" = "peng.desktop";
    "image/jpg" = "peng.desktop";
    "image/png" = "peng.desktop";
    "image/tiff" = "peng.desktop";
    "image/webp" = "peng.desktop";
    "image/x-adobe-dng" = "peng.desktop";
    "image/x-canon-cr2" = "peng.desktop";
    "image/x-canon-cr3" = "peng.desktop";
    "image/x-canon-crw" = "peng.desktop";
    "image/x-dcraw" = "peng.desktop";
    "image/x-fuji-raf" = "peng.desktop";
    "image/x-nikon-nef" = "peng.desktop";
    "image/x-nikon-nrw" = "peng.desktop";
    "image/x-olympus-orf" = "peng.desktop";
    "image/x-panasonic-rw" = "peng.desktop";
    "image/x-panasonic-rw2" = "peng.desktop";
    "image/x-pentax-pef" = "peng.desktop";
    "image/x-samsung-srw" = "peng.desktop";
    "image/x-sigma-x3f" = "peng.desktop";
    "image/x-sony-arw" = "peng.desktop";
    "image/x-sony-sr2" = "peng.desktop";
    "image/x-sony-srf" = "peng.desktop";
    "image/x-tga" = "peng.desktop";
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
