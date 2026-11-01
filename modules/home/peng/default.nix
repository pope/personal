{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.peng;
  mimetypes = {
    "application/vnd.comicbook+zip" = "peng.desktop";
    "application/x-cbz" = "peng.desktop";
    "image/avif" = "peng.desktop";
    "image/bmp" = "peng.desktop";
    "image/gif" = "peng.desktop";
    "image/jpeg" = "peng.desktop";
    "image/jpg" = "peng.desktop";
    "image/png" = "peng.desktop";
    "image/svg+xml" = "peng.desktop";
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
  };
in
{
  options.my.home.peng = {
    enable = lib.mkEnableOption "Peng image viewer";
    service.enable = lib.mkOption {
      default = pkgs.stdenv.hostPlatform.isLinux;
      example = true;
      description = "Whether to enable the Peng resident server systemd user service.";
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.service.enable || pkgs.stdenv.hostPlatform.isLinux;
        message = "my.home.peng.service.enable is only supported on Linux.";
      }
    ];

    home.packages = [
      pkgs.peng
    ];

    xdg.mimeApps = lib.mkIf config.my.home.xdg.enable {
      associations.added = mimetypes;
      defaultApplications = mimetypes;
    };

    systemd.user.services.peng = lib.mkIf cfg.service.enable {
      Unit = {
        Description = "Peng resident image viewer server";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
        ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
      };

      Install.WantedBy = [ "graphical-session.target" ];

      Service = {
        ExecStart = "${lib.getExe pkgs.peng} --server";
        ExecStop = "-${lib.getExe' pkgs.peng "peng-msg"} quit";
        Restart = "on-failure";
        RestartSec = 2;
        TimeoutStopSec = 5;
      };
    };
  };
}
