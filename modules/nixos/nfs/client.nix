{ config, lib, ... }:

let
  cfg = config.my.nixos.nfs.client;
in
{
  options.my.nixos.nfs.client = {
    enable = lib.mkEnableOption "NFS client system options";
    host = lib.mkOption {
      default = "skrapnel.lan";
      description = ''
        Specifies the NFS server host.
      '';
      example = "skrapnel";
      type = lib.types.enum [
        "skrapnel"
        "skrapnel.lan"
      ];
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.host != "skrapnel" || config.my.nixos.tailscale.enable;
        message = "When using `skrapnel`, tailscale must be enabled";
      }
    ];

    fileSystems = {
      "/media/cyberia" = {
        device = "${cfg.host}:/mnt/Cyberia";
        fsType = "nfs";
        options = [
          "x-systemd.automount"
          "noauto"
          "x-systemd.idle-timeout=1800"
          "x-systemd.mount-timeout=5s"
          "noatime"
          "softreval"
          "x-systemd.after=network-online.target"
          "x-systemd.requires=network-online.target"
        ]
        ++ lib.optionals (cfg.host == "skrapnel") [
          "x-systemd.after=tailscaled.service"
          "x-systemd.requires=tailscaled.service"
          "x-systemd.after=tailscaled-autoconnect.service"
          "x-systemd.wants=tailscaled-autoconnect.service"
        ];
      };
    };

    # Prevent repeated mount failures (e.g. while offline) from hitting systemd's
    # start limit and permanently killing media-cyberia.automount.
    # Note: if a process polls /media/cyberia in a tight loop while offline, it
    # will repeatedly spawn mount.nfs (check `journalctl -u media-cyberia.mount`).
    systemd.units."media-cyberia.mount" = {
      overrideStrategy = "asDropin";
      text = ''
        [Unit]
        StartLimitIntervalSec=0
      '';
    };
  };
}
