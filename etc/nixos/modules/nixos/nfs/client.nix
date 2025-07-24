{ config, lib, ... }:

let
  cfg = config.my.nixos.nfs.client;
in
{
  options.my.nixos.nfs.client = {
    enable = lib.mkEnableOption "NFS client system options";
    host = lib.mkOption {
      default = "skrapnel.lan";
      description = lib.mkDoc ''
        Specifies the NFS server host.
      '';
      example = "skrapnel";
      type = lib.types.enum [ "skrapnel" "skrapnel.lan" ];
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
          "x-systemd.after=network-online.target"
          "x-systemd.idle-timeout=300"
        ];
      };
    };
  };
}
