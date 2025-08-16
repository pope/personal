{ config, lib, ... }:

let
  cfg = config.my.nixos.idrive;
in
{
  options.my.nixos.idrive = {
    enable = lib.mkEnableOption "IDrive backup options";
  };

  config = lib.mkIf cfg.enable {
    virtualisation = {
      podman.enable = true;

      oci-containers = {
        backend = "podman";

        containers.idrive = {
          image = "ghcr.io/snorre-k/idrive-docker:3.10.0";
          volumes = [
            "idrive:/opt/IDriveForLinux/idriveIt"
            "/mnt/Cyberia:/media/cyberia:ro"
          ];
          autoRemoveOnStop = false;
          autoStart = true;
          environment = {
            TZ = config.time.timeZone;
          };
        };
      };
    };
  };
}
