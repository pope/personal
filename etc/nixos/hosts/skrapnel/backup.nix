{ pkgs, ... }:

{
  fileSystems = {
    "/mnt/Backup" = {
      device = "/dev/disk/by-label/Cyberia";
      fsType = "ext4";
      options = [ "rw" "users" "noatime" ];
    };
  };

  systemd = {
    services.cyberia-backup = {
      description = "Cyberia rsync daily backup service";
      path = with pkgs; [ coreutils findutils rsync ];
      script = ''
        LASTBACKUP=$(find /mnt/Backup/ -maxdepth 1 -iname "Cyberia.*" -type d | sort | tail -1)
        BACKUP="/mnt/Backup/Cyberia.$(date -d today +"%Y%m%d")"

        if [ -d "$BACKUP" ]; then
          echo "$BACKUP is already backed up. Skipping."
        else
          rsync -av --exclude=lost+found/ --link-dest="$LASTBACKUP" /mnt/Cyberia/ "$BACKUP"
        fi
      '';
      after = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      wants = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      serviceConfig.Type = "oneshot";
    };

    timers.cyberia-backup = {
      description = "Cyberia rsync daily backup timer";
      wantedBy = [ "timers.target" ];
      partOf = [ "cyberia-backup.service" ];
      after = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      requires = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = "6h";
      };
    };
  };
}
