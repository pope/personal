{ pkgs, ... }:

{
  fileSystems = {
    "/mnt/Backup" = {
      device = "/dev/disk/by-label/Cyberia";
      fsType = "ext4";
      options = [ "rw" "users" "noatime" ];
    };
  };

  my.nixos.idrive.enable = true;

  systemd = {
    services.cyberia-backup = {
      description = "Cyberia rsync daily backup service";
      path = with pkgs; [ coreutils findutils rsync ];
      script = ''
        LASTBACKUP=$(find /mnt/Backup/ -maxdepth 1 -iname "Cyberia.*" -type d | sort | tail -1)
        BACKUP="/mnt/Backup/Cyberia.$(date -d today +"%Y%m%d")"

        TO_CLEANUP="/mnt/Backup/Cyberia.$(date -d "7 days ago" +"%Y%m%d")"

        if [ -d "$BACKUP" ]; then
          echo "$BACKUP is already backed up. Skipping."
        else
          echo "Backing up $BACKUP from $LASTBACKUP"
          rsync -av --exclude=lost+found/ --link-dest="$LASTBACKUP" /mnt/Cyberia/ "$BACKUP"
          # If the sync succeeds, then clean up the backup 7 days ago, unless
          # that backup was from the 1st or 15th.
          if [ $? -eq 0 ] && [[ ! "$TO_CLEANUP" =~ (01|15)$ ]]; then
            echo "Removing $TO_CLEANUP from 7 days ago"
            rm -rf "$TO_CLEANUP"
          else
            echo "Keeping $TO_CLEANUP from 7 days ago"
          fi
          echo "$BACKUP done."
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
