{
  pkgs,
  ...
}:

{
  boot.resumeDevice = "/dev/disk/by-uuid/e598815b-331d-468f-b44a-222bd4ae238a";

  # https://boilingsteam.com/a-quick-fix-to-improve-the-battery-life-of-the-amd-framework-13/
  powerManagement = {
    powerDownCommands = ''
      ${pkgs.util-linux}/bin/rfkill block all
    '';
    resumeCommands = ''
      ${pkgs.util-linux}/bin/rfkill unblock all
    '';
  };

  services = {
    logind.settings.Login = {
      HandleLidSwitch = "suspend-then-hibernate";
      HandleLidSwitchExternalPower = "suspend-then-hibernate";
    };
  };

  systemd = {
    sleep.settings.Sleep = {
      AllowHibernation = "yes";
      AllowHybridSleep = "yes";
      AllowSuspend = "yes";
      AllowSuspendThenHibernate = "yes";
      HibernateDelaySec = "2h";
      MemorySleepMode = "s2idle";
    };

    services = {
      clear-rtc-alarm = {
        description = "Clear stale RTC alarms on boot";
        wantedBy = [ "multi-user.target" ];
        serviceConfig.Type = "oneshot";
        script = ''
          echo 0 > /sys/class/rtc/rtc0/wakealarm 2>/dev/null || true
        '';
      };
    };
  };
}
