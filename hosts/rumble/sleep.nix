{
  pkgs,
  ...
}:

{
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
      HibernateDelaySec = "4h";
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

      # https://boilingsteam.com/a-quick-fix-to-improve-the-battery-life-of-the-amd-framework-13/
      prepare-sleep = {
        description = "Disable wifi and bluetooth before suspend to reduce battery draw in sleep";
        wantedBy = [ "sleep.target" ];
        before = [ "sleep.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.util-linux}/bin/rfkill block all";
          ExecStop = "${pkgs.util-linux}/bin/rfkill unblock all";
        };
      };
    };
  };
}
