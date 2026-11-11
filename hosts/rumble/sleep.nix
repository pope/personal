{
  config,
  ...
}:

{
  boot.resumeDevice = (builtins.head config.swapDevices).device;

  # Avoid loading amdgpu in initrd so it doesn't modeset /dev/tty1 mid-LUKS
  # password entry or conflict with the hibernated kernel's amdgpu S4 restore.
  hardware.amdgpu.initrd.enable = false;

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
