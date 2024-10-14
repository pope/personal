{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.gaming;
in
{
  options.my.nixos.gaming = {
    enable = mkEnableOption "gaming system options";
    enableSteam = mkEnableOption "whether or not to enable Steam";
  };

  config = mkIf cfg.enable {
    # 8BitDo support
    # Thank you https://gist.github.com/interdependence/28452fbfbe692986934fbe1e54c920d4

    # Udev rules to start or stop systemd service when controller is connected or disconnected
    services.udev.extraRules = ''
      # May vary depending on your controller model, find product id using 'lsusb'
      SUBSYSTEM=="usb", ATTR{idVendor}=="2dc8", ATTR{idProduct}=="3106", ATTR{manufacturer}=="8BitDo", RUN+="${pkgs.systemd}/bin/systemctl start 8bitdo-ultimate-xinput@2dc8:3106"

      # This device is connected when the above device disconnects
      SUBSYSTEM=="usb", ATTR{idVendor}=="2dc8", ATTR{idProduct}=="3019", ATTR{manufacturer}=="8BitDo", RUN+="${pkgs.systemd}/bin/systemctl stop 8bitdo-ultimate-xinput@2dc8:3106"
    '';

    # Systemd service which starts xboxdrv in xbox360 mode
    systemd.services."8bitdo-ultimate-xinput@" = {
      unitConfig.Description = "8BitDo Ultimate Controller XInput mode xboxdrv daemon";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.xboxdrv}/bin/xboxdrv --mimic-xpad --silent --type xbox360 --device-by-id %I --force-feedback";
      };
    };

    # Hardware support from Steam
    hardware = {
      steam-hardware.enable = true;
      xone.enable = true;
    };

    programs = {
      gamescope = {
        enable = true;
        capSysNice = true;
      };

      steam = {
        enable = cfg.enableSteam;
        dedicatedServer.openFirewall = true;
        gamescopeSession.enable = true;
        localNetworkGameTransfers.openFirewall = true;
        remotePlay.openFirewall = true;
      };
    };
  };
}
