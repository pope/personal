{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.system.bluetooth;
in
{
  options.my.system.bluetooth = {
    enable = mkEnableOption "bluetooth system options";
  };

  config = mkIf cfg.enable {
    # enable bluetooth & gui paring tools - blueman
    # or you can use cli:
    # $ bluetoothctl
    # [bluetooth] # power on
    # [bluetooth] # agent on
    # [bluetooth] # default-agent
    # [bluetooth] # scan on
    # ...put device in pairing mode and wait [hex-address] to appear here...
    # [bluetooth] # pair [hex-address]
    # [bluetooth] # connect [hex-address]
    # Bluetooth devices automatically connect with bluetoothctl as well:
    # [bluetooth] # trust [hex-address]
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
