{ config, lib, ... }:

let
  cfg = config.my.nixos.bluetooth;
in
{
  options.my.nixos.bluetooth = {
    enable = lib.mkEnableOption "bluetooth system options";
  };

  config = lib.mkIf cfg.enable {
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
    hardware.bluetooth = {
      enable = true;
      settings = {
        General.Enable = "Source,Sink,Media,Socket";
      };
    };
    services.blueman.enable = true;
  };
}
