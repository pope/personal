{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.nixos.displaycal;
in
{
  options.my.nixos.displaycal = {
    enable = lib.mkEnableOption "DisplayCAL and i1Display3 settings";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.displaycal ];

    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0765", ATTR{idProduct}=="5020", GROUP="users", MODE="0660", TAG+="uaccess"
    '';
  };
}
