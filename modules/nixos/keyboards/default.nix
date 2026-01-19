{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.nixos.keyboards;
in
{
  options.my.nixos.keyboards = {
    enable = lib.mkEnableOption "keyboard configuration options";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      keymapp
      qmk
      via
      vial
    ];

    hardware.keyboard = {
      qmk.enable = true;
      zsa.enable = true;
    };

    services.udev.packages = with pkgs; [ via ];
  };
}
