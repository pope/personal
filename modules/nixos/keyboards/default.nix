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
    # https://codeberg.org/alexeygumirov/lofree-flow-fn-fix
    boot.extraModprobeConfig = ''
      options hid_apple fnmode=2 swap_opt_cmd=1
    '';

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
