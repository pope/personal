{ config, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.enableAutoLogin) {
    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = mainUser;
    };

    systemd = {
      services = {
        # https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
        "getty@tty1".enable = false;
        "autovt@tty1".enable = false;
      };
    };
  };
}

