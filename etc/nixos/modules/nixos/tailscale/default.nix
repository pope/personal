{ config, lib, ... }:
let
  cfg = config.my.nixos.tailscale;
in
{
  options.my.nixos.tailscale = {
    enable = lib.mkEnableOption "Whether to enable tailscale";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.my.nixos.sops.enable;
        message = "sops must be enabled to use tailscale";
      }
    ];

    services.tailscale = {
      enable = true;
      authKeyFile = config.sops.secrets.tailscale-auth-key.path;
    };
    sops.secrets.tailscale-auth-key = { };
  };
}
