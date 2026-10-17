{
  config,
  lib,
  ...
}:
let
  cfg = config.my.nixos.tailscale;
in
{
  options.my.nixos.tailscale = {
    enable = lib.mkEnableOption "tailscale";
    exitNode.enable = lib.mkEnableOption "features that allow the system to be an exit node";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.my.nixos.sops.enable;
        message = "sops must be enabled to use tailscale";
      }
    ];
    boot.kernel.sysctl = lib.mkIf cfg.exitNode.enable {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };
    services.tailscale = {
      enable = true;
      authKeyFile = config.sops.secrets.tailscale-auth-key.path;
    };
    sops.secrets.tailscale-auth-key = { };
  };
}
