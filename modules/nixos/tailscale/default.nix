{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.my.nixos.tailscale;
in
{
  options.my.nixos.tailscale = {
    enable = lib.mkEnableOption "Whether to enable tailscale";
    enableAsExitNode = lib.mkEnableOption "Whether to enable features that allow the system to be an exit node";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.my.nixos.sops.enable;
        message = "sops must be enabled to use tailscale";
      }
    ];
    boot.kernel.sysctl = lib.mkIf cfg.enableAsExitNode {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };
    services.tailscale = {
      enable = true;
      authKeyFile = config.sops.secrets.tailscale-auth-key.path;
      # TODO(pope): Remove. See https://github.com/NixOS/nixpkgs/issues/438765
      package = pkgs.tailscale.overrideAttrs { doCheck = false; };
    };
    sops.secrets.tailscale-auth-key = { };
  };
}
