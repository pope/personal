{ config, lib, inputs, ... }:

let
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.sops;
in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  options.my.nixos.sops = {
    enable = lib.mkEnableOption "SOPS Nix options";
  };

  config = lib.mkIf cfg.enable {
    sops = {
      defaultSopsFile = ../../../secrets/default.yaml;
      defaultSopsFormat = "yaml";

      age.keyFile = "/home/${mainUser}/.config/sops/age/keys.txt";

      secrets.test-key = { };
    };
  };
}
