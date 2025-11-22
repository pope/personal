{ config, lib, ... }:

let
  cfg = config.my.home.sops;
in
{
  options.my.home.sops = {
    enable = lib.mkEnableOption "SOPS Nix options";
  };

  config = lib.mkIf cfg.enable {
    sops = {
      age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
      defaultSopsFile = ../../../secrets/home.yaml;
      defaultSopsFormat = "yaml";
    };
  };
}
