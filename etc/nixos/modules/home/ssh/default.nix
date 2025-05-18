{ lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.ssh;
in
{
  options.my.home.ssh = {
    enable = mkEnableOption "SSH home options";

    opIdentityAgent = mkOption {
      type = types.str;
      default = "~/.1password/agent.sock";
      description = lib.mkDoc ''
        The location of the 1Password agent socket used for SSH.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.ssh = {
      enable = true;

      controlMaster = "auto";
      controlPersist = "5m";

      matchBlocks = {
        "*".identityAgent = cfg.opIdentityAgent;

        "*.lan *.local".forwardAgent = true;

        "shifteleven.com" = {
          addressFamily = "inet";
          user = "root";
        };
      };
    };
  };
}
