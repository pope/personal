{ lib, config, ... }:

let
  cfg = config.my.home.ssh;
in
{
  options.my.home.ssh = {
    enable = lib.mkEnableOption "SSH home options";

    opIdentityAgent = lib.mkOption {
      type = lib.types.str;
      default = "~/.1password/agent.sock";
      description = lib.mkDoc ''
        The location of the 1Password agent socket used for SSH.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ssh = {
      enable = true;

      matchBlocks = {
        "*" = {
          match = ''host * exec "test -z $SSH_TTY"'';
          controlMaster = "auto";
          controlPersist = "5m";
          identityAgent = cfg.opIdentityAgent;
        };

        "*.lan *.local".forwardAgent = true;

        "shifteleven.com" = {
          addressFamily = "inet";
          user = "root";
        };
      };
    };
  };
}
