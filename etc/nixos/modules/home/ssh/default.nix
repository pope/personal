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
      enableDefaultConfig = false;

      matchBlocks = {
        "*" = {
          match = ''host * exec "test -z $SSH_TTY"'';
          addKeysToAgent = "no";
          compression = false;
          controlMaster = "auto";
          controlPath = "~/.ssh/master-%r@%n:%p";
          # TODO(pope): Figure out why controlPersist hangs my SSH connections
          # controlPersist = "5m";
          forwardAgent = false;
          hashKnownHosts = false;
          identityAgent = cfg.opIdentityAgent;
          serverAliveCountMax = 3;
          serverAliveInterval = 0;
          userKnownHostsFile = "~/.ssh/known_hosts";
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
