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

      settings = {
        "*" = {
          header = ''Match host * exec "test -z $SSH_TTY"'';
          AddKeysToAgent = "no";
          Compression = false;
          ControlMaster = "auto";
          ControlPath = "~/.ssh/master-%r@%n:%p";
          # TODO(pope): Figure out why controlPersist hangs my SSH connections
          # controlPersist = "5m";
          ForwardAgent = false;
          HashKnownHosts = false;
          IdentityAgent = cfg.opIdentityAgent;
          ServerAliveCountMax = 3;
          ServerAliveInterval = 0;
          UserKnownHostsFile = "~/.ssh/known_hosts";
        };

        "*.lan *.local".ForwardAgent = true;

        "shifteleven.com" = {
          AddressFamily = "inet";
          User = "root";
        };
      };
    };
  };
}
