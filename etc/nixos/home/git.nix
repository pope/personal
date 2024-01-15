{ pkgs, lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.git;
in
{
  options.my.home.git = {
    enable = mkEnableOption "Git home options";

    opSshSignCommand = mkOption {
      type = types.str;
      default = "${pkgs._1password-gui}/bin/op-ssh-sign";
      description = lib.mdDoc ''
        The location of the 1Password SSH sign app used for signing Git
        commits.
      '';
    };

    opIdentityAgent = mkOption {
      type = types.str;
      default = "~/.1password/agent.sock";
      description = lib.mkDoc ''
        The location of the 1Password agent socket used for SSH.
      '';
    };

    sshCommand = mkOption {
      type = with types; nullOr str;
      default = null;
      description = lib.mkDoc ''
        Specifies an override for the SSH command to use with Git.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs = {
      git = {
        enable = true;

        userName = "K. Adam Christensen";
        userEmail = "pope@shifteleven.com";

        extraConfig = {
          core.sshCommand = mkIf (cfg.sshCommand != null) cfg.sshCommand;
          init.defaultBranch = "main";
          user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo";
          commit.gpgsign = true;
          gpg.format = "ssh";
          "gpg \"ssh\"" = {
            program = cfg.opSshSignCommand;
            allowedSignersFile = "~/.ssh/allowed_signers";
          };
        };
      };

      gh = {
        enable = true;

        settings = {
          # Workaround for https://github.com/nix-community/home-manager/issues/4744
          version = 1;
        };
      };

      ssh = {
        enable = true;

        matchBlocks."shifteleven.com" = {
          addressFamily = "inet";
        };

        matchBlocks."*".extraOptions = {
          IdentityAgent = ''"${cfg.opIdentityAgent}"'';
        };
      };
    };

    home.file.".ssh/allowed_signers".text = ''
      pope@shifteleven.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo
    '';
  };
}
