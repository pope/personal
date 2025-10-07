{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.git;
  ghWrapper = pkgs.writeShellScriptBin "op-gh" ''
    if [ -e "$HOME/.config/op/plugins/gh.json" ]; then
      exec op plugin run -- gh "''${@:1}"
    else
      exec ${pkgs.gh}/bin/gh "''${@:1}"
    fi
  '';
in
{
  options.my.home.git = {
    enable = lib.mkEnableOption "Git home options";

    opSshSignCommand = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs._1password-gui}/bin/op-ssh-sign";
      description = lib.mdDoc ''
        The location of the 1Password SSH sign app used for signing Git
        commits.
      '';
    };

    opIdentityAgent = lib.mkOption {
      type = lib.types.str;
      default = "~/.1password/agent.sock";
      description = lib.mkDoc ''
        The location of the 1Password agent socket used for SSH.
      '';
    };

    sshCommand = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = lib.mkDoc ''
        Specifies an override for the SSH command to use with Git.
      '';
    };

    remoteOnly = lib.mkEnableOption "if the config is on a remote machine only";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      git = {
        enable = true;
        lfs.enable = true;

        userName = "K. Adam Christensen";
        userEmail = "pope@shifteleven.com";

        diff-so-fancy.enable = true;

        extraConfig = {
          credential = lib.mkIf (!cfg.remoteOnly) (
            builtins.listToAttrs (
              lib.map
                (
                  host:
                  lib.nameValuePair host {
                    helper = "${ghWrapper}/bin/op-gh auth git-credential";
                  }
                )
                [
                  "https://github.com"
                  "https://gist.github.com"
                ]
            )
          );
          core.sshCommand = lib.mkIf (cfg.sshCommand != null) cfg.sshCommand;
          format.signOff = true;
          gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";
          init.defaultBranch = "main";
          log.showSignature = false;
          push = {
            autoSetupRemote = true;
            default = "simple";
            recurseSubmodules = "on-demand";
          };
          submodule.recurse = true;
        };

        signing = {
          format = "ssh";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo";
          signByDefault = true;
          signer = lib.mkIf (!cfg.remoteOnly) cfg.opSshSignCommand;
        };
      };

      gh = {
        enable = !cfg.remoteOnly;
        gitCredentialHelper.enable = false;

        settings = {
          # Workaround for https://github.com/nix-community/home-manager/issues/4744
          version = 1;
        };
      };

      lazygit = {
        enable = true;
        settings = {
          gui = {
            nerdFontsVersion = "3";
            showDivergenceFromBaseBranch = "arrowAndNumber";
            filterMode = "fuzzy";
          };
          git = {
            commit.signOff = true;
            parseEmoji = true;
            paging = {
              color = "always";
              pager = "diff-so-fancy";
            };
          };
        };
      };

      # TODO(pope): Move this somewhere else. This is common enough for other
      # 1password plugins, but `gh` is the only one I really use now.
      fish.interactiveShellInit = # fish
        ''
          if test -e "$HOME/.config/op/plugins.sh"
            source "$HOME/.config/op/plugins.sh"
          end
        '';
      zsh.initContent = # sh
        ''
          if [ -e "$HOME/.config/op/plugins.sh" ]; then
            source "$HOME/.config/op/plugins.sh"
          fi
        '';
    };
    home = {
      file.".ssh/allowed_signers".text = ''
        pope@shifteleven.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo
      '';
      packages = with pkgs; [
        git-code-maintenance
        serie
      ];
    };
  };
}
