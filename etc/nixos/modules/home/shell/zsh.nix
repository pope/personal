{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.shell.zsh;
in
{
  options.my.home.shell.zsh = {
    enable = mkEnableOption "zsh shell home options";
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      defaultKeymap = "emacs";
      dotDir = ".config/zsh";
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        extended = true;
        expireDuplicatesFirst = true;
        path = "${config.xdg.dataHome}/zsh/history";
      };
      plugins = [
        {
          name = "powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
        }
        {
          name = "powerlevel10k-config";
          src = ./.;
          file = "p10k.zsh";
        }
      ];
      profileExtra = ''
        if [ -e /opt/homebrew/bin/brew ]
        then
          eval "$(/opt/homebrew/bin/brew shellenv)"
        fi

        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]
        then
          . $HOME/.nix-profile/etc/profile.d/nix.sh;
        fi

        if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]
        then
          . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        fi
      '';
      sessionVariables = {
        CLICOLOR = 1;
      };
      shellAliases = {
        "eza" = "eza --group-directories-first";
      };
      syntaxHighlighting.enable = true;
    };

    programs.dircolors = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.yazi.enableZshIntegration = true;
  };
}

