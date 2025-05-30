{ config, lib, ... }:

let
  cfg = config.my.home.shell.zsh;
in
{
  options.my.home.shell.zsh = {
    enable = lib.mkEnableOption "zsh shell home options";
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      defaultKeymap = "emacs";
      dotDir = ".config/zsh";
      autosuggestion.enable = true;
      enableCompletion = true;
      history = {
        extended = true;
        expireDuplicatesFirst = true;
        path = "${config.xdg.dataHome}/zsh/history";
        size = 99999;
      };
      initContent = lib.mkMerge [
        # Add this escape hatch for Emacs remote editing with tramp.
        # See https://www.emacswiki.org/emacs/TrampMode#h5o-9
        (lib.mkOrder 500 /* sh */ ''
          [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
        '')

        (lib.mkOrder 550 /* sh */ ''
          if [ -e /opt/homebrew/bin/brew ]
          then
            fpath+=($(brew --prefix)/share/zsh/site-functions)
          fi
        '')

        (lib.mkOrder 1000 /* sh */ ''
          autoload -z edit-command-line
          zle -N edit-command-line
          bindkey "^X^E" edit-command-line
        '')
      ];
      profileExtra = /* sh */ ''
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
        "ls" = "eza --group-directories-first";
      };
      syntaxHighlighting = {
        enable = true;
        highlighters = [ "main" "brackets" "pattern" "line" "cursor" "root" ];
      };
    };

    programs.dircolors = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.yazi.enableZshIntegration = true;
  };
}
