{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.home.packages;

  bashIntegration = ''
    if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
      eval "$(${lib.getExe pkgs.fzf} --bash)"
    fi
  '';
  zshIntegration = ''
    if [[ $options[zle] = on ]]; then
      source <(${lib.getExe pkgs.fzf} --zsh)
    fi
  '';
  fishIntegration = ''
    ${lib.getExe pkgs.fzf} --fish | source
  '';

  colors = with config.my.home.theme.colors.withHash; {
    "bg+" = base02;
    "fg+" = base05;
    "hl+" = base0A;
    bg = base00;
    border = base07;
    fg = base04;
    gutter = base00;
    header = base0B;
    hl = base08;
    info = base0C;
    marker = base0E;
    pointer = base0D;
    prompt = base04;
    separator = base07;
    spinner = base0E;
  };
  renderedColors = lib.concatStringsSep "," (
    lib.mapAttrsToList (name: value: "${name}:${value}") colors
  );
  defaultOptions = lib.concatStringsSep "\n" [
    "--bind 'ctrl-p:toggle-preview'"
    "--bind 'alt-a:select-all'"
    "--bind 'alt-n:deselect-all'"
    "--bind 'ctrl-f:jump'"
    "--bind 'ctrl-/:change-preview-window(down|hidden|)'"
    "--color ${renderedColors}"
  ];

  changeDirWidgetCommand = "${lib.getExe pkgs.fd} --type directory --hidden";
  defaultCommand = "${lib.getExe pkgs.fd} --type file --hidden";
  dirPreviewOpt = "--preview '${lib.getExe pkgs.eza} --tree --color=always --icons {} | head -200'";
  filePreviewOpt = "--preview '${lib.getExe pkgs.fzf-preview} {}'";
  historyWidgetOptions = toString [
    ''
      --preview 'echo {} | ${lib.getExe pkgs.gnused} \"s/^ *[0-9*]\+ *//\" | ${lib.getExe pkgs.bat} --language=sh --color=always --plain'
    ''
    "--preview-window up:3:hidden:wrap"
  ];
in
{
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        fzf
        fzf-preview
      ];

      sessionVariables = {
        FZF_ALT_C_COMMAND = changeDirWidgetCommand;
        FZF_ALT_C_OPTS = dirPreviewOpt;
        FZF_COMPLETION_DIR_OPTS = dirPreviewOpt;
        FZF_COMPLETION_PATH_OPTS = filePreviewOpt;
        FZF_CTRL_R_OPTS = historyWidgetOptions;
        FZF_CTRL_T_COMMAND = defaultCommand;
        FZF_CTRL_T_OPTS = filePreviewOpt;
        FZF_PREVIEW_IMAGE_HANDLER = "symbols";
        FZF_DEFAULT_OPTS_FILE = "${config.xdg.configHome}/fzf/fzfrc";
      }
      // lib.optionalAttrs config.my.home.tmux.enable {
        FZF_TMUX = "1";
        FZF_TMUX_OPTS = "-p 75%";
      };
    };

    # Order rules taken from
    # https://github.com/nix-community/home-manager/blob/master/modules/programs/fzf.nix
    programs = {
      bash.initExtra = lib.mkIf cfg.enableBashIntegration (lib.mkOrder 200 bashIntegration);
      zsh.initContent = lib.mkIf cfg.enableZshIntegration (lib.mkOrder 910 zshIntegration);
      fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration (lib.mkOrder 200 fishIntegration);
    };

    xdg.configFile."fzf/fzfrc".text = defaultOptions;
  };
}
