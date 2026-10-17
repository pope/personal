{
  config,
  pkgs,
  lib,
  ...
}:

# Per-shell fzf configuration:
#
# Home Manager's `programs.fzf` places top-level `defaultOptions`, `colors`,
# `tmux`, and widget options (`changeDirWidget.command`, `fileWidget.command`,
# etc.) into `home.sessionVariables` (`hm-session-vars.sh`). Because
# `hm-session-vars.sh` is guarded by `__HM_SESS_VARS_SOURCED=1`, changes to
# `home.sessionVariables` (including updated `/nix/store/...` paths for preview
# tools) do not take effect until logging out of the desktop session and logging
# back in.
#
# To make fzf changes take effect in new shells without logging out:
# 1. Widget commands and options (`ALT-C`, `CTRL-T`, `CTRL-R`) use
#    `programs.fzf.<widget>.{bash,fish,zsh}` per-shell options rather than the
#    top-level widget options, which causes Home Manager to export them directly
#    in `.bashrc` / `config.fish` / `.zshrc` instead of `hm-session-vars.sh`.
# 2. Remaining `FZF_*` variables that lack per-shell options in `programs.fzf`
#    (`FZF_COMPLETION_DIR_OPTS`, `FZF_COMPLETION_PATH_OPTS`,
#    `FZF_DEFAULT_OPTS`, `FZF_PREVIEW_IMAGE_HANDLER`, `FZF_TMUX`,
#    `FZF_TMUX_OPTS`) are exported directly in the shell init hooks alongside
#    `programs.fzf`.

let
  cfg = config.my.home.packages;
  fzfCfg = config.programs.fzf;

  # Home Manager's `programs.fzf` exports per-shell variables in `bash` and `zsh`
  # using `config.lib.shell.exportAll` (`export VAR="${toString v}"`, which does
  # not escape inner double quotes), whereas `fish` uses `lib.escapeShellArg`
  # (single-quoted). Escape `"` for `bash` and `zsh` while passing unescaped
  # strings to `fish`.
  escapeForExportAll = lib.replaceStrings [ ''"'' ] [ ''\"'' ];

  perShell =
    attrs:
    let
      escapedAttrs =
        lib.optionalAttrs (attrs ? command) {
          command = escapeForExportAll attrs.command;
        }
        // lib.optionalAttrs (attrs ? options) {
          options = map escapeForExportAll attrs.options;
        };
    in
    {
      bash = escapedAttrs;
      fish = attrs;
      zsh = escapedAttrs;
    };

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
  defaultOptions = lib.concatStringsSep " " [
    "--bind 'ctrl-p:toggle-preview'"
    "--bind 'alt-a:select-all'"
    "--bind 'alt-n:deselect-all'"
    "--bind 'ctrl-f:jump'"
    "--bind 'ctrl-/:change-preview-window(down|hidden|)'"
    "--color ${renderedColors}"
  ];

  dirPreviewOpt = "--preview '${lib.getExe pkgs.eza} --tree --color=always --icons {} | head -200'";
  filePreviewOpt = "--preview '${lib.getExe pkgs.fzf-preview} {}'";

  extraVars = {
    FZF_COMPLETION_DIR_OPTS = dirPreviewOpt;
    FZF_COMPLETION_PATH_OPTS = filePreviewOpt;
    FZF_DEFAULT_OPTS = defaultOptions;
    FZF_PREVIEW_IMAGE_HANDLER = "symbols";
  }
  // lib.optionalAttrs config.my.home.tmux.enable {
    FZF_TMUX = "1";
    FZF_TMUX_OPTS = "-p 75%";
  };

  posixExtraExports = config.lib.shell.exportAll (lib.mapAttrs (_: escapeForExportAll) extraVars);
  fishExtraExports = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (name: value: "set -gx ${name} ${lib.escapeShellArg value}") extraVars
  );
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      fzf-preview
    ];

    programs = {
      bash.initExtra = lib.mkIf fzfCfg.enableBashIntegration (lib.mkOrder 200 posixExtraExports);
      zsh.initContent = lib.mkIf fzfCfg.enableZshIntegration (lib.mkOrder 910 posixExtraExports);
      fish.interactiveShellInit = lib.mkIf fzfCfg.enableFishIntegration (
        lib.mkOrder 200 fishExtraExports
      );

      fzf = {
        enable = true;

        changeDirWidget = perShell {
          command = "${lib.getExe pkgs.fd} --type directory --hidden";
          options = [ dirPreviewOpt ];
        };

        fileWidget = perShell {
          command = "${lib.getExe pkgs.fd} --type file --hidden";
          options = [ filePreviewOpt ];
        };

        historyWidget = perShell {
          options = [
            "--preview 'echo {} | ${lib.getExe pkgs.gnused} \"s/^ *[0-9*]\\+ *//\" | ${lib.getExe pkgs.bat} --language=sh --color=always --plain'"
            "--preview-window up:3:hidden:wrap"
          ];
        };
      };
    };
  };
}
