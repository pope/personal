{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.shell;
in
{
  imports = [
    ./fish.nix
    ./zsh.nix
  ];

  options.my.home.shell = {
    overrideDefaultShell = lib.mkEnableOption "overriding the default shell in terminals and tmux";
    defaultShell = lib.mkOption {
      type = with lib.types; nullOr str;
      readOnly = true;
      default =
        if !cfg.overrideDefaultShell then
          null
        else if cfg.zsh.enable then
          lib.getExe pkgs.zsh
        else if cfg.fish.enable then
          lib.getExe pkgs.fish
        else
          throw "my.home.shell.overrideDefaultShell requires either zsh or fish to be enabled";
      description = ''
        Read-only path to the configured shell binary when overrideDefaultShell is enabled, or null otherwise.
      '';
    };
    loginShell = lib.mkOption {
      type = with lib.types; nullOr str;
      readOnly = true;
      default = if cfg.defaultShell != null then "${cfg.defaultShell} --login" else null;
      description = ''
        Read-only login shell command when overrideDefaultShell is enabled, or null otherwise.
      '';
    };
  };

  config = {
    programs.oh-my-posh = lib.mkIf (cfg.zsh.enable || cfg.fish.enable) {
      enable = true;
      settings = builtins.fromJSON (
        builtins.unsafeDiscardStringContext (builtins.readFile ./p10k.omp.json)
      );
    };
  };
}
