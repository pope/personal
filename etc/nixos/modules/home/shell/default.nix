{ config, lib, ... }:

let
  cfg = config.my.home.shell;
in
{
  imports = [
    ./fish.nix
    ./zsh.nix
  ];

  programs.oh-my-posh = lib.mkIf (cfg.zsh.enable || cfg.fish.enable) {
    enable = true;
    # TODO(pope): Support a few more p10k things
    # - Folder icons for the specific path (a house for ~, etc...)
    # - A less busy git, with maybe counts to push/pull
    # - Other CLI hints
    settings = builtins.fromJSON
      (builtins.unsafeDiscardStringContext
        (builtins.readFile ./p10k.omp.json));
  };
}
