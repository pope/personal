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
    settings = builtins.fromJSON (
      builtins.unsafeDiscardStringContext (builtins.readFile ./p10k.omp.json)
    );
  };
}
