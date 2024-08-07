{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.python;
in
{
  options.my.home.languages.python = {
    enable = mkEnableOption "Python language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pyright
      python312Packages.pyls-isort
      python312Packages.python-lsp-server
      python3Full
    ];
  };
}

