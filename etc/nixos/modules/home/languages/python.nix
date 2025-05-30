{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.languages.python;
in
{
  options.my.home.languages.python = {
    enable = lib.mkEnableOption "Python language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pyright
      python312Packages.pyls-isort
      python312Packages.python-lsp-server
      python3Full
    ];
  };
}

