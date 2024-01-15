{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.languages.javascript;
in
{
  options.my.home.languages.javascript = {
    enable = mkEnableOption "JavaScript/TypeScript language home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodePackages.typescript-language-server
      nodejs
    ];
  };
}
