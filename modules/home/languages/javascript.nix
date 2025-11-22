{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.languages.javascript;
in
{
  options.my.home.languages.javascript = {
    enable = lib.mkEnableOption "JavaScript/TypeScript language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nodePackages.typescript-language-server
      nodejs
    ];
  };
}
