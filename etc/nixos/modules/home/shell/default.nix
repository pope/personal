{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.shell;
in
{
  options.my.home.shell = {
    enable = mkEnableOption "Fish shell home options";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      interactiveShellInit = ''
        fish_config theme choose "Rosé Pine"
      '';
      plugins = [
        { name = "tide"; inherit (pkgs.fishPlugins.tide) src; }
      ];
    };

    xdg.configFile."fish/themes" = {
      source = "${pkgs.fish-rose-pine}/share/fish/themes";
      recursive = true;
    };
  };
}
