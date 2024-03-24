{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkMerge mkOption types;
  cfg = config.my.home.shell.fish;
in
{
  options.my.home.shell.fish = {
    enable = mkEnableOption "Fish shell home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs.fish = {
        enable = true;
        plugins = [
          { name = "tide"; inherit (pkgs.fishPlugins.tide) src; }
        ];
      };
    })

    (mkIf (cfg.enable && cfg.colorScheme == "rose-pine") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "Rosé Pine"
      '';
      xdg.configFile."fish/themes" = {
        source = "${pkgs.fish-rose-pine}/share/fish/themes";
        recursive = true;
      };
    })

    (mkIf (cfg.enable && cfg.colorScheme == "catppuccin") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "Catppuccin Mocha"
      '';
      xdg.configFile."fish/themes" = {
        source = "${pkgs.fish-catppuccin}/share/fish/themes";
        recursive = true;
      };
    })
  ];
}
