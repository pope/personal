{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.shell.fish;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.shell.fish = {
    enable = lib.mkEnableOption "Fish shell home options";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.fish.enable = true;
    })

    (lib.mkIf (cfg.enable && colorScheme == "rose-pine") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "Rosé Pine"
      '';
      xdg.configFile."fish/themes" = {
        source = "${pkgs.fish-rose-pine}/share/fish/themes";
        recursive = true;
      };
    })

    (lib.mkIf (cfg.enable && colorScheme == "catppuccin") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "Catppuccin Mocha"
      '';
      xdg.configFile."fish/themes" = {
        source = "${pkgs.fish-catppuccin}/share/fish/themes";
        recursive = true;
      };
    })

    (lib.mkIf (cfg.enable && colorScheme == "dracula") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "Dracula"
      '';
    })

    (lib.mkIf (cfg.enable && colorScheme == "tokyonight") {
      programs.fish.interactiveShellInit = ''
        fish_config theme choose "TokyoNight Storm"
      '';
      xdg.configFile."fish/themes" = {
        source = "${pkgs.fish-tokyonight}/share/fish/themes";
        recursive = true;
      };
    })
  ];
}
