{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.shell.fish;
  inherit (config.my.home.theme) colorScheme;

  themes = {
    rose-pine = {
      name = "Rosé Pine";
      package = pkgs.fish-rose-pine;
    };
    catppuccin = {
      name = "Catppuccin Mocha";
      package = pkgs.fish-catppuccin;
    };
    dracula = {
      name = "Dracula";
      package = null;
    };
    tokyonight = {
      name = "TokyoNight Storm";
      package = pkgs.fish-tokyonight;
    };
  };
  theme = themes.${colorScheme};
in
{
  options.my.home.shell.fish = {
    enable = lib.mkEnableOption "Fish shell home options";
  };

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      interactiveShellInit = ''
        fish_config theme choose "${theme.name}"
      '';
    };

    xdg.configFile."fish/themes" = lib.mkIf (theme.package != null) {
      source = "${theme.package}/share/fish/themes";
      recursive = true;
    };
  };
}
