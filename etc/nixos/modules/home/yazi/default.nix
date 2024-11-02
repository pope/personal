{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.packages;

  keymap = builtins.fromTOML (builtins.readFile ./keymap.toml);
  settings = builtins.fromTOML (builtins.readFile ./yazi.toml);
in
{
  options.my.home.yazi = {
    enable = mkEnableOption "Yazi options";
  };

  config = mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      keymap = keymap // {
        manager.prepend_keymap = [
          {
            on = "T";
            run = "plugin --sync max-preview";
            desc = "Maximize or restore preview";
          }
        ];
      };
      settings = settings // {
        preview = {
          max_width = 2048;
          max_height = 4096;
        };
      };
    };

    xdg.configFile = {
      "yazi/plugins".source = "${pkgs.yazi-plugins}/share/yazi/plugins";
    };
  };
}
