{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.packages;

  mkDefaultAttrs = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
  keymap = mkDefaultAttrs (builtins.fromTOML (builtins.readFile ./keymap-default.toml));
  settings = mkDefaultAttrs (builtins.fromTOML (builtins.readFile ./yazi-default.toml));
in
{
  options.my.home.yazi = {
    enable = lib.mkEnableOption "Yazi options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ mediainfo ];

    programs.yazi = {
      enable = true;
      keymap = lib.mkMerge [
        keymap
        {
          mgr.prepend_keymap = [
            {
              on = "T";
              run = "plugin toggle-pane max-preview";
              desc = "Maximize or restore preview";
            }
          ];
        }
      ];
      plugins = {
        inherit (pkgs.yaziPlugins) mediainfo toggle-pane;
      };
      settings = lib.mkMerge [
        settings
        {
          mgr.ratio = [ 2 4 3 ];
          plugin = {
            prepend_preloaders = [
              { mime = "{audio,video,image}/*"; run = "mediainfo"; }
              { mime = "application/{subrip,postscript}"; run = "mediainfo"; }
            ];
            prepend_previewers = [
              { mime = "{audio,video,image}/*"; run = "mediainfo"; }
              { mime = "application/{subrip,postscript}"; run = "mediainfo"; }
            ];
          };
          preview = {
            max_width = 2048;
            max_height = 2048;
          };
        }
      ];
    };

    xdg.configFile = {
      # Not really needed, but good for reference.
      "yazi/theme-dark.toml".source = ./theme-dark.toml;
      "yazi/theme-light.toml".source = ./theme-light.toml;
    };
  };
}
