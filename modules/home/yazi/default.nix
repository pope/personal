{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.yazi;

  mkDefaultAttrs = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
  keymap = mkDefaultAttrs (fromTOML (builtins.readFile ./keymap-default.toml));
  settings = mkDefaultAttrs (fromTOML (builtins.readFile ./yazi-default.toml));
  rawUrl = "*.{arw,ARW,raf,RAF,cr2,CR2,cr3,CR3,nef,NEF,dng,DNG,orf,ORF,rw2,RW2,pef,PEF}";
  rawMime = "image/x-{sony-arw,fuji-raf,canon-cr2,canon-cr3,nikon-nef,adobe-dng,olympus-orf,panasonic-rw2,pentax-pef}";
in
{
  options.my.home.yazi = {
    enable = lib.mkEnableOption "Yazi options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      exiftool
      exiv2
      mediainfo
    ];

    programs.yazi = {
      enable = true;
      initLua = ''
        require("mime-ext"):setup()
        require("mime-ext.local"):setup {
          with_exts = {
            arw = "image/x-sony-arw",
            raf = "image/x-fuji-raf",
            cr2 = "image/x-canon-cr2",
            cr3 = "image/x-canon-cr3",
            nef = "image/x-nikon-nef",
            dng = "image/x-adobe-dng",
            orf = "image/x-olympus-orf",
            rw2 = "image/x-panasonic-rw2",
            pef = "image/x-pentax-pef",
            heic = "image/heic",
            heif = "image/heif",
            xmp = "text/xml",
          },
          fallback_file1 = true,
        }
      '';
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
        inherit (pkgs.yaziPlugins) mime-ext toggle-pane;
        exif-image = ./exif-image.yazi;
      };
      settings = lib.mkMerge [
        settings
        {
          mgr.ratio = [
            2
            4
            3
          ];
          plugin = {
            prepend_spotters = [
              {
                url = rawUrl;
                run = "exif-image";
              }
              {
                mime = "{image,audio,video}/*";
                run = "exif-image";
              }
              {
                mime = "application/{subrip,postscript}";
                run = "exif-image";
              }
            ];
            prepend_preloaders = [
              {
                mime = rawMime;
                run = "exif-image";
              }
              {
                mime = "image/tiff";
                run = "magick";
              }
            ];
            prepend_previewers = [
              {
                url = rawUrl;
                run = "exif-image";
              }
              {
                mime = rawMime;
                run = "exif-image";
              }
              {
                mime = "image/tiff";
                run = "magick";
              }
            ];
          };
          preview = {
            max_width = 1200;
            max_height = 1200;
            image_delay = 10;
            image_quality = 65;
          };
          tasks = {
            preload_workers = 2;
            plugin_workers = 8;
            image_alloc = 1073741824; # 1GB
            image_bound = [
              24000
              24000
            ];
          };
        }
      ];
      shellWrapperName = "y";
    };

    xdg.configFile = {
      # Not really needed, but good for reference.
      "yazi/theme-dark.toml".source = ./theme-dark.toml;
      "yazi/theme-light.toml".source = ./theme-light.toml;
    };
  };
}
