{ pkgs, helpers, lib, config, ... }:

let
  inherit (lib) mkOption types optionalString;
  cfg = config.my.nixvim.theme;

  # Importing this as such because for some reason, the home-module version of
  # this doesn't work while the nixvim package does.
  nvsrcs = pkgs.callPackage ../../../packages/_sources/generated.nix { };
  neopywal-nvim = pkgs.callPackage ./../../../packages/neopywal-nvim { inherit nvsrcs; };
in
{
  options.my.nixvim.theme = {
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = neopywal-nvim;
      priority = 1000;
      opts = {
        custom_colors.all = helpers.mkRaw /* lua */ ''
          function(C)
            local U = require("neopywal.utils.color")
            return {
              cursorline = U.blend(C.background, C.foreground, 0.88),
              nontext = U.blend(C.background, C.foreground, 0.93),
            }
          end
        '';
        custom_highlights.all = helpers.mkRaw /* lua */ ''
          function(C)
            return {
              ColorColumn = { bg = C.cursorline },
              NonText = {
                fg = C.nontext,
                bg = C.none,
              },
            }
          end
        '';
        # In addition to the defaults
        plugins = {
          barbar = true;
          dap = true;
          indent_blankline = {
            enable = true;
            scope_color = "nontext";
          };
          rainbow = true;
          surround = true;
        };
        transparent_background = true;
      };
    }
    {
      pkg = rose-pine;
      priority = 1000;
      opts = helpers.mkRaw /* lua */ ''
        function()
          local p = require("rose-pine.palette")
          return {
            dark_variant = "main",
            dim_inactive_windows = false,
            extend_background_behind_borders = true,
            styles = {
              bold = true,
              italic = true,
              transparency = true,
            },
            highlight_groups = {
              ColorColumn = { bg = p.highlight_low },
              NonText = {
                fg = p.highlight_med,
                bg = p.none,
              },
              NotifyBackground = {
                bg = p.overlay,
              },
            },
          }
        end
      '';
      config = ''
        function(_, opts)
          require('rose-pine').setup(opts)
      '' + (optionalString (cfg.colorScheme == "rose-pine") ''
        vim.cmd('colorscheme rose-pine')
      '') + ''
        end
      '';
    }
    {
      pkg = catppuccin-nvim;
      priority = 1000;
      opts.flavor = "mocha";
      opts.transparent_background = true;
      opts.dim_inactive.enabled = false;
      opts.integrations = {
        cmp = true;
        dashboard = true;
        gitsigns = true;
        indent_blankline.enabled = true;
        markdown = true;
        native_lsp.enabled = true;
        noice = true;
        notify = true;
        rainbow_delimiters = true;
        telescope.enabled = true;
        treesitter = true;
        treesitter_context = true;
        ufo = true;
        which_key = true;
      };
      opts.custom_highlights = helpers.mkRaw /* lua */ ''
        function(colors)
          return {
            ColorColumn = { bg = colors.surface0 },
            NonText = { fg = colors.surface0 },
          }
        end
      '';
      config = ''
        function(_, opts)
          require("catppuccin").setup(opts)
      '' + (optionalString (cfg.colorScheme == "catppuccin") ''
        vim.cmd("colorscheme catppuccin")
      '') + ''
        end
      '';
    }
    {
      pkg = dracula-nvim;
      priority = 1000;
      opts.transparent_bg = true;
      opts.italic_comment = true;
      opts.overrides = helpers.mkRaw /* lua */ ''
        function(colors)
          return {
            NotifyBackground = { bg = colors.bg },
          }
        end
      '';
      config = ''
        function(_, opts)
          require("dracula").setup(opts)
      '' + (optionalString (cfg.colorScheme == "dracula") ''
        vim.cmd("colorscheme dracula")
      '') + ''
        end
      '';
    }
    {
      pkg = tokyonight-nvim;
      priority = 1000;
      opts = {
        transparent = true;
        styles = {
          floats = "transparent";
        };
      };
      config = ''
        function(_, opts)
          require("tokyonight").setup(opts)
      '' + (optionalString (cfg.colorScheme == "tokyonight") ''
        vim.cmd("colorscheme tokyonight")
      '') + ''
        end
      '';
    }
  ];
}
