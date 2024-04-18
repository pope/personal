{ pkgs, helpers, lib, config, ... }:

let
  inherit (import ./lib.nix { inherit helpers; }) lua;
  inherit (lib) mkOption types optionalString;
  cfg = config.my.nixvim.theme;
in
{
  options.my.nixvim.theme = {
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = rose-pine;
      priority = 1000;
      opts = lua ''
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
      opts.custom_highlights = lua ''
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
  ];
}
