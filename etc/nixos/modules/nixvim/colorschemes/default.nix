{
  pkgs,
  helpers,
  lib,
  config,
  ...
}:

let
  cfg = config.my.nixvim.theme;

  # Importing this as such because for some reason, the home-module version of
  # this doesn't work while the nixvim package does.
  nvsrcs = pkgs.callPackage ../../../packages/_sources/generated.nix { };
  neopywal-nvim = pkgs.callPackage ./../../../packages/vim/neopywal-nvim.nix { inherit nvsrcs; };
in
{
  options.my.nixvim.theme = {
    colorScheme = lib.mkOption {
      type = lib.types.enum [
        "rose-pine"
        "catppuccin"
        "dracula"
        "tokyonight"
      ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = neopywal-nvim;
      priority = 999;
      opts = {
        custom_colors.all =
          helpers.mkRaw # lua
            ''
              function(C)
                local U = require("neopywal.utils.color")
                return {
                  cursorline = U.blend(C.background, C.foreground, 0.88),
                  nontext = U.blend(C.background, C.foreground, 0.93),

                  -- Variable types:
                  variable = C.foreground, -- (preferred) any variable.
                  constant = U.blend(C.color1, C.color3, 0.5), -- (preferred) any constant
                  string = C.color2, -- a string constant: "this is a string"
                  character = C.color10, -- a character constant: 'c', '\n'
                  number = C.color3, -- a number constant: 234, 0xff
                  boolean = C.color3, -- a boolean constant: TRUE, FALSE
                  float = C.color3, -- a floating point constant: 2.3e10
                  identifier = C.color3, -- (preferred) any variable name
                  func = C.color4, -- function name (also: methods for classes)

                  member = U.blend(C.color4, C.color6, 0.5),

                  -- Statements:
                  statement = C.color1, -- (preferred) any statement
                  conditional = C.color5, -- if, then, else, endif, switch, etc.
                  loop = C.color5, -- for, do, while, etc.
                  label = C.color5, -- case, default, etc.
                  exception = C.color5, -- try, catch, throw
                  operator = C.color6, -- "sizeof", "+", "*", etc.
                  keyword = C.color13, -- any other keyword
                  debug = C.color3, -- debugging statements.

                  -- Preprocessors:
                  preproc = C.color6, -- (preferred) generic Preprocessor
                  include = C.color6, -- preprocessor #include
                  define = C.color6, -- preprocessor #define
                  macro = C.color6, -- same as Define
                  precondit = C.color6, -- preprocessor #if, #else, #endif, etc.

                  -- Type definitions:
                  type = C.color4, -- (preferred) int, long, char, etc.
                  structure = C.color4, -- struct, union, enum, etc.
                  storageclass = C.color4, -- static, register, volatile, etc.
                  typedef = C.color4, -- A typedef

                  -- Special:
                  special = C.color5, -- (preferred) any special symbol
                  secialchar = C.color5, -- special character in a constant
                  tag = U.blend(C.color1, C.color3, 0.5), -- you can use CTRL-] on this
                  delimiter = C.foreground, -- character that needs attention
                  specialcomment = C.color8, -- special things inside a comment
                }
              end
            '';
        custom_highlights.all =
          helpers.mkRaw # lua
            ''
              function(C)
                return {
                  ColorColumn = { bg = C.cursorline },
                  ["@variable.member"] = { fg = C.color12 },
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
      # TODO(pope): Load neopywal if a colors file is found and not empty.
      # Otherwise, load the sent in theme.
      config = # lua
        ''
          function(_, opts)
            -- $XDG_CONFIG_HOME is more appropriate, but the lib uses $HOME
            local config = os.getenv("HOME") .. "/.cache/wal/colors-wal.vim"
            if vim.uv.fs_stat(config) then
              require("neopywal").setup(opts)
              vim.cmd("colorscheme neopywal")
            end
          end
        '';
    }
    {
      pkg = rose-pine;
      priority = 1000;
      opts =
        helpers.mkRaw # lua
          ''
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
      ''
      + (lib.optionalString (cfg.colorScheme == "rose-pine") ''
        vim.cmd('colorscheme rose-pine')
      '')
      + ''
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
      opts.custom_highlights =
        helpers.mkRaw # lua
          ''
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
      ''
      + (lib.optionalString (cfg.colorScheme == "catppuccin") ''
        vim.cmd("colorscheme catppuccin")
      '')
      + ''
        end
      '';
    }
    {
      pkg = dracula-nvim;
      priority = 1000;
      opts.transparent_bg = true;
      opts.italic_comment = true;
      opts.overrides =
        helpers.mkRaw # lua
          ''
            function(colors)
              return {
                NotifyBackground = { bg = colors.bg },
              }
            end
          '';
      config = ''
        function(_, opts)
          require("dracula").setup(opts)
      ''
      + (lib.optionalString (cfg.colorScheme == "dracula") ''
        vim.cmd("colorscheme dracula")
      '')
      + ''
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
      ''
      + (lib.optionalString (cfg.colorScheme == "tokyonight") ''
        vim.cmd("colorscheme tokyonight")
      '')
      + ''
        end
      '';
    }
  ];
}
