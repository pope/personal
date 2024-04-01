{ pkgs, helpers, ... }:

let
  inherit (import ./lib.nix { inherit helpers; }) lua;
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
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
          vim.cmd("colorscheme catppuccin")
        end
      '';
    }
  ];
}
