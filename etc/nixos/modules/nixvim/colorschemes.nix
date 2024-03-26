{
  config.colorschemes.rose-pine =
    let
      lua = x: { __raw = x; };
    in
    {
      enable = true;
      transparentBackground = true;
      highlightGroups = {
        ColorColumn = {
          bg = lua "require('rose-pine.palette').highlight_low";
        };
        IblIndent = {
          fg = lua "require('rose-pine.palette').highlight_high";
        };
        IblScope = {
          fg = lua "require('rose-pine.palette').highlight_high";
        };
        NonText = {
          fg = lua "require('rose-pine.palette').highlight_med";
          bg = lua "require('rose-pine.palette').none";
        };
        NotifyBackground = {
          bg = lua "require('rose-pine.palette').overlay";
        };
      };
    };
  config.colorschemes.catppuccin =
    let
      lua = x: x;
    in
    {
      enable = false;
      flavour = "mocha";
      customHighlights = lua ''
        function(colors)
          return {
            ColorColumn = { bg = colors.surface0 },
            NonText = { fg = colors.surface0, },
          }
        end
      '';
      dimInactive.enabled = true;
      integrations = {
        cmp = true;
        dashboard = true;
        fidget = true;
        gitsigns = true;
        indent_blankline.enabled = true;
        lsp_trouble = true;
        markdown = true;
        native_lsp.enabled = true;
        navic.enabled = true;
        noice = true;
        notify = true;
        nvimtree = true;
        rainbow_delimiters = true;
        telescope.enabled = true;
        treesitter = true;
        treesitter_context = true;
        ufo = true;
        which_key = true;
      };
      transparentBackground = true;
    };
}
