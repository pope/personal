{ pkgs, helpers, ... }:

{
  config.autoGroups.cursorColor = { };
  config.autoCmd = [
    {
      group = "cursorColor";
      event = "ModeChanged";
      callback = helpers.mkRaw /* lua */ ''
        function()
          local auto = require("lualine.themes.auto")
          local modes = {
            ["n"] = auto.normal.a.bg,
            ["i"] = auto.insert.a.bg,
            ["c"] = auto.command.a.bg,
            ["V"] = auto.visual.a.bg,
            ["v"] = auto.visual.a.bg,
            [""] = auto.visual.a.bg,
            ["S"] = auto.visual.a.bg,
            ["s"] = auto.visual.a.bg,
            ["R"] = auto.replace.a.bg,
            ["r"] = auto.replace.a.bg,
            ["t"] = auto.command.a.bg,
          }
          local mode = vim.api.nvim_get_mode().mode
          local color = modes[mode] or modes.n

          local base_highlight = vim.api.nvim_get_hl_by_name('CursorLineNr', true)
          local o = vim.tbl_extend("keep", { foreground = color }, base_highlight)
          vim.api.nvim_set_hl(0, "CursorLineNr", o)
        end
      '';
    }
  ];
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = lualine-nvim;
      dependencies = [ nvim-web-devicons nvim-navic ];
      event = "VeryLazy";
      opts.extensions = [ "fugitive" "nvim-tree" "toggleterm" ];
      opts.options = {
        section_separators = { left = ""; right = ""; };
        component_separators = { left = ""; right = ""; };
        theme = "auto";
      };
      opts.sections.lualine_c = [
        "filename"
        (helpers.listToUnkeyedAttrs [ "navic" ] // { navic_opts = null; })
      ];
    }
  ];
}
