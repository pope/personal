let
  lua = x: { __raw = x; };
in
{
  autoGroups = {
    cursorColor = { };
    lspConfig = { };
    restoreCursorPosition = { };
  };
  autoCmd = [
    {
      group = "cursorColor";
      event = "ModeChanged";
      callback = lua ''
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
    {
      group = "lspConfig";
      event = "LspAttach";
      pattern = "*";
      callback = lua ''
        function(opts)
          local bufnr = opts.buf
          local client = vim.lsp.get_client_by_id(opts.data.client_id)
          local capabilities = client.server_capabilities

          -- TODO(pope): Conditionally enable this based on Neovim version
          -- -- Enable inlay hints if supported
          -- if capabilities.inlayHintProvider then
          --   vim.lsp.inlay_hint.enable(bufnr, true)
          -- end
          -- -- Some Lsp servers do not advertise inlay hints properly so enable this keybinding regardless
          -- vim.keymap.set('n', '<space>ht',
          --   function()
          --     vim.lsp.inlay_hint.enable(0, not vim.lsp.inlay_hint.is_enabled())
          --   end,
          --   { desc = "Hints toggle", noremap = true, buffer = bufnr, }
          -- )

          vim.keymap.set("n", "<space>F",
            function()
              vim.lsp.buf.format()
              vim.api.nvim_command("write")
            end,
            { desc = "Format and save", noremap = true, buffer = bufnr, }
          )
          if client.server_capabilities["documentSymbolProvider"] then
            require("nvim-navic").attach(client, bufnr)
          end
        end
      '';
    }
    {
      group = "restoreCursorPosition";
      event = "BufReadPost";
      pattern = "*";
      callback = lua ''
        function()
          if vim.fn.line "'\"" > 0 and vim.fn.line "'\"" <= vim.fn.line "$" then
            vim.cmd [[execute "normal! g'\""]]
          end
        end
      '';
    }
  ];
}
