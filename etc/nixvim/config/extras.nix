let
  lua = x: x;
in
{
  extraConfigLua = lua ''
    -- diagnostic
    local sign = function(opts)
      vim.fn.sign_define(opts.name, {
        texthl = opts.name,
        text = opts.text,
        numhl = ""
      })
    end

    sign({ name = "DiagnosticSignError", text = "✘" })
    sign({ name = "DiagnosticSignWarn", text = "▲" })
    sign({ name = "DiagnosticSignHint", text = "⚑" })
    sign({ name = "DiagnosticSignInfo", text = "" })

    vim.diagnostic.config({
      -- Enable warnings inline.
      virtual_text = true,
      signs = true,
      update_in_insert = false,
      underline = true,
      severity_sort = true,
      float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
      },
    })
  '';

  extraConfigLuaPost = lua ''
    require('luasnip.loaders.from_vscode').lazy_load()
  '';
}
