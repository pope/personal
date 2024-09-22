{ pkgs, helpers, ... }:

# Shout out to https://vonheikemen.github.io/devlog/tools/setup-nvim-lspconfig-plus-nvim-cmp/
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = nvim-navic;
      lazy = true;
      config = true;
    }
    {
      pkg = neodev-nvim;
      ft = [ "lua" ];
      opts.pathStrict = true;
    }
    {
      pkg = nvim-cmp;
      dependencies = [
        # Utils
        lspkind-nvim
        # Config
        cmp-nvim-lsp
        nvim-lspconfig
        # Autocompletion
        cmp-buffer
        cmp-nvim-lua
        cmp-path
        cmp_luasnip
        # Snippets
        luasnip
        friendly-snippets
        # Extra
        nvim-navic
      ];
      event = [ "BufReadPre" "BufNewFile" ];
      opts = helpers.mkRaw /* lua */ ''
        function (_, opts)
          local capabilities = vim.lsp.protocol.make_client_capabilities()
          capabilities.textDocument.completion.completionItem.snippetSupport = true
          capabilities.textDocument.foldingRange = {
            dynamicRegistration = false,
            lineFoldingOnly = true,
          }
          capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

          local server = {
            clangd = {},
            dartls = {},
            gdscript = {},
            glsl_analyzer = {},
            kotlin_language_server = {},
            nil_ls = {},
            phpactor = {},
            pyright = {
              pyright = {
                exclude = { ".venv" },
                venvPath = ".",
                venv = ".venv",
              },
            },
            rust_analyzer = {
              ["rust-analyzer"] = {
                files = {
                  excludeDirs = { ".direnv", ".devenv" },
                },
              },
            },
            lua_ls = {
              Lua = {
                diagnostics = {
                  globals = { "vim" },
                },
                workspace = { checkThirdParty = false },
                telemetry = { enable = false },
              },
            },
            gopls = {
              gopls = {
                analyses = {
                  nilness = true,
                  shadow = true,
                  unusedparams = true,
                  unusewrites = true,
                },
                hints = {
                  assignVariableTypes = true,
                  compositeLiteralFields = true,
                  constantValues = true,
                  functionTypeParameters = true,
                  parameterNames = true,
                  rangeVariableTypes = true,
                },
                staticcheck = true,
              },
            },
            ts_ls = {},
            zls = {
              zls = {
                enable_snippets = true,
                warn_style = true,
              },
            },
          }

          local lspconfig = require("lspconfig")
          for name, settings in pairs(server) do
            lspconfig[name].setup({
              capabilities = capabilities,
              settings = settings,
            })
          end

          local cmp = require("cmp")

          local lspkind = require("lspkind")
          lspkind.init()

          local luasnip = require("luasnip")
          require("luasnip.loaders.from_vscode").lazy_load()

          return vim.tbl_extend("keep", {
            formatting = {
              format = lspkind.cmp_format({
                with_text = true,
                maxwidth = 60,
                menu = {
                  buffer = "[buffer ]",
                  luasnip = "[snip ]",
                  nvim_lsp = "[LSP ]",
                  nvim_lua = "[API ]",
                  path = "[path 練]",
                },
              }),
            },
            mapping = cmp.mapping.preset.insert {
              ["<C-d>"] = cmp.mapping.scroll_docs(-4),
              ["<C-f>"] = cmp.mapping.scroll_docs(4),
              ["<C-Space>"] = cmp.mapping.complete({}),
              ["<CR>"] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
              },
              ["<C-e>"] = cmp.mapping(function(_)
                if cmp.visible() then
                  cmp.abort()
                else
                  cmp.complete()
                end
              end),
              ["<Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                  cmp.select_next_item()
                elseif luasnip.expand_or_jumpable() then
                  luasnip.expand_or_jump()
                else
                  fallback()
                end
              end, { "i", "s" }),
              ["<S-Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                  cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                  luasnip.jump(-1)
                else
                  fallback()
                end
              end, { "i", "s" }),
            },
            sources = cmp.config.sources({
              { name = "luasnip", keyword_length = 2 },
              { name = "nvim_lsp" },
              { name = "path" },
            }, {
              { name = "buffer", keyword_length = 3 },
            }),
            snippet = {
              expand = function(args)
                luasnip.lsp_expand(args.body)
              end,
            },
            window = {
              completion = cmp.config.window.bordered(),
              documentation = cmp.config.window.bordered(),
            },
          }, opts)
        end
      '';
    }
  ];
  config.extraConfigLua = /* lua */ ''
    do
      -- diagnostic
      local sign = function(opts)
        if type(opts.text) ~= "string" then
          return
        end

        vim.fn.sign_define(opts.name, {
          texthl = opts.name,
          text = opts.text,
          numhl = "",
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

      -- Diagnostic keymaps
      vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
      vim.keymap.set("n", "]d", vim.diagnostic.goto_next)
      vim.keymap.set("n", "<leader>gl", vim.diagnostic.open_float)
      vim.keymap.set("n", "<leader>gL", vim.diagnostic.setloclist)
    end
  '';
  config.autoGroups.lspConfig = { };
  config.autoCmd = [
    {
      group = "lspConfig";
      event = "LspAttach";
      pattern = "*";
      callback =
        let
          rawMapping = desc: key: fn: ''
            vim.keymap.set("n", "${key}", ${fn}, { buffer = bufnr, desc = "${desc}" }) 
          '';
          mapping = desc: key: cmd: (rawMapping desc key "vim.lsp.buf.${cmd}");
        in
        helpers.mkRaw /* lua */ ''
          function(opts)
            local bufnr = opts.buf
            local client = vim.lsp.get_client_by_id(opts.data.client_id)

            if client.supports_method("textDocument/documentSymbol") then
              require("nvim-navic").attach(client, bufnr)
            end

            if client.supports_method("textDocument/inlayHint") then
              vim.lsp.inlay_hint.enable(true, { bufnr })
              ${rawMapping "Hints toggle" "<leader>ht" /* lua */ ''
                function()
                  vim.lsp.inlay_hint.enable(
                      not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }),
                      { bufnr = 0 })
                end
              ''}
            end

            -- Enable completion triggered by <c-x><c-o>
            vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

            -- Add mappings
            ${mapping "Rename" "<leader>rn" "rename"}
            ${mapping "Code action" "<leader>ca" "code_action"}

            ${mapping "Goto definition" "gd" "definition"}
            ${mapping "Goto declaration" "gD" "declaration"}
            ${mapping "Goto implementation" "gi" "implementation"}
            ${mapping "Goto type definition" "gtd" "type_definition"}
            ${mapping "Goto references" "gr" "references"}

            ${mapping "Hover documentation" "K" "hover"}
            ${mapping "Signature documentation" "<C-k>" "signature_help"}

            ${mapping "Workspace add folder" "<leader>wa" "add_workspace_folder"}
            ${mapping "Workspace remove folder" "<leader>wr" "remove_workspace_folder"}
            ${rawMapping "Workspace list folders" "<leader>wl" /* lua */ ''
              function()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
              end
            ''}

            ${mapping "Format" "<leader>f" "format"}

            ${rawMapping "Format and save" "<leader>F" /* lua */ ''
              function()
                vim.lsp.buf.format()
                vim.api.nvim_command("write")
              end
            ''}
          end
        '';
    }
  ];
}
