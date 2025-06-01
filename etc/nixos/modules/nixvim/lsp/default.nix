{ pkgs, helpers, ... }:

# Shout outs to
#   - https://lugh.ch/switching-to-neovim-native-lsp.html
#   - https://neovim.io/doc/user/lsp.html
#   - https://github.com/neovim/nvim-lspconfig/tree/master/lsp
#   - (Original jump-point) https://vonheikemen.github.io/devlog/tools/setup-nvim-lspconfig-plus-nvim-cmp/
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = nvim-navic;
      lazy = true;
      config = true;
    }
    {
      pkg = blink-cmp;
      dependencies = [ friendly-snippets ];
      opts = {
        appearance.nerd_font_variant = "mono";
        completion.documentation.auto_show = true;
        fuzzy.implementation = "prefer_rust_with_warning";
        keymap.preset = "default";
        sources.default = [ "lsp" "path" "snippets" "buffer" ];
      };
      event = [ "BufReadPre" "BufNewFile" ];
    }
  ];
  config.extraConfigLua = /* lua */ ''
    do
      local server = {
        clangd = {
          cmd = { "clangd" },
          filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
          root_markers = {
            ".clang-format",
            ".clang-tidy",
            ".clangd",
            ".git",
            "compile_commands.json",
            "compile_flags.txt",
            "configure.ac", -- AutoTools
          },
          capabilities = {
            textDocument = {
              completion = {
                editsNearCursor = true,
              },
            },
            offsetEncoding = { "utf-8", "utf-16" },
          },
          on_attach = function(client, bufnr)
            vim.api.nvim_buf_create_user_command(0, "LspClangdSwitchSourceHeader", function()
              local params = vim.lsp.util.make_text_document_params(bufnr)
              client.request("textDocument/switchSourceHeader", params, function(err, result)
                if err then
                  error(tostring(err))
                end
                if not result then
                  vim.notify("corresponding file cannot be determined")
                  return
                end
                vim.cmd.edit(vim.uri_to_fname(result))
              end, bufnr)
            end, { desc = "Switch between source/header" })

            vim.api.nvim_buf_create_user_command(0, "LspClangdShowSymbolInfo", function()
              local win = vim.api.nvim_get_current_win()
              local params = vim.lsp.util.make_position_params(win, client.offset_encoding)
              client.request("textDocument/symbolInfo", params, function(err, res)
                if err or #res == 0 then
                  -- Clangd always returns an error, there is not reason to parse it
                  return
                end
                local container = string.format("container: %s", res[1].containerName) ---@type string
                local name = string.format("name: %s", res[1].name) ---@type string
                vim.lsp.util.open_floating_preview({ name, container }, "", {
                  height = 2,
                  width = math.max(string.len(name), string.len(container)),
                  focusable = false,
                  focus = false,
                  border = "single",
                  title = "Symbol Info",
                })
              end, bufnr)
            end, { desc = "Show symbol info" })
          end,
        },
        cmake = {
          cmd = { "cmake-language-server" },
          filetypes = { "cmake" },
          root_markers = { "CMakePresets.json", "CTestConfig.cmake", ".git", "build", "cmake" },
          init_options = {
            buildDirectory = "build",
          },
        },
        dart = {
          cmd = { "dart", "language-server", "--protocol=lsp" },
          filetypes = { "dart" },
          root_markers = { "pubspec.yaml" },
          init_options = {
            closingLabels = true,
            flutterOutline = true,
            onlyAnalyzeProjectsWithOpenFiles = true,
            outline = true,
            suggestFromUnimportedLibraries = true,
          },
          settings = {
            dart = {
              completeFunctionCalls = true,
              showTodos = true,
            },
          },
        },
        gdscript = {
          cmd = vim.lsp.rpc.connect("127.0.0.1", 6005),
          filetypes = { "gd", "gdscript", "gdscript3" },
          root_markers = { "project.godot", ".git" },
        },
        glsl_analyzer = {
          cmd = { "glsl_analyzer" },
          filetypes = { "glsl", "vert", "tesc", "tese", "frag", "geom", "comp" },
          root_markers = { ".git" },
          capabilities = {},
        },
        nixd = {
          cmd = { "nixd" },
          filetypes = { "nix" },
          root_markers = { "flake.nix", "git" },
          settings = {
            nixd = {
              nixpkgs = {
                expr = "import <nixpkgs> { }",
              },
              formatting = {
                command = { "nixpkgs-fmt" },
              },
              options = {
                nixos = {
                  expr = 'let configs = (builtins.getFlake ((builtins.getEnv "HOME") + "/Code/personal/etc/nixos")).nixosConfigurations; in (builtins.head (builtins.attrValues configs)).options',
                },
                home_manager = {
                  expr = 'let configs = (builtins.getFlake ((builtins.getEnv "HOME") + "/Code/personal/etc/nixos")).homeConfigurations; in (builtins.head (builtins.attrValues configs)).options',
                },
              },
            },
          },
        },
        phpactor = {
          cmd = { "phpactor", "language-server" },
          filetypes = { "php" },
          root_dir = function(bufnr, on_dir)
            local fname = vim.api.nvim_buf_get_name(bufnr)
            local cwd = assert(vim.uv.cwd())
            local root = vim.fs.root(fname, { "composer.json", ".git", ".phpactor.json", ".phpactor.yml" })

            -- prefer cwd if root is a descendant
            on_dir(root and vim.fs.relpath(cwd, root) and cwd)
          end,
        },
        pyright = {
          cmd = { "pyright-langserver", "--stdio" },
          filetypes = { "python" },
          root_markers = {
            ".git",
            "Pipfile",
            "pyproject.toml",
            "pyrightconfig.json",
            "requirements.txt",
            "setup.cfg",
            "setup.py",
          },
          settings = {
            python = {
              analysis = {
                autoSearchPaths = true,
                useLibraryCodeForTypes = true,
                diagnosticMode = "openFilesOnly",
              },
            },
            pyright = {
              exclude = { ".venv" },
              venvPath = ".",
              venv = ".venv",
            },
          },
          on_attach = function(client, bufnr)
            vim.api.nvim_buf_create_user_command(bufnr, "LspPyrightOrganizeImports", function()
              client:exec_cmd({
                command = "pyright.organizeimports",
                arguments = { vim.uri_from_bufnr(bufnr) },
              })
            end, {
              desc = "Organize Imports",
            })
          end,
        },
        rust_analyzer = {
          cmd = { "rust-analyzer" },
          filetypes = { "rust" },
          root_dir = function(bufnr, on_dir)
            local is_library = function(fname)
              local user_home = vim.fs.normalize(vim.env.HOME)
              local cargo_home = os.getenv("CARGO_HOME") or user_home .. "/.cargo"
              local registry = cargo_home .. "/registry/src"
              local git_registry = cargo_home .. "/git/checkouts"

              local rustup_home = os.getenv("RUSTUP_HOME") or user_home .. "/.rustup"
              local toolchains = rustup_home .. "/toolchains"

              for _, item in ipairs({ toolchains, registry, git_registry }) do
                if vim.fs.relpath(item, fname) then
                  local clients = vim.lsp.get_clients({ name = "rust_analyzer" })
                  return #clients > 0 and clients[#clients].config.root_dir or nil
                end
              end
            end

            local fname = vim.api.nvim_buf_get_name(bufnr)
            local reused_dir = is_library(fname)
            if reused_dir then
              on_dir(reused_dir)
              return
            end

            local cargo_crate_dir = vim.fs.root(fname, { "Cargo.toml" })
            local cargo_workspace_root

            if cargo_crate_dir == nil then
              on_dir(
                vim.fs.root(fname, { "rust-project.json" })
                  or vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
              )
              return
            end

            local cmd = {
              "cargo",
              "metadata",
              "--no-deps",
              "--format-version",
              "1",
              "--manifest-path",
              cargo_crate_dir .. "/Cargo.toml",
            }

            vim.system(cmd, { text = true }, function(output)
              if output.code == 0 then
                if output.stdout then
                  local result = vim.json.decode(output.stdout)
                  if result["workspace_root"] then
                    cargo_workspace_root = vim.fs.normalize(result["workspace_root"])
                  end
                end

                on_dir(cargo_workspace_root or cargo_crate_dir)
              else
                vim.schedule(function()
                  vim.notify(("[rust_analyzer] cmd failed with code %d: %s\n%s"):format(output.code, cmd, output.stderr))
                end)
              end
            end)
          end,
          capabilities = {
            experimental = {
              serverStatusNotification = true,
            },
          },
          settings = {
            ["rust-analyzer"] = {
              files = {
                excludeDirs = { ".direnv", ".devenv" },
              },
            },
          },
          before_init = function(init_params, config)
            -- See https://github.com/rust-lang/rust-analyzer/blob/eb5da56d839ae0a9e9f50774fa3eb78eb0964550/docs/dev/lsp-extensions.md?plain=1#L26
            init_params.initializationOptions = config.settings["rust-analyzer"]
          end,
          on_attach = function(client, bufnr)
            vim.api.nvim_buf_create_user_command(0, "LspCargoReload", function()
              client.request("rust-analyzer/reloadWorkspace", nil, function(err)
                if err then
                  error(tostring(err))
                end
                vim.notify("Cargo workspace reloaded")
              end, 0)
            end, { desc = "Reload current cargo workspace" })
          end,
        },
        lua_ls = {
          cmd = { "lua-language-server" },
          filetypes = { "lua" },
          root_markers = {
            ".git",
            ".luacheckrc",
            ".luarc.json",
            ".luarc.jsonc",
            ".stylua.toml",
            "selene.toml",
            "selene.yml",
            "stylua.toml",
          },
          settings = {
            Lua = {
              diagnostics = {
                globals = { "vim" },
              },
              workspace = { checkThirdParty = false },
              telemetry = { enable = false },
            },
          },
        },
        gopls = {
          cmd = { "gopls" },
          root_markers = { "go.work", "go.mod", ".git" },
          filetypes = { "go", "gomod", "gowork", "gotmpl" },
          settings = {
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
        },
        ts_ls = {
          cmd = { "typescript-language-server", "--stdio" },
          init_options = { hostInfo = "neovim" },
          filetypes = {
            "javascript",
            "javascript.jsx",
            "javascriptreact",
            "typescript",
            "typescript.tsx",
            "typescriptreact",
          },
          root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
          handlers = {
            -- handle rename request for certain code actions like extracting functions / types
            ["_typescript.rename"] = function(_, result, ctx)
              local client = assert(vim.lsp.get_client_by_id(ctx.client_id))
              vim.lsp.util.show_document({
                uri = result.textDocument.uri,
                range = {
                  start = result.position,
                  ["end"] = result.position,
                },
              }, client.offset_encoding)
              vim.lsp.buf.rename()
              return vim.NIL
            end,
          },
          on_attach = function(client)
            -- ts_ls provides `source.*` code actions that apply to the whole file. These only appear in
            -- `vim.lsp.buf.code_action()` if specified in `context.only`.
            vim.api.nvim_buf_create_user_command(0, "LspTypescriptSourceAction", function()
              local source_actions = vim.tbl_filter(function(action)
                return vim.startswith(action, "source.")
              end, client.server_capabilities.codeActionProvider.codeActionKinds)

              vim.lsp.buf.code_action({
                context = {
                  only = source_actions,
                },
              })
            end, {})
          end,
        },
        zls = {
          cmd = { "zls" },
          filetypes = { "zig", "zir" },
          root_markers = { "zls.json", "build.zig", ".git" },
          workspace_required = false,
          settings = {
            zls = {
              enable_snippets = true,
              warn_style = true,
            },
          },
        },
      }

      local lsps = {}
      for name, settings in pairs(server) do
        vim.lsp.config[name] = settings
        table.insert(lsps, name)
      end
      vim.lsp.enable(lsps)
    end

    vim.diagnostic.config({
      float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
      },
      severity_sort = true,
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = "✘",
          [vim.diagnostic.severity.WARN] = "▲",
          [vim.diagnostic.severity.HINT] = "⚑",
          [vim.diagnostic.severity.INFO] = "",
        },
      },
      underline = true,
      update_in_insert = false,
      virtual_lines = true,
    })
  '';
  config.autoGroups = {
    lspConfig = { };
    lspSave = { clear = true; };
  };
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
            local client = assert(vim.lsp.get_client_by_id(opts.data.client_id))

            if client:supports_method("textDocument/documentSymbol") then
              require("nvim-navic").attach(client, bufnr)
            end

            if client:supports_method("textDocument/inlayHint") then
              vim.lsp.inlay_hint.enable(true, { bufnr })
              ${rawMapping "Hints toggle" "<leader>ht" /* lua */ ''
                function()
                  vim.lsp.inlay_hint.enable(
                      not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }),
                      { bufnr = 0 })
                end
              ''}
            end

            if client:supports_method("textDocument/completion") then
              -- vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
            end

            if
                not client:supports_method("textDocument/willSaveWaitUntil")
                and client:supports_method("textDocument/formatting")
            then
              ${mapping "Format" "<leader>f" "format"}

              vim.api.nvim_create_autocmd("BufWritePre", {
                group = "lspSave", 
                buffer = bufnr,
                callback = function()
                  vim.lsp.buf.format({ bufnr = bufnr, id = client.id, timeout_ms = 1000 })
                end,
              })
            end

            if client:supports_method("textDocument/implementation") then
              -- Add additional mappings
              ${mapping "Goto definition" "gd" "definition"}
              ${mapping "Goto declaration" "gD" "declaration"}
              ${mapping "Goto type definition" "gtd" "type_definition"}
            end

            -- Enable completion triggered by <c-x><c-o>
            vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
          end
        '';
    }
  ];
}
