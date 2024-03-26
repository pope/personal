let
  lua = x: x;
in
{
  config.plugins = {
    bufferline.enable = true;
    comment.enable = true;
    cmp = {
      enable = true;
      autoEnableSources = true;
      settings = {
        formatting.__raw = lua ''
          {
            format = require("lspkind").cmp_format({
              mode = "symbol_text",
              maxwidth = 60,
              show_labelDetails = true,
            }),
          }
        '';
        mapping.__raw = lua ''
          cmp.mapping.preset.insert({
            ["<Tab>"] = cmp.mapping(function(fallback)
              if cmp.visible() then
                cmp.select_next_item()
              elseif require("luasnip").expand_or_jumpable() then
                require("luasnip").expand_or_jump()
              else
                fallback()
              end
            end, { "i", "s" }),
            ["<S-Tab>"] = cmp.mapping(function(fallback)
              if cmp.visible() then
                cmp.select_prev_item()
              elseif require("luasnip").jumpable(-1) then
                require("luasnip").jump(-1)
              else
                fallback()
              end
            end, { "i", "s" }),
            ["<C-u>"] = cmp.mapping(function(fallback)
              if require("luasnip").choice_active() then
                require("luasnip").next_choice()
              else
                fallback()
              end
            end),
            ["<C-p>"] = cmp.mapping.select_prev_item(),
            ["<C-n>"] = cmp.mapping.select_next_item(),
            ["<C-b>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),
            ["<C-Space>"] = cmp.mapping.complete(),
            ["<C-e>"] = cmp.mapping.close(),
            ["<CR>"] = cmp.mapping.confirm {
              behavior = cmp.ConfirmBehavior.Insert,
              select = true,
            },
          })
        '';
        snippet.expand = lua ''
          function(args) require("luasnip").lsp_expand(args.body) end
        '';
        sources = [
          { name = "nvim_lsp"; groupIndex = 1; }
          { name = "luasnip"; groupIndex = 1; }
          { name = "nvim_lua"; groupIndex = 1; }
          { name = "calc"; groupIndex = 2; }
          { name = "emoji"; groupIndex = 2; }
          { name = "path"; groupIndex = 2; }
          { name = "buffer"; groupIndex = 3; }
        ];
        window = {
          completion.__raw = lua "cmp.config.window.bordered()";
          documentation.__raw = lua "cmp.config.window.bordered()";
        };
      };
    };
    dashboard.enable = true;
    fidget.enable = false;
    friendly-snippets.enable = true;
    fugitive.enable = true;
    gitsigns = {
      enable = true;
      settings.trouble = true;
    };
    indent-blankline = {
      enable = true;
      settings = {
        exclude.filetypes = [
          "TelescopePrompt"
          "TelescopeResults"
          "\'\'"
          "checkhealth"
          "dashboard"
          "fugitive"
          "gitcommit"
          "help"
          "lspinfo"
          "man"
          "packer"
        ];
        indent = {
          char = "┊";
          highlight = "NonText";
        };
        scope.enabled = true;
        whitespace = {
          highlight = "NonText";
          # TODO(pope): See what happened to this attribute.
          # removeBlanklineTrail = false;
        };
      };
    };
    lualine = {
      enable = true;
      componentSeparators.left = "";
      componentSeparators.right = "";
      disabledFiletypes.statusline = [ "NvimTree" ];
      extensions = [ "fugitive" "nvim-tree" "toggleterm" ];
      sectionSeparators.left = "";
      sectionSeparators.right = "";
      sections.lualine_c = [
        {
          name = "filename";
        }
        {
          name = "navic";
          extraConfig = {
            navic_opts = null;
          };
        }
      ];
    };
    luasnip = {
      enable = true;
      extraConfig = { update_events = "TextChanged,TextChangedI"; };
    };
    lsp = {
      enable = true;
      keymaps = {
        diagnostic = {
          "[d" = "goto_prev";
          "]d" = "goto_next";
          "<leader>gl" = "open_float";
          "<leader>gL" = "setloclist";
        };
        lspBuf = {
          "<leader>rn" = "rename";
          "<leader>ca" = "code_action";
          "gd" = "definition";
          "gD" = "declaration";
          "gi" = "implementation";
          "gt" = "type_definition";
          "gr" = "references";
          "K" = "hover";
          "<C-k>" = "signature_help";
          "<leader>f" = "format";

          "<leader>wa" = "add_workspace_folder";
          "<leader>wr" = "remove_workspace_folder";
          "<leader>wl" = "list_workspace_folders";
        };
      };
      servers = {
        bashls.enable = true;
        clangd.enable = true;
        cssls.enable = true;
        dartls.enable = true;
        gopls = {
          enable = true;
          extraOptions = {
            analyses.unusedparams = true;
            staticcheck = true;
          };
        };
        html.enable = true;
        jsonls.enable = true;
        kotlin-language-server.enable = true;
        lua-ls.enable = true;
        nil_ls.enable = true;
        phpactor.enable = true;
        pyright.enable = true;
        rust-analyzer = {
          enable = true;
          installRustc = false;
          installCargo = false;
          settings.files.excludeDirs = [ ".direnv" ".devenv" ];
        };
        tsserver = { };
        typos-lsp = {
          enable = true;
          extraOptions = {
            init_options = {
              diagnosticSeverity = "Warning";
            };
          };
        };
      };
    };
    lspkind = {
      enable = true;
      mode = "symbol";
      extraOptions.maxwidth = 50;
    };
    navic.enable = true;
    neoscroll.enable = true;
    noice = {
      enable = true;
      lsp.override = {
        "vim.lsp.util.convert_input_to_markdown_lines" = true;
        "vim.lsp.util.stylize_markdown" = true;
        "cmp.entry.get_documentation" = true;
      };
      presets = {
        bottom_search = true;
        command_palette = true;
        long_message_to_split = true;
        inc_rename = true;
        lsp_doc_border = false;
      };
    };
    notify.enable = true;
    nvim-autopairs = {
      enable = true;
      checkTs = true;
    };
    nvim-colorizer = {
      enable = true;
      userDefaultOptions.mode = "virtualtext";
    };
    nvim-osc52.enable = true;
    nvim-tree = {
      enable = true;
      git.enable = true;
      renderer.indentMarkers.enable = true;
      updateFocusedFile.enable = true;
      view.width = 32;
      syncRootWithCwd = true;
    };
    nvim-ufo = {
      enable = true;
      enableGetFoldVirtText = true;
    };
    rainbow-delimiters.enable = true;
    surround.enable = true;
    telescope = {
      enable = true;
      extensions = {
        file_browser.enable = true;
        # frecency.enable = true;
        fzf-native.enable = true;
        fzy-native.enable = false;
        media_files.enable = true;
        undo.enable = true;
        ui-select.enable = true;
      };
    };
    toggleterm.enable = true;
    treesitter = {
      enable = true;
      folding = true;
      incrementalSelection = {
        enable = true;
        keymaps = {
          initSelection = "<C-space>";
          nodeDecremental = "<bs>";
          nodeIncremental = "<C-space>";
          scopeIncremental = "<C-s>";
        };
      };
      indent = true;
    };
    treesitter-context.enable = true;
    treesitter-textobjects = {
      enable = true;
      move = {
        enable = true;
        gotoNextStart = {
          "]f" = "@call.outer";
          "]m" = "@function.outer";
          "]c" = "@class.outer";
          "]i" = "@conditional.outer";
          "]l" = "@loop.outer";
        };
        gotoNextEnd = {
          "]F" = "@call.outer";
          "]M" = "@function.outer";
          "]C" = "@class.outer";
          "]I" = "@conditional.outer";
          "]L" = "@loop.outer";
        };
        gotoPreviousStart = {
          "[f" = "@call.outer";
          "[m" = "@function.outer";
          "[c" = "@class.outer";
          "[i" = "@conditional.outer";
          "[l" = "@loop.outer";
        };
        gotoPreviousEnd = {
          "[F" = "@call.outer";
          "[M" = "@function.outer";
          "[C" = "@class.outer";
          "[I" = "@conditional.outer";
          "[L" = "@loop.outer";
        };
      };
      select = {
        enable = true;
        lookahead = true;
        keymaps = {
          "a=" = "@assignment.outer";
          "i=" = "@assignment.inner";
          "l=" = "@assignment.lhs";
          "r=" = "@assignment.rhs";

          # works for javascript/typescript files (custom capture I created in
          # after/queries/ecma/textobjects.scm)
          "a:" = "@property.outer";
          "i:" = "@property.inner";
          "l:" = "@property.lhs";
          "r:" = "@property.rhs";

          "ap" = "@parameter.outer";
          "ip" = "@parameter.inner";
          "ai" = "@condition.outer";
          "ii" = "@condition.inner";
          "af" = "@call.outer";
          "if" = "@call.outer";
          "am" = "@function.inner";
          "im" = "@function.inner";
          "ac" = "@class.inner";
          "ic" = "@class.inner";
        };
      };
      swap = {
        enable = true;
        swapNext = {
          "<leader>na" = "@parameter.inner";
          "<leader>n:" = "@property.outer";
          "<leader>nm" = "@function.outer";
        };
        swapPrevious = {
          "<leader>pa" = "@parameter.inner";
          "<leader>p:" = "@property.outer";
          "<leader>pm" = "@function.outer";
        };
      };
    };
    trouble = {
      enable = true;
      settings.auto_close = true;
    };
    which-key.enable = true;
  };
}
