{ pkgs, helpers, ... }:

let
  inherit (import ../lib.nix { inherit helpers; }) mkLazyKeys;
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = nvim-tree-lua;
      dependencies = [ nvim-web-devicons ];
      cmd = [ "NvimTreeToggle" ];
      keys = mkLazyKeys [
        {
          lhs = "<leader>pv";
          rhs = "<cmd>NvimTreeToggle<cr>";
          desc = "Toggle project view";
        }
      ];
      opts.view.width = 32;
      opts.renderer.indent_markers.enable = true;
      opts.sync_root_with_cwd = true;
      opts.update_focused_file.enable = true;
    }
    {
      pkg = quicker-nvim;
      event = [ "FileType qf" ];
      keys = mkLazyKeys [
        {
          lhs = "<leader>q";
          rhs = "<cmd>lua require('quicker').toggle()<CR>";
          desc = "Toggle quickfix";
        }
        {
          lhs = "<leader>l";
          rhs = "<cmd>lua require('quicker').toggle({ loclist = true })<CR>";
          desc = "Toggle loclist";
        }
        {
          lhs = "<M-l>";
          rhs = "<cmd>cnext<CR>";
          desc = "Quickfix next";
        }
        {
          lhs = "<M-h>";
          rhs = "<cmd>cprev<CR>";
          desc = "Quickfix previous";
        }
      ];
      opts.keys = mkLazyKeys [
        {
          lhs = ">";
          rhs = "<cmd>lua require('quicker').expand()<CR>";
          desc = "Expand quicker content";
        }
        {
          lhs = "<";
          rhs = "<cmd>lua require('quicker').collapse()<CR>";
          desc = "Collapse quicker content";
        }
      ];
    }
    {
      pkg = neoscroll-nvim;
      event = [ "VeryLazy" ];
      config = true;
    }
    {
      pkg = zen-mode-nvim;
      cmd = [ "ZenMode" ];
      config = true;
    }
    {
      pkg = nvim-colorizer-lua;
      event = [ "BufReadPost" "BufNewFile" ];
      opts.user_default_options.mode = "virtualtext";
    }
    {
      pkg = indent-blankline-nvim;
      event = [ "BufReadPost" "BufNewFile" ];
      main = "ibl";
      opts.indent.char = "┊";
      opts.whitespace.remove_blankline_trail = false;
      opts.scope.enabled = false;
      opts.exclude.filetypes = [
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
    }
    {
      pkg = noice-nvim;
      dependencies = [ nui-nvim nvim-notify ];
      event = [ "VeryLazy" ];
      opts.lsp.progress.enabled = true;
      opts.lsp.override = {
        "vim.lsp.util.convert_input_to_markdown_lines" = true;
        "vim.lsp.util.stylize_markdown" = true;
        "cmp.entry.get_documentation" = true;
      };
      keys = mkLazyKeys [
        {
          lhs = "<leader><Esc>";
          rhs = "<cmd>NoiceDismiss<cr>";
          desc = "Dismiss notifications";
        }
      ];
    }
    {
      pkg = nvim-notify;
      lazy = true;
      config = true;
    }
  ];
}
