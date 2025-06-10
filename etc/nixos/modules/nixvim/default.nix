{ pkgs, helpers, ... }:

{
  imports = [
    ./colorschemes
    ./dashboard
    ./debugging
    ./editing
    ./folding
    ./ftplugins
    ./git
    ./lsp
    ./lualine
    ./options
    ./telescope
    ./treesitter
    ./ui
  ];

  config = {
    extraPackages = with pkgs; [
      lua-language-server
      nixd
      nixpkgs-fmt
      tree-sitter
      wget
    ];

    editorconfig.enable = true;

    plugins.lazy.enable = true;
    plugins.lazy.plugins = with pkgs.vimPlugins; [
      {
        pkg = vim-protobuf;
        ft = [ "proto" ];
      }
      {
        pkg = which-key-nvim;
        event = [ "VeryLazy" ];
        dependencies = [ nvim-web-devicons mini-nvim ];
      }
      {
        pkg = toggleterm-nvim;
        cmd = [ "ToggleTerm" ];
        config = true;
      }
    ];

    keymaps = [
      {
        mode = "n";
        key = "<Esc>";
        action = "<cmd>nohlsearch<CR>";
      }
    ];

    autoGroups.restoreCursorPosition = { };
    autoCmd =
      [
        {
          group = "restoreCursorPosition";
          event = "BufReadPost";
          pattern = "*";
          callback = helpers.mkRaw /* lua */ ''
            function()
              if vim.fn.line "'\"" > 0 and vim.fn.line "'\"" <= vim.fn.line "$" then
                vim.cmd [[execute "normal! g'\""]]
              end
            end
          '';
        }
      ];
  };
}
