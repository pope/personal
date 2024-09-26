{ pkgs, helpers, ... }:

{
  imports = [
    ./colorschemes.nix
    ./dashboard.nix
    ./debugging.nix
    ./editing.nix
    ./folding.nix
    ./ftplugins.nix
    ./git.nix
    ./lsp.nix
    ./lualine.nix
    ./options.nix
    ./telescope.nix
    ./treesitter.nix
    ./ui.nix
  ];

  config = {
    extraPackages = with pkgs; [
      tree-sitter
    ];

    editorconfig.enable = true;

    plugins.lazy.enable = true;
    plugins.lazy.plugins = with pkgs.vimPlugins; [
      {
        pkg = vim-protobuf;
        ft = [ "proto" ];
      }
      {
        pkg = vim-startuptime;
        cmd = "StartupTime";
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
