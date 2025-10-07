{ pkgs, helpers, ... }:

let
  inherit (import ../lib.nix { inherit helpers; }) mkLazyKeys;
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = vim-fugitive;
      cmd = "Git";
      keys = mkLazyKeys [
        {
          lhs = "<leader>gs";
          rhs = "<cmd>Git<cr>";
          desc = "Git status";
        }
      ];
    }
    {
      pkg = gitsigns-nvim;
      event = [
        "BufReadPre"
        "BufNewFile"
      ];
      config = true;
    }
    {
      pkg = lazygit-nvim;
      lazy = true;
      cmd = [
        "LazyGit"
        "LazyGitConfig"
        "LazyGitCurrentFile"
        "LazyGitFilter"
        "LazyGitFilterCurrentFile"
      ];
      dependencies = [ plenary-nvim ];
      keys = mkLazyKeys [
        {
          lhs = "<leader>lg";
          rhs = "<cmd>LazyGit<cr>";
          desc = "Lazy Git";
        }
      ];
    }
  ];
}
