{ pkgs, helpers, ... }:

let
  inherit (import ./lib.nix { inherit helpers; }) mkLazyKeys;
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = guess-indent-nvim;
      event = [ "BufReadPost" "BufNewFile" ];
      config = true;
    }
    {
      pkg = comment-nvim;
      config = true;
      keys = mkLazyKeys [
        { lhs = "gb"; mode = "v"; desc = "Toggle blockwise comment"; }
        { lhs = "gc"; mode = "v"; desc = "Toggle linewise comment"; }
        { lhs = "gbb"; desc = "Toggle blockwise comment"; }
        { lhs = "gcc"; desc = "Toggle linewise comment"; }
      ];
    }
    {
      pkg = nvim-osc52;
      opts = {
        max_length = 0;
        silent = false;
        trim = false;
      };
      keys = mkLazyKeys [
        {
          lhs = "<leader>y";
          rhs = helpers.mkRaw /* lua */ "function() require('osc52').copy_visual() end";
          mode = "v";
        }
      ];
    }
    {
      pkg = nvim-autopairs;
      dependencies = [ nvim-treesitter ];
      event = [ "InsertEnter" ];
      opts = {
        break_undo = false;
        check_ts = true; # enable tree-sitter
        ts_config = {
          lua = [ "string" ];
          javascript = [ "string" "template_string" ];
        };
      };
    }
    {
      pkg = nvim-surround;
      event = [ "InsertEnter" ];
      config = true;
    }
  ];
}
