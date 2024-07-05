{ pkgs, helpers, ... }:

let
  inherit (import ./lib.nix { inherit helpers; }) mkLazyKeys;
  searchmap = desc: key: cmd: {
    lhs = "<leader>${key}";
    rhs = "<cmd>Telescope ${cmd}<cr>";
    inherit desc;
  };
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = telescope-nvim;
      dependencies = [
        noice-nvim
        plenary-nvim
        telescope-file-browser-nvim
        telescope-live-grep-args-nvim
        telescope-media-files-nvim
        telescope-ui-select-nvim
        telescope-undo-nvim
      ];
      cmd = [ "Telescope" ];
      keys = mkLazyKeys [
        (searchmap "Search recently opened files" "<space>" "oldfiles")
        (searchmap "Resume telescope" "r" "resume")
        (searchmap "Search buffers" "sb" "buffers")
        (searchmap "Search files" "sf" "find_files")
        (searchmap "Search help" "sh" "help_tags")
        (searchmap "Search current word" "sw" "grep_string")
        (searchmap "Search via ripgrep" "sr" "live_grep")
        (searchmap "Search current buffer" "sc" "current_buffer_fuzzy_find")
        (searchmap "Search diagnostics" "sd" "diagnostics")
        (searchmap "Search quickfix" "sq" "quickfix")
        (searchmap "Search jumplist" "sj" "jumplist")
        (searchmap "Search marks" "sm" "marks")
        (searchmap "Search git status" "sgs" "git_status")
        (searchmap "Search document symbols" "sds" "lsp_document_symbols")
        (searchmap "Search document references" "sdr" "lsp_references")
        (searchmap "Search workspace symbols" "sdw" "lsp_dynamic_workspace_symbols")
        (searchmap "Search undo tree" "su" "undo")
      ];
      config = /* lua */ ''
        function (_, opts)
          require("telescope").setup(opts)
          require("telescope").load_extension("file_browser")
          require("telescope").load_extension("live_grep_args")
          require("telescope").load_extension("media_files")
          require("telescope").load_extension("ui-select")
          require("telescope").load_extension("undo")
          require("telescope").load_extension("noice")
        end
      '';
    }
  ];
}
