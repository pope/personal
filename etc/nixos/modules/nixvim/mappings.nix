let
  searchmap = desc: key: fn: {
    mode = "n";
    key = "<leader>${key}";
    action = "require('telescope.builtin').${fn}";
    lua = true;
    options.desc = desc;
  };
  textobjectmap = desc: key: fn: {
    inherit key;
    mode = [ "n" "x" "o" ];
    action = "require('nvim-treesitter.textobjects.repeatable_move').${fn}";
    lua = true;
    options.desc = desc;
  };
  troublemap = desc: key: fn: {
    inherit key;
    mode = "n";
    action =
      let
        arg = if (fn == null) then "" else "'${fn}'";
      in
      "function() require('trouble').toggle(${arg}) end";
    lua = true;
    options.desc = desc;
  };
in
{
  config.keymaps = [
    {
      mode = "n";
      key = "<Esc>";
      action = "<cmd>nohlsearch<CR>";
    }

    {
      mode = "v";
      key = "<leader>y";
      action = "require('osc52').copy_visual";
      lua = true;
      options.desc = "Yank with osc52";
    }

    {
      mode = "n";
      key = "<leader>pv";
      action = "<cmd>NvimTreeToggle<cr>";
      options.desc = "Project tree view";
    }

    # SCN
    {
      mode = "n";
      key = "<leader>gs";
      action = "<cmd>Git<cr>";
      options.desc = "Git status";
    }

    # Telescope
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

    # Treesitter TextObject
    (textobjectmap "Repeat last move" ";" "repeat_last_move")
    (textobjectmap "Repeat last move opposite" "," "repeat_last_move_opposite")

    (textobjectmap "Forward to the right" "f" "builtin_f")
    (textobjectmap "Forward to the left" "F" "builtin_F")
    (textobjectmap "Till before" "t" "builtin_t")
    (textobjectmap "Till after" "T" "builtin_T")

    # Trouble
    (troublemap "Toggle trouble" "<leader>xx" null)
    (troublemap "Toggle workspace trouble" "<leader>xw" "workspace_diagnostics")
    (troublemap "Toggle document trouble" "<leader>xd" "document_diagnostics")
    (troublemap "Toggle quickfix trouble" "<leader>xq" "quickfix")
    (troublemap "Toggle loclist trouble" "<leader>xl" "loclist")
    (troublemap "Toggle lsp references trouble" "gR" "lsp_references")
  ];
}
