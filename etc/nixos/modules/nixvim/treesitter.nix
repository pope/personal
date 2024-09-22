{ pkgs, ... }:

{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = nvim-treesitter;
      dependencies = [
        nvim-treesitter-textobjects
        rainbow-delimiters-nvim
      ];
      event = [ "BufReadPost" "BufNewFile" ];
      opts.highlight = {
        enable = true;
        additional_vim_regex_highlighting = false;
      };
      opts.indent.enable = true;
      opts.incremental_selection = {
        enable = true;
        keymaps = {
          init_selection = "<C-space>";
          node_incremental = "<C-space>";
          scope_incremental = "<C-s>";
          node_decremental = "<bs>";
        };
      };
      opts.textobjects.move = {
        enable = true;
        set_jumps = true; # whether to set jumps in the jumplist.
        goto_next_start = {
          "]f" = { query = "@call.outer"; desc = "Next function call start"; };
          "]m" = { query = "@function.outer"; desc = "Next method/function def start"; };
          "]c" = { query = "@class.outer"; desc = "Next class start"; };
          "]i" = { query = "@conditional.outer"; desc = "Next conditional start"; };
          "]l" = { query = "@loop.outer"; desc = "Next loop start"; };

          # You can pass a query group to use query from
          # `queries/<lang>/<query_group>.scm file in your runtime path.
          # Below example nvim-treesitter's `locals.scm` and `folds.scm`.
          # They also provide highlights.scm and indent.scm.
          "]s" = { query = "@scope"; query_group = "locals"; desc = "Next scope"; };
          "]z" = { query = "@fold"; query_group = "folds"; desc = "Next fold"; };
        };
        goto_next_end = {
          "]F" = { query = "@call.outer"; desc = "Next function call end"; };
          "]M" = { query = "@function.outer"; desc = "Next method/function def end"; };
          "]C" = { query = "@class.outer"; desc = "Next class end"; };
          "]I" = { query = "@conditional.outer"; desc = "Next conditional end"; };
          "]L" = { query = "@loop.outer"; desc = "Next loop end"; };
        };
        goto_previous_start = {
          "[f" = { query = "@call.outer"; desc = "Prev function call start"; };
          "[m" = { query = "@function.outer"; desc = "Prev method/function def start"; };
          "[c" = { query = "@class.outer"; desc = "Prev class start"; };
          "[i" = { query = "@conditional.outer"; desc = "Prev conditional start"; };
          "[l" = { query = "@loop.outer"; desc = "Prev loop start"; };
        };
        goto_previous_end = {
          "[F" = { query = "@call.outer"; desc = "Prev function call end"; };
          "[M" = { query = "@function.outer"; desc = "Prev method/function def end"; };
          "[C" = { query = "@class.outer"; desc = "Prev class end"; };
          "[I" = { query = "@conditional.outer"; desc = "Prev conditional end"; };
          "[L" = { query = "@loop.outer"; desc = "Prev loop end"; };
        };
      };
      opts.textobjects.select = {
        enable = true;
        lookahead = true;
        keymaps = {
          # You can use the capture groups defined in textobjects.scm
          "a=" = { query = "@assignment.outer"; desc = "Select outer part of an assignment"; };
          "i=" = { query = "@assignment.inner"; desc = "Select inner part of an assignment"; };
          "l=" = { query = "@assignment.lhs"; desc = "Select left hand side of an assignment"; };
          "r=" = { query = "@assignment.rhs"; desc = "Select right hand side of an assignment"; };

          # works for javascript/typescript files (custom capture I created in
          # after/queries/ecma/textobjects.scm)
          "a:" = { query = "@property.outer"; desc = "Select outer part of an object property"; };
          "i:" = { query = "@property.inner"; desc = "Select inner part of an object property"; };
          "l:" = { query = "@property.lhs"; desc = "Select left part of an object property"; };
          "r:" = { query = "@property.rhs"; desc = "Select right part of an object property"; };

          "aa" = { query = "@parameter.outer"; desc = "Select outer part of a parameter/argument"; };
          "ia" = { query = "@parameter.inner"; desc = "Select inner part of a parameter/argument"; };

          "ai" = { query = "@conditional.outer"; desc = "Select outer part of a conditional"; };
          "ii" = { query = "@conditional.inner"; desc = "Select inner part of a conditional"; };

          "al" = { query = "@loop.outer"; desc = "Select outer part of a loop"; };
          "il" = { query = "@loop.inner"; desc = "Select inner part of a loop"; };

          "af" = { query = "@call.outer"; desc = "Select outer part of a function call"; };
          "if" = { query = "@call.inner"; desc = "Select inner part of a function call"; };

          "am" = { query = "@function.outer"; desc = "Select outer part of a method/function definition"; };
          "im" = { query = "@function.inner"; desc = "Select inner part of a method/function definition"; };

          "ac" = { query = "@class.outer"; desc = "Select outer part of a class"; };
          "ic" = { query = "@class.inner"; desc = "Select inner part of a class"; };
        };
      };
      opts.textobjects.swap = {
        enable = true;
        swap_next = {
          "<leader>na" = "@parameter.inner"; # swap parameters/argument with next
          "<leader>n:" = "@property.outer"; # swap object property with next
          "<leader>nm" = "@function.outer"; # swap function with next
        };
        swap_previous = {
          "<leader>pa" = "@parameter.inner"; # swap parameters/argument with prev
          "<leader>p:" = "@property.outer"; # swap object property with prev
          "<leader>pm" = "@function.outer"; # swap function with previous
        };
      };
      init =
        let
          # Create one path to hold all of the gammars. This greatly reduces the number of paths
          # That vim needs to search for as opposed to just doing one gammar at a time.
          parsersPath = pkgs.symlinkJoin {
            name = "treesitter-parsers";
            paths = pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies;
            # Taken from https://github.com/nvim-treesitter/nvim-treesitter/issues/6870#issuecomment-2296220844
            postBuild = "rm -r $out/queries";
          };
        in
          /* lua */ ''
          function ()
            -- Put treesitter path as first entry in rtp
            vim.opt.rtp:prepend("${parsersPath}")
          end
        '';
      config =
        let
          mapping = key: cmd: /* lua */ ''
            vim.keymap.set({ "n", "x", "o" }, "${key}", ts_repeat_move.${cmd})
          '';
        in
          /* lua */ ''
          function (_, opts)
            require("nvim-treesitter.configs").setup(opts)
            local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")
            -- vim way: ; goes to the direction you were moving.
            ${mapping ";" "repeat_last_move"}
            ${mapping "," "repeat_last_move_opposite"}
            -- Optionally, make builtin f, F, t, T also repeatable with ; and ,
            ${mapping "f" "builtin_f"}
            ${mapping "F" "builtin_F"}
            ${mapping "t" "builtin_t"}
            ${mapping "T" "builtin_T"}
          end
        '';
    }
  ];
  config.extraFiles."queries/ecma/textobjects.scm".source =
    ./treesitter-extra/after/queries/ecma/textobjects.scm;
  config.extraFiles."queries/nix/injections.scm".source =
    ./treesitter-extra/after/queries/nix/injections.scm;
}
