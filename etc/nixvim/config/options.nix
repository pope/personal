{
  config = {
    clipboard.register = "unnamedplus";
    globals = {
      mapleader = " ";
      maplocalleader = " ";

      # netrw
      # Inspiration from https://github.com/doom-neovim/doom-nvim/blob/main/lua/doom/modules/features/netrw/init.lua
      netrw_banner = 1; # 0 to disable the banner
      netrw_keepdir = 0; # cwd and browser in sync
      netrw_sort_sequence = "[[[\/]$,*]]"; # dirs before files
      netrw_sizestyle = "H"; # human readable
      netrw_list_hide = "vim.fn['netrw_gitignore#Hide']()";
      netrw_hide = 0; # show all files
      netrw_browse_split = 0; # enter opens in current window
    };
    opts =
      let
        sw = 2;
      in
      {
        # line numbers
        number = true;
        relativenumber = true;

        # tabs & indentation
        tabstop = sw;
        shiftwidth = sw;
        softtabstop = sw;
        expandtab = false;
        autoindent = true;
        smartindent = true;

        # word wrapping
        breakindent = true;
        wrap = false;

        # search settings
        hlsearch = true;
        incsearch = true;
        ignorecase = true;
        smartcase = true;

        # input handling
        mouse = "a";
        backspace = "indent,eol,start";
        scrolloff = 4;

        # appearance
        termguicolors = true;
        signcolumn = "yes";
        colorcolumn = "80,100";
        cursorline = true;

        # text folding
        foldcolumn = "0";
        foldlevel = 99;
        foldlevelstart = -1;
        fillchars = {
          fold = "·";
          foldopen = "";
          foldsep = "│";
          foldclose = "";
        };
        foldmethod = "expr";
        foldexpr = "nvim_treesitter#foldexpr()";
        foldenable = true;

        # panels
        splitbelow = true;
        splitright = true;

        # whitespace display
        listchars = {
          eol = "¬";
          extends = "→";
          nbsp = "+";
          precedes = "←";
          tab = "»·";
          trail = "·";
        };
        list = true;

        completeopt = [
          "menu"
          "menuone"
          "noselect"
        ];

        timeout = true;
        timeoutlen = 500;
      };
  };
}
