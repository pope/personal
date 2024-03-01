-- line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- tabs & indendation
local indendation_width = 2
vim.opt.tabstop = indendation_width
vim.opt.shiftwidth = indendation_width
vim.opt.softtabstop = indendation_width
vim.opt.expandtab = false
vim.opt.autoindent = true
vim.opt.smartindent = true

-- word wrapping
vim.opt.breakindent = true
vim.opt.wrap = false

-- search settings
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- appearance
vim.opt.termguicolors = true
vim.opt.signcolumn = 'yes'
vim.opt.colorcolumn = '80,100'
vim.opt.cursorline = true

-- input handling
vim.opt.mouse = 'a'
vim.opt.backspace = 'indent,eol,start'
vim.opt.scrolloff = 4

-- clipboard
vim.opt.clipboard:append('unnamedplus')

-- panels
vim.opt.splitbelow = true
vim.opt.splitright = true

-- list chars
vim.opt.listchars:append({
	eol = '¬',
	extends = '→',
	nbsp = '+',
	precedes = '←',
	tab = '»·',
	trail = '·',
})
vim.opt.list = true

-- code complete
vim.opt.completeopt:append('menu')
vim.opt.completeopt:append('menuone')
vim.opt.completeopt:append('noselect')

-- responsiveness
vim.opt.updatetime = 250

-- netrw
-- Inspiration from https://github.com/doom-neovim/doom-nvim/blob/main/lua/doom/modules/features/netrw/init.lua
vim.g.netrw_banner = 1                  -- 0 to disable the banner
vim.g.netrw_keepdir = 0                 -- cwd and browser in sync
vim.g.netrw_sort_sequence = [[[\/]$,*]] -- dirs before files
vim.g.netrw_sizestyle = 'H'             -- human readable
vim.g.netrw_list_hide = vim.fn['netrw_gitignore#Hide']()
vim.g.netrw_hide = 0                    -- show all files
vim.g.netrw_browse_split = 4            -- enter opens in previous window
