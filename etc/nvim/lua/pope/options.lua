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

-- line wrapping
vim.opt.wrap = false

-- search settings
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- cursor
vim.opt.cursorline = true

-- appearance
vim.opt.termguicolors = true
vim.opt.signcolumn = 'yes'
vim.opt.colorcolumn = '80,100'

-- input handling
vim.opt.mouse = 'a'
vim.opt.backspace = 'indent,eol,start'

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
vim.opt.list = false
