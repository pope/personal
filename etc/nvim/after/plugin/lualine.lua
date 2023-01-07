local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
	print('lualine not installed')
	return
end

lualine.setup({
	extensions = { 'fugitive', 'nvim-tree', 'toggleterm' },
	options = {
		section_separators = { left = '', right = '' },
		component_separators = { left = '', right = '' },
		theme = 'auto',
	},
})

-- Make the current line number match the lualine state.
local auto = require('lualine.themes.auto')
local modes = {
	['n'] = auto.normal.a.bg,
	['i'] = auto.insert.a.bg,
	['c'] = auto.command.a.bg,
	['V'] = auto.visual.a.bg,
	['v'] = auto.visual.a.bg,
	[''] = auto.visual.a.bg,
	['S'] = auto.visual.a.bg,
	['s'] = auto.visual.a.bg,
	['R'] = auto.replace.a.bg,
	['r'] = auto.replace.a.bg,
	['t'] = auto.command.a.bg,
}
vim.api.nvim_create_autocmd('ModeChanged', {
	callback = function()
		local mode = vim.api.nvim_get_mode().mode
		local color = modes[mode] or modes.n

		local base_highlight = vim.api.nvim_get_hl_by_name('CursorLineNr', true)
		local opts = vim.tbl_extend('keep', { foreground = color }, base_highlight)
		vim.api.nvim_set_hl(0, 'CursorLineNr', opts)
	end,
})
