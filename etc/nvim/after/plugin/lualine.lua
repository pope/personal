local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
	return
end

lualine.setup({
	extensions = { 'fugitive', 'nvim-tree', 'toggleterm' },
	options = {
		section_separators = { left = '', right = '' },
		component_separators = { left = '', right = '' },
		theme = 'rose-pine',
	},
})
