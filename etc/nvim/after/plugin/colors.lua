local status_ok, rose_pine = pcall(require, 'rose-pine')
if not status_ok then
	return
end

rose_pine.setup({
	dark_variant = 'main',
	disable_background = true
})

function ColorMyPencils(color)
	color = color or 'rose-pine'
	vim.cmd.colorscheme(color)

	vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
	vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })

	if color == 'rose-pine' then
		vim.api.nvim_set_hl(0, 'ColorColumn', { bg = '#21202e' })
		vim.api.nvim_set_hl(0, 'NonText', { fg = '#403d52', bg = 'none' })
	end
end

--ColorMyPencils('dracula')
ColorMyPencils()
