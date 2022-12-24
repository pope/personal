local status_ok, rose_pine = pcall(require, 'rose-pine')
if not status_ok then
	print('rose-pine not installed')
	return
end

local status_ok, nightfox = pcall(require, 'nightfox')
if not status_ok then
	print('nightfox not installed')
	return
end

rose_pine.setup({
	bold_vert_split = false,
	dark_variant = 'main',
	dim_nc_background = false,
	disable_background = true,
	disable_float_background = false,
})

nightfox.setup({
	options = {
		transparent = true,
	},
})

function ColorMyPencils(color)
	color = color or 'rose-pine'
	vim.cmd.colorscheme(color)

	if color == 'rose-pine' then
		local p = require('rose-pine.palette')
		local blend = require('rose-pine.util').blend

		vim.api.nvim_set_hl(0, 'ColorColumn', { bg = p.highlight_low })
		vim.api.nvim_set_hl(0, 'NonText', {
			fg = blend(p.highlight_med, p.highlight_low, .4),
			bg = p.none,
		})
	end
end

--ColorMyPencils('dracula')
ColorMyPencils()
