local rp_status_ok, rose_pine = pcall(require, 'rose-pine')
if not rp_status_ok then
	print('rose-pine not installed')
	return
end

local nf_status_ok, nightfox = pcall(require, 'nightfox')
if not nf_status_ok then
	print('nightfox not installed')
	return
end

local p = require('rose-pine.palette')
local blend = require('rose-pine.util').blend

rose_pine.setup({
	bold_vert_split = false,
	dark_variant = 'main',
	dim_nc_background = false,
	disable_background = true,
	disable_float_background = false,
	highlight_groups = {
		ColorColumn = { bg = p.highlight_low },
		NonText = {
			fg = blend(p.highlight_med, p.highlight_low, .4),
			bg = p.none,
		},
	},
})

nightfox.setup({
	options = {
		transparent = false,
	},
})

vim.cmd.colorscheme('rose-pine')
