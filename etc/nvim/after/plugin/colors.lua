local rp_status_ok, rose_pine = pcall(require, 'rose-pine')
if rp_status_ok then
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

	vim.cmd.colorscheme('rose-pine')
end

local nf_status_ok, nightfox = pcall(require, 'nightfox')
if nf_status_ok then
	nightfox.setup({
		options = {
			transparent = false,
		},
	})
end

local od_status_ok, onedark = pcall(require, 'onedark')
if od_status_ok then
	onedark.setup({
		style = 'dark',
	})
end
