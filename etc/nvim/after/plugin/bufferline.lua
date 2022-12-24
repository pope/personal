local status_ok, bufferline = pcall(require, 'bufferline')
if not status_ok then
	print('bufferline not installed')
	return
end

bufferline.setup({
	options = {
		offsets = {
			{
				filetype = 'NvimTree',
				text = 'File Explorer',
				highlight = 'Directory',
				text_align = 'center',
			},
		},
	},
})
