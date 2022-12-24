local status_ok, indent_blankline = pcall(require, 'indent_blankline')
if not status_ok then
	print('indent_blankline not installed')
	return
end

indent_blankline.setup {
	--char                           = '┊',
	disable_with_nolist            = true,
	show_trailing_blankline_indent = false,
	char_highlight_list            = { 'NonText' },
	space_char_highlight_list      = { 'NonText' },
}
