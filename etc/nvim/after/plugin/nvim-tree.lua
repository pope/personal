local status_ok, nvim_tree = pcall(require, 'nvim-tree')
if not status_ok then
	print('nvim-tree not installed')
	return
end

nvim_tree.setup({
	--open_on_setup = true,
	renderer = {
		indent_markers = {
			enable = true,
		}
	},
	sync_root_with_cwd = true,
	update_focused_file = {
		enable = true,
	},
})
