require("nvim-tree").setup({
	open_on_setup = true,
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
