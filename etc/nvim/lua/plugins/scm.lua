return {
	{
		'tpope/vim-fugitive',
		cmd = 'Git',
		keys = {
			{
				'<leader>gs',
				'<cmd>Git<cr>',
				desc = '[g]it [s]tatus',
			},
		},
	},
	{
		'lewis6991/gitsigns.nvim',
		opts = {},
	},
}
