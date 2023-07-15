return {
	'tpope/vim-sleuth',
	'mg979/vim-visual-multi',
	{
		'numToStr/Comment.nvim',
		config = true,
		keys = {
			{ 'gc',  mode = 'v', desc = 'Toggle linewise comment' },
			{ 'gb',  mode = 'v', desc = 'Toggle blockwise comment' },
			{ 'gcc', mode = 'n', desc = 'Toggle current line linewise comment' },
			{ 'gbb', mode = 'n', desc = 'Toggle current line blockwise comment' },
		},
	},
	{
		'ojroques/nvim-osc52',
		opts = {
			max_length = 0, -- Maximum length of selection (0 for no limit)
			silent = false, -- Disable message on successful copy
			trim = false, -- Trim text before copy
		},
		keys = {
			{
				'<leader>y',
				function()
					require('osc52').copy_visual()
				end,
				mode = 'v',
			},
		},
	},
	{
		'windwp/nvim-autopairs',
		event = { 'InsertEnter' },
		opts = {
			break_undo = false,
			check_ts = true,            -- enable treesitter
			ts_config = {
				lua = { 'string' },     -- don't add pairs in lua string treesitter nodes
				javascript = { 'template_string' }, -- don't add pairs in javscript template_string treesitter nodes
				java = false,           -- don't check treesitter on java
			},
		},
	},
}
