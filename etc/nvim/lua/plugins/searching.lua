return {
	{
		'nvim-telescope/telescope.nvim',
		dependencies = {
			'nvim-lua/plenary.nvim',
			'nvim-telescope/telescope-file-browser.nvim',
			'folke/noice.nvim',
		},
		opts = {},
		config = function(_, opts)
			require('telescope').setup(opts)
			require('telescope').load_extension('file_browser')
			require('telescope').load_extension('noice')
		end,
		cmd = { 'Telescope' },
		keys = {
			{
				'<leader><space>',
				'<cmd>Telescope oldfiles<cr>',
				desc = '[ ] Find recently opened files',
			},
			{
				'<leader>?',
				'<cmd>Telescope buffers<cr>',
				desc = '[?] Find existing buffers',
			},
			{
				'<leader>/',
				'<cmd>Telescope current_buffer_fuzzy_find<cr>',
				desc = '[/] Fuzzily search in current buffer'
			},
			{
				'<leader>r',
				'<cmd>Telescope resume<cr>',
				desc = '[r]esume find',
			},
			{
				'<leader>sG',
				function()
					require('telescope.builtin').grep_string({
						search = vim.fn.input('Grep > '),
					})
				end,
				desc = '[s]earch [G]rep',
			},
			{
				'<leader>sf',
				'<cmd>Telescope find_files<cr>',
				desc = '[s]earch [f]iles',
			},
			{
				'<leader>sh',
				'<cmd>Telescope help_tags<cr>',
				desc = '[s]earch [h]elp',
			},
			{
				'<leader>sw',
				'<cmd>Telescope grep_string<cr>',
				desc = '[s]earch current [w]ord',
			},
			{
				'<leader>sr',
				'<cmd>Telescope live_grep<cr>',
				desc = '[s]earch via [r]ipgrep',
			},
			{
				'<leader>sd',
				'<cmd>Telescope diagnostics<cr>',
				desc = '[s]earch [d]iagnostics',
			},
			{
				'<leader>sg',
				'<cmd>Telescope git_files<cr>',
				desc = '[s]earch [g]it',
			},
		},
	},
}
