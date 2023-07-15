return {
	'mbbill/undotree',
	{
		'dstein64/vim-startuptime',
		cmd = 'StartupTime',
	},
	{
		'folke/which-key.nvim',
		cmd = { 'WhichKey' },
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 500
		end,
		opts = {
			operators = {
				gc = 'Line comments',
				gb = 'Block comments',
				ys = 'Add surround',
				cs = 'Change surround',
				ds = 'Delete surround',
				ii = 'Object scope',
				ai = 'Object scope with border'
			},
		},
	},
	{
		'akinsho/toggleterm.nvim',
		opts = {
			size = 10,
			open_mapping = [[<C-\>]],
			start_in_insert = true,
			direction = 'float',
			float_opts = {
				border = 'curved',
			},
		},
		config = function(_, opts)
			require('toggleterm').setup(opts)

			function _G.set_terminal_keymaps()
				local km_opts = { buffer = 0 }
				vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], km_opts)
				vim.keymap.set('t', 'jk', [[<C-\><C-n>]], km_opts)
				vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], km_opts)
				vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], km_opts)
				vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], km_opts)
				vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], km_opts)
			end

			-- if you only want these mappings for toggle term use term://*toggleterm#* instead
			vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")
		end,
		cmd = { 'ToggleTerm' },
		keys = {
			[[<C-\>]],
		},
	},
}
