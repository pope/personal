return {
	{
		"wfxr/protobuf.vim",
		ft = { "proto" },
	},
	{
		'nvim-treesitter/nvim-treesitter',
		build = ':TSUpdate',
		dependencies = {
			{
				'nvim-treesitter/playground',
				'mrjones2014/nvim-ts-rainbow',
				'nvim-treesitter/nvim-treesitter-textobjects',
			}
		},
		event = { 'BufReadPost', 'BufNewFile' },
		cmd = { 'TSUpdateSync', 'TSUpdate' },
		keys = {
			{ '<C-space>', desc = 'Increment selection' },
			{ '<bs>',      desc = 'Decrement selection', mode = 'x' },
		},
		opts = {
			ensure_installed = {
				'bash',
				'c',
				'cmake',
				'comment',
				'cpp',
				'dockerfile',
				'fish',
				'go',
				'html',
				'http',
				'java',
				'javascript',
				'jsdoc',
				'json',
				'lua',
				'make',
				'markdown',
				'ninja',
				'python',
				'regex',
				'rust',
				'scss',
				'toml',
				'tsx',
				'typescript',
				'vim',
				'yaml',
			},
			-- Install parsers synchronously (only applied to `ensure_installed`)
			sync_install = false,
			-- Automatically install missing parsers when entering buffer
			auto_install = false,

			highlight = {
				enable = true,
				disable = function(lang, buf)
					local max_filesize = 100 * 1024 -- 100 KB
					local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
					if ok and stats and stats.size > max_filesize then
						return true
					end
				end,
				additional_vim_regex_highlighting = false,
			},

			indent = { enable = true, disable = { 'python' } },

			rainbow = {
				enable = true,
				extended_mode = true,
			},

			incremental_selection = {
				enable = true,
				keymaps = {
					init_selection = '<C-space>',
					node_incremental = '<C-space>',
					scope_incremental = '<C-s>',
					--node_decremental = '<C-backspace>',
					node_decremental = '<bs>',
				},
			},

			textobjects = {
				select = {
					enable = true,
					lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
					keymaps = {
						-- You can use the capture groups defined in textobjects.scm
						['aa'] = '@parameter.outer',
						['ia'] = '@parameter.inner',
						['af'] = '@function.outer',
						['if'] = '@function.inner',
						['ac'] = '@class.outer',
						['ic'] = '@class.inner',
					},
				},
				move = {
					enable = true,
					set_jumps = true, -- whether to set jumps in the jumplist
					goto_next_start = {
						[']m'] = '@function.outer',
						[']]'] = '@class.outer',
					},
					goto_next_end = {
						[']M'] = '@function.outer',
						[']['] = '@class.outer',
					},
					goto_previous_start = {
						['[m'] = '@function.outer',
						['[['] = '@class.outer',
					},
					goto_previous_end = {
						['[M'] = '@function.outer',
						['[]'] = '@class.outer',
					},
				},
				swap = {
					enable = true,
					swap_next = {
						['<leader>a'] = '@parameter.inner',
					},
					swap_previous = {
						['<leader>A'] = '@parameter.inner',
					},
				},
			},
		},
		config = function(_, opts)
			require('nvim-treesitter.configs').setup(opts)
		end,
	},
}
