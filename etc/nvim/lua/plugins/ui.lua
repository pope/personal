return {
	{
		'glepnir/dashboard-nvim',
		dependencies = { 'nvim-tree/nvim-web-devicons' },
		event = { 'VimEnter' },
		opts = {
			theme = 'hyper',
		},
	},
	-- Colorscheme
	{
		'rose-pine/neovim',
		name = 'rose-pine',
		priority = 1000,
		opts = function()
			local p = require('rose-pine.palette')
			return {
				dark_variant = 'main',
				dim_inactive_windows = false,
				extend_background_behind_borders = true,
				styles = {
					bold = true,
					italic = true,
					transparency = true,
				},
				highlight_groups = {
					ColorColumn = { bg = p.highlight_low },
					NonText = {
						fg = p.highlight_med,
						bg = p.none,
					},
				},
			}
		end,
		config = function(_, opts)
			require('rose-pine').setup(opts)
			vim.cmd('colorscheme rose-pine')
		end
	},
	{
		'catppuccin/nvim',
		name = 'catppuccin',
		event = { 'VeryLazy' },
		opts = {
			flavour = 'mocha',
			transparent_background = true,
			dim_inactive = {
				enabled = false,
			},
			custom_highlights = function(colors)
				return {
					ColorColumn = { bg = colors.surface0 },
					NonText = {
						fg = colors.surface0,
					},
				}
			end,
		},
	},
	{
		'Mofiqul/dracula.nvim',
		event = { 'VeryLazy' },
	},
	{
		'hachy/eva01.vim',
		event = { 'VeryLazy' },
	},
	{
		'EdenEast/nightfox.nvim',
		event = { 'VeryLazy' },
		opts = {
			options = {
				transparent = false,
			},
		},
	},
	{
		'folke/tokyonight.nvim',
		event = { 'VeryLazy' },
		opts = {},
	},
	{
		'navarasu/onedark.nvim',
		event = { 'VeryLazy' },
		opts = {
			style = 'dark',
		},
	},
	{
		"brenoprata10/nvim-highlight-colors",
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			render = 'virtual',
			enable_named_colors = true,
			enable_tailwind = false,
		},
	},
	'nvim-tree/nvim-web-devicons',
	{
		'nvim-tree/nvim-tree.lua',
		dependencies = {
			'nvim-tree/nvim-web-devicons',
		},
		cmd = { 'NvimTreeToggle' },
		keys = {
			{
				'<leader>pv',
				'<cmd>NvimTreeToggle<cr>',
				desc = '[p]roject tree [v]iew',
			},
		},
		opts = {
			view = {
				width = 32,
			},
			renderer = {
				indent_markers = {
					enable = true,
				},
			},
			sync_root_with_cwd = true,
			update_focused_file = {
				enable = true,
			},
		},
	},
	{
		"prichrd/netrw.nvim",
		dependencies = {
			'nvim-tree/nvim-web-devicons',
		},
		-- event = { 'VeryLazy' },
		opts = {
			icons = {
				symlink = " ",
				directory = " ",
				file = " ",
			},
			use_devicons = true,
			mappings = {
				["K"] = function(payload)
					print(vim.inspect(payload))
				end,
			},
		},
	},
	{
		'nvim-lualine/lualine.nvim',
		dependencies = {
			'nvim-tree/nvim-web-devicons',
			'SmiteshP/nvim-navic',
		},
		opts = {
			extensions = { 'fugitive', 'nvim-tree', 'toggleterm' },
			options = {
				section_separators = { left = '', right = '' },
				component_separators = { left = '', right = '' },
				theme = 'auto',
			},
			sections = {
				lualine_c = {
					'filename',
					{ 'navic', navic_opts = nil },
				},
			},
		},
		config = function(_, opts)
			require('lualine').setup(opts)

			-- Make the current line number match the lualine state.
			local auto = require('lualine.themes.auto')
			local modes = {
				['n'] = auto.normal.a.bg,
				['i'] = auto.insert.a.bg,
				['c'] = auto.command.a.bg,
				['V'] = auto.visual.a.bg,
				['v'] = auto.visual.a.bg,
				[''] = auto.visual.a.bg,
				['S'] = auto.visual.a.bg,
				['s'] = auto.visual.a.bg,
				['R'] = auto.replace.a.bg,
				['r'] = auto.replace.a.bg,
				['t'] = auto.command.a.bg,
			}
			vim.api.nvim_create_augroup('pope_lualine', {})
			vim.api.nvim_create_autocmd('ModeChanged', {
				callback = function()
					local mode = vim.api.nvim_get_mode().mode
					local color = modes[mode] or modes.n

					local base_highlight = vim.api.nvim_get_hl_by_name('CursorLineNr', true)
					local o = vim.tbl_extend('keep', { foreground = color }, base_highlight)
					vim.api.nvim_set_hl(0, 'CursorLineNr', o)
				end,
				group = 'pope_lualine',
			})
		end,
	},
	{
		'lukas-reineke/indent-blankline.nvim',
		main = 'ibl',
		opts = {
			indent = {
				char = '┊',
				-- tab_char = '┊',
				highlight = { 'NonText' },
			},
			whitespace = {
				highlight = { 'NonText' },
				remove_blankline_trail = false,
			},
			scope = {
				enabled = false,
			},
			exclude = {
				filetypes = {
					'fugitive',
					'dashboard',
				},
			},
		},
	},
	{
		'folke/zen-mode.nvim',
		cmd = { 'ZenMode' },
	},
	{
		'melkster/modicator.nvim',
		opts = {},
		init = function()
			vim.o.termguicolors = true
			vim.o.cursorline = true
			vim.o.number = true
		end,
	},
	{
		'karb94/neoscroll.nvim',
		event = { 'VeryLazy' },
		opts = {},
	},
	{
		'kevinhwang91/nvim-ufo',
		dependencies = {
			'kevinhwang91/promise-async'
		},
		event = { 'BufReadPost', 'BufNewFile' },
		init = function()
			vim.opt.foldcolumn = '0' -- '0' is not bad, '1' for one column
			vim.opt.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
			vim.opt.foldlevelstart = -1

			vim.opt.fillchars = [[fold:·,foldopen:,foldsep:│,foldclose:]]

			vim.opt.foldenable = true
		end,
		opts = function(_, opts)
			local handler = function(virtText, lnum, endLnum, width, truncate)
				local newVirtText = {}
				local suffix = (' ⤸ %d '):format(endLnum - lnum)
				local sufWidth = vim.fn.strdisplaywidth(suffix)
				local targetWidth = width - sufWidth
				local curWidth = 0
				for _, chunk in ipairs(virtText) do
					local chunkText = chunk[1]
					local chunkWidth = vim.fn.strdisplaywidth(chunkText)
					if targetWidth > curWidth + chunkWidth then
						table.insert(newVirtText, chunk)
					else
						chunkText = truncate(chunkText, targetWidth - curWidth)
						local hlGroup = chunk[2]
						table.insert(newVirtText, { chunkText, hlGroup })
						chunkWidth = vim.fn.strdisplaywidth(chunkText)
						-- str width returned from truncate() may less than 2nd argument, need padding
						if curWidth + chunkWidth < targetWidth then
							suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
						end
						break
					end
					curWidth = curWidth + chunkWidth
				end
				table.insert(newVirtText, { suffix, 'MoreMsg' })
				return newVirtText
			end

			return vim.tbl_extend('keep', {
				fold_virt_text_handler = handler,
			}, opts)
		end,
	},
	{
		'folke/noice.nvim',
		event = 'VeryLazy',
		opts = {
			lsp = {
				-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
				override = {
					['vim.lsp.util.convert_input_to_markdown_lines'] = true,
					['vim.lsp.util.stylize_markdown'] = true,
					['cmp.entry.get_documentation'] = true, -- requires hrsh7th/nvim-cmp
				},
			},
			-- you can enable a preset for easier configuration
			presets = {
				bottom_search = true, -- use a classic bottom cmdline for search
				command_palette = true, -- position the cmdline and popupmenu together
				long_message_to_split = true, -- long messages will be sent to a split
				inc_rename = false, -- enables an input dialog for inc-rename.nvim
				lsp_doc_border = false, -- add a border to hover docs and signature help
			},
		},
		dependencies = {
			'MunifTanjim/nui.nvim',
			'rcarriga/nvim-notify',
		}
	},
	{
		'rcarriga/nvim-notify',
		lazy = true,
		opts = {
			background_colour = '#000000'
		},
	},
	{
		'stevearc/dressing.nvim',
		opts = {},
	},
}
