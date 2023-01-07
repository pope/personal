local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({
			'git', 'clone', '--depth', '1',
			'https://github.com/wbthomason/packer.nvim', install_path
		})
		vim.cmd [[packadd packer.nvim]]
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	use({
		'nvim-telescope/telescope.nvim',
		requires = { { 'nvim-lua/plenary.nvim' } }
	})

	-- Colors
	use({
		'rose-pine/neovim',
		as = 'rose-pine',
	})
	use 'Mofiqul/dracula.nvim'
	use 'hachy/eva01.vim'
	use 'EdenEast/nightfox.nvim'

	use 'nvim-tree/nvim-web-devicons'

	-- Windowing goodness
	use {
		'nvim-tree/nvim-tree.lua',
		requires = {
			'nvim-tree/nvim-web-devicons', -- optional, for file icons
		},
		tag = 'nightly' -- optional, updated every week. (see issue #1193)
	}
	use {
		'nvim-lualine/lualine.nvim',
		requires = { 'kyazdani42/nvim-web-devicons', opt = true },
	}
	use {
		'akinsho/bufferline.nvim', tag = "v3.*",
		requires = 'nvim-tree/nvim-web-devicons'
	}
	use 'lukas-reineke/indent-blankline.nvim'
	use { 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async' }
	use 'folke/zen-mode.nvim'

	-- Treesitter
	use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
	use({
		'nvim-treesitter/playground',
		after = 'nvim-treesitter',
	})
	use({
		'p00f/nvim-ts-rainbow',
		after = 'nvim-treesitter',
	})
	use({
		'nvim-treesitter/nvim-treesitter-textobjects',
		after = 'nvim-treesitter',
	})

	-- Git
	use('tpope/vim-fugitive')
	use {
		'lewis6991/gitsigns.nvim',
		config = function()
			require('gitsigns').setup()
		end
	}

	-- LSP
	-- Setup
	use 'neovim/nvim-lspconfig'
	use 'williamboman/mason.nvim'
	use 'williamboman/mason-lspconfig.nvim'
	-- Autocompletion
	use 'folke/neodev.nvim'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-nvim-lua'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/nvim-cmp'
	use 'saadparwaiz1/cmp_luasnip'
	-- Snippets
	use 'L3MON4D3/LuaSnip'
	use 'rafamadriz/friendly-snippets'
	-- Support
	use 'SmiteshP/nvim-navic'
	use 'j-hui/fidget.nvim'
	use 'onsails/lspkind.nvim'

	use 'mfussenegger/nvim-lint'

	-- Automatic editor settings
	use('tpope/vim-sleuth')
	use('gpanders/editorconfig.nvim')

	use('mbbill/undotree')
	use('numToStr/Comment.nvim')

	use 'christoomey/vim-tmux-navigator'

	use 'akinsho/toggleterm.nvim'

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if packer_bootstrap then
		require('packer').sync()
	end
end)
