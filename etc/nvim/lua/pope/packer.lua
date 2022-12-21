-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use({
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} }
  })

  -- Colors
  use({
    'rose-pine/neovim',
    as = 'rose-pine',
    config = function()
        vim.cmd('colorscheme rose-pine')
    end
  })
  use 'Mofiqul/dracula.nvim'

  use 'nvim-tree/nvim-web-devicons'
  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly' -- optional, updated every week. (see issue #1193)
  }

  use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
  use({
    'nvim-treesitter/playground',
    after = 'nvim-treesiter',
  })
  use({
    'p00f/nvim-ts-rainbow',
    after = 'nvim-treesitter',
  })
  use({
    'nvim-treesitter/nvim-treesitter-textobjects',
    after = 'nvim-treesitter',
  })

  use('mbbill/undotree')

  -- Git
  use('tpope/vim-fugitive')
  use {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end
  }

  -- LSP
  use {
    'VonHeikemen/lsp-zero.nvim',
    requires = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim'},
      {'williamboman/mason-lspconfig.nvim'},

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      {'saadparwaiz1/cmp_luasnip'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-nvim-lua'},

      -- Snippets
      {'L3MON4D3/LuaSnip'},
      {'rafamadriz/friendly-snippets'},
    }
  }

  -- Airline
  use('vim-airline/vim-airline')
  use('vim-airline/vim-airline-themes')
  use('edkolev/tmuxline.vim')

  use('gpanders/editorconfig.nvim')

  use {
    'akinsho/bufferline.nvim', tag = "v3.*",
    requires = 'nvim-tree/nvim-web-devicons'
  }
end)
