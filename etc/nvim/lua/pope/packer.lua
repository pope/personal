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
  -- use({
  --   'rose-pine/neovim',
  --   as = 'rose-pine',
  --   --config = function()
  --   --    vim.cmd('colorscheme rose-pine')
  --   --end
  -- })
  use 'Mofiqul/dracula.nvim'

  use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
  use('nvim-treesitter/playground')
  use({
    'ThePrimeagen/harpoon',
    requires = { {'nvim-lua/plenary.nvim'} }
  })
  use('mbbill/undotree')
  use('tpope/vim-fugitive')
end)
