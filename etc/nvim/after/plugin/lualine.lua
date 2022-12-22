require('lualine').setup({
  extensions = { 'fugitive', 'nvim-tree', 'toggleterm' },
  options = {
    --disabled_filetypes = {
    --  statusline = { 'NvimTree' },
    --  winbar = { 'NvimTree' },
    --}
    section_separators = { left = '', right = '' },
    component_separators = { left = '', right = '' },
    theme = 'rose-pine',
  },
})
