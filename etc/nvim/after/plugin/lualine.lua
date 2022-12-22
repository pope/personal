require('lualine').setup({
  extensions = { 'fugitive', 'nvim-tree', 'toggleterm' },
  options = {
    --disabled_filetypes = {
    --  statusline = { 'NvimTree' },
    --  winbar = { 'NvimTree' },
    --}
    section_separators = {left = "\u{e0b4}", right = "\u{e0b6}"},
    component_separators = {left = "\u{e0b5}", right = "\u{e0b7}"},
    theme = 'rose-pine',
  },
})
