require('rose-pine').setup({
  dark_variant = 'moon',
  disable_background = true
})

function ColorMyPencils(color)
  color = color or 'rose-pine'
  vim.cmd.colorscheme(color)

  vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })

  if color == 'rose-pine' then
    vim.api.nvim_set_hl(0, 'ColorColumn', { bg = '#2a283e' })
    vim.api.nvim_set_hl(0, 'NonText', { fg = '#44415a', bg = 'none' })
  end
end

--ColorMyPencils('dracula')
ColorMyPencils()
