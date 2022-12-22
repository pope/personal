local builtin = require('telescope.builtin')

vim.keymap.set('n', '<leader><space>', builtin.oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>/', builtin.buffers, { desc = '[ ] Find existing buffers' })

vim.keymap.set('n', '<leader>sG', function()
  builtin.grep_string({ search = vim.fn.input("Grep > ") })
end, { desc = '[S]earch [G]rep' })

vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', builtin.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sr', builtin.live_grep, { desc = '[S]earch by [R]ipgrep' })
vim.keymap.set('n', '<leader>sd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sg', builtin.git_files, { desc = '[S]earch [G]it' })
