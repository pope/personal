local status_ok, lint = pcall(require, 'lint')
if not status_ok then
	print('lint not installed')
	return
end

vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
	callback = function()
		lint.try_lint(nil, { ignore_errors = true })
	end,
})
