local status_ok, lint = pcall(require, 'lint')
if not status_ok then
	print('lint not installed')
	return
end

lint.linters_by_ft = {
	markdown = { 'vale', }
}

vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
	callback = function()
		lint.try_lint()
	end,
})
