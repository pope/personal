local status_ok, lsp = pcall(require, 'lsp-zero')
if not status_ok then
	print('lsp-zero not installed')
	return
end

lsp.preset('recommended')

lsp.ensure_installed({
	'sumneko_lua'
})

lsp.nvim_workspace()

lsp.setup()

vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
