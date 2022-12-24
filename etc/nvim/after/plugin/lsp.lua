local status_ok, lsp = pcall(require, 'lsp-zero')
if not status_ok then
	print('lsp-zero not installed')
	return
end

local status_ok, fidget = pcall(require, 'fidget')
if not status_ok then
	print('fidget not installed')
	return
end

lsp.preset('recommended')

lsp.ensure_installed({
	'sumneko_lua'
})

-- code folding
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.foldingRange = {
	dynamicRegistration = false,
	lineFoldingOnly = true,
}
require('cmp_nvim_lsp').default_capabilities(capabilities)

lsp.nvim_workspace()

fidget.setup()

lsp.setup()

vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
