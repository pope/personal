local lsp_status_ok, lsp = pcall(require, 'lsp-zero')
if not lsp_status_ok then
	print('lsp-zero not installed')
	return
end

local fidget_status_ok, fidget = pcall(require, 'fidget')
if not fidget_status_ok then
	print('fidget not installed')
	return
end

lsp.preset('recommended')

lsp.ensure_installed({
	'sumneko_lua',
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

lsp.on_attach(function(client, bufnr)
	local keymap_opts = { buffer = bufnr, remap = false }

	-- Document formatting.
	vim.keymap.set('n', '<leader>f', vim.lsp.buf.format, keymap_opts)
	vim.keymap.set('n', '<leader>F', function()
		vim.lsp.buf.format()
		vim.api.nvim_command('write')
	end, keymap_opts)

	-- Show the diagnostic message on hover.
	vim.api.nvim_create_autocmd("CursorHold", {
		buffer = bufnr,
		callback = function()
			vim.diagnostic.open_float(nil, {
				focusable = false,
				close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
				source = 'always',
				scope = 'cursor',
			})
		end
	})
end)

fidget.setup()
lsp.setup()

vim.diagnostic.config({
	-- Enable warnings inline.
	virtual_text = true,
})
