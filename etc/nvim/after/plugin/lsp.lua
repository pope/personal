-- Imports {{{
local lspkind_status_ok, lspkind = pcall(require, 'lspkind')
if not lspkind_status_ok then
	print('lspkind not installed')
	return
end

local fidget_status_ok, fidget = pcall(require, 'fidget')
if not fidget_status_ok then
	print('fidget not installed')
	return
end

local lspconfig_status_ok, lspconfig = pcall(require, 'lspconfig')
if not lspconfig_status_ok then
	print('lspconfig not installed')
	return
end

local cmp_nvim_lsp_status_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if not cmp_nvim_lsp_status_ok then
	print('cmp_nvim_lsp not installed')
	return
end

local cmp_status_ok, cmp = pcall(require, 'cmp')
if not cmp_status_ok then
	print('cmp not installed')
	return
end

local navic_status_ok, navic = pcall(require, 'nvim-navic')
if not navic_status_ok then
	print('nvim-navic not installed')
	return
end

local bbq_status_ok, barbecue = pcall(require, 'barbecue')
if not bbq_status_ok then
	print('barbecue not installed')
	return
end

local neodev_status_ok, neodev = pcall(require, 'neodev')
if not neodev_status_ok then
	print('neodev not installed')
	return
end

local mason_status_ok, mason = pcall(require, 'mason')
if not mason_status_ok then
	print('mason not installed')
	return
end

local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, 'mason-lspconfig')
if not mason_lspconfig_status_ok then
	print('mason-lspconfig not installed')
	return
end

local luasnip_status_ok, luasnip = pcall(require, 'luasnip')
if not luasnip_status_ok then
	print('luasnip not installed')
	return
end
-- }}}

-- icons
lspkind.init()
-- lsp reporting
fidget.setup()
-- lua + vim support
neodev.setup()
-- code context
navic.setup({})
barbecue.setup({
	attach_navic = false,
})

-- diagnostic
local sign = function(opts)
	if type(opts.text) ~= 'string' then
		return
	end

	vim.fn.sign_define(opts.name, {
		texthl = opts.name,
		text = opts.text,
		numhl = ''
	})
end
sign({ name = 'DiagnosticSignError', text = '✘' })
sign({ name = 'DiagnosticSignWarn', text = '▲' })
sign({ name = 'DiagnosticSignHint', text = '⚑' })
sign({ name = 'DiagnosticSignInfo', text = '' })
vim.diagnostic.config({
	-- Enable warnings inline.
	virtual_text = true,
	signs = true,
	update_in_insert = false,
	underline = true,
	severity_sort = true,
	float = {
		focusable = false,
		style = 'minimal',
		border = 'rounded',
		source = 'always',
		header = '',
		prefix = '',
	},
})

local on_attach = function(client, bufnr)
	local nmap = function(keys, cmd, desc)
		if desc then
			desc = 'LSP: ' .. desc
		end
		vim.keymap.set('n', keys, cmd, { buffer = bufnr, desc = desc })
	end

	-- Actions
	nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
	nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

	-- Jumping Around
	nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
	nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
	nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
	nmap('gtd', vim.lsp.buf.type_definition, '[G]oto [T]ype [D]efinition')

	local status_ok, ts_builtin = pcall(require, 'telescope.builtin')
	if status_ok then
		nmap('<leader>ds', ts_builtin.lsp_document_symbols, '[D]ocument [S]ymbols')
		nmap('<leader>ws', ts_builtin.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
		nmap('gr', ts_builtin.lsp_references, '[G]oto [R]eferences')
	else
		nmap('gr', vim.lsp.buf.references, '[G]oto [R]eferences')
	end

	-- Help
	nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
	nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

	-- Workspace
	nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
	nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
	nmap('<leader>wl', function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, '[W]orkspace [L]ist Folders')

	-- Document formatting.
	nmap('<leader>f', vim.lsp.buf.format, '[F]ormat')
	nmap('<leader>F', function()
		vim.lsp.buf.format()
		vim.api.nvim_command('write')
	end, '[F]ormat and save')

	if client.server_capabilities['documentSymbolProvider'] then
		navic.attach(client, bufnr)
	end
end

local servers = {
	lua_ls = {
		Lua = {
			workspace = { checkThirdParty = false },
			telemetry = { enable = false },
		},
	},
}

-- code folding
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.foldingRange = {
	dynamicRegistration = false,
	lineFoldingOnly = true,
}

cmp_nvim_lsp.default_capabilities(capabilities)

mason.setup()
mason_lspconfig.setup({
	ensure_installed = vim.tbl_keys(servers),
})
mason_lspconfig.setup_handlers({
	function(server_name)
		lspconfig[server_name].setup {
			capabilities = capabilities,
			on_attach = on_attach,
			settings = servers[server_name],
		}
	end,
})

cmp.setup {
	formatting = {
		format = lspkind.cmp_format({
			with_text = true,
			maxwidth = 60,
			menu = {
				buffer = '[buffer ]',
				luasnip = '[snip ]',
				nvim_lsp = '[LSP ]',
				nvim_lua = '[API ]',
				path = '[path 練]',
			},
		}),
	},
	mapping = cmp.mapping.preset.insert {
		['<C-d>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete({}),
		['<CR>'] = cmp.mapping.confirm {
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		},
		['<C-e>'] = cmp.mapping(function(_)
			if cmp.visible() then
				cmp.abort()
			else
				cmp.complete()
			end
		end),
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { 'i', 's' }),
		['<S-Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { 'i', 's' }),
	},
	sources = cmp.config.sources({
		{ name = 'luasnip', keyword_length = 2 },
		{ name = 'nvim_lsp' },
		{ name = 'path' },
	}, {
		{ name = 'buffer', keyword_length = 3 },
	}),
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
}

-- vim: foldmethod=marker
