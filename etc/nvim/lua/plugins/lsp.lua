-- Shout out to https://vonheikemen.github.io/devlog/tools/setup-nvim-lspconfig-plus-nvim-cmp/

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

-- code folding
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.foldingRange = {
	dynamicRegistration = false,
	lineFoldingOnly = true,
}

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
		require('nvim-navic').attach(client, bufnr)
	end
end

return {
	{
		'SmiteshP/nvim-navic',
		lazy = true,
		init = function()
			vim.g.navic_silence = true
		end,
		opts = {},
	},
	{
		'j-hui/fidget.nvim',
		tag = 'legacy',
		event = { 'VeryLazy' },
		opts = {},
	},
	{
		'hrsh7th/cmp-nvim-lsp',
		lazy = true,
		config = function()
			--This is standard cmp_nvim_lsp config
			capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
		end,
	},
	{
		'williamboman/mason.nvim',
		build = ':MasonUpdate',
		opts = {},
	},
	{
		'williamboman/mason-lspconfig.nvim',
		dependencies = {
			'SmiteshP/nvim-navic',
			'hrsh7th/cmp-nvim-lsp',
			'neovim/nvim-lspconfig',
			'williamboman/mason.nvim',
		},
		lazy = true,
		config = function(_, opts)
			local lspconfig = require('lspconfig')
			local server = {
				dartls = {},
				nil_ls = {},
				rust_analyzer = {
					['rust-analyzer'] = {},
				},
				lua_ls = {
					Lua = {
						diagnostics = {
							globals = { 'vim' },
						},
						workspace = { checkThirdParty = false },
						telemetry = { enable = false },
					},
				},
				gopls = {
					gopls = {
						analyses = {
							unusedparams = true,
						},
						staticcheck = true,
					},
				},
			}
			for name, settings in pairs(server) do
				lspconfig[name].setup({
					capabilities = capabilities,
					on_attach = on_attach,
					settings = settings,
				})
			end
			require('mason-lspconfig').setup(opts)
			require('mason-lspconfig').setup_handlers({
				function(server_name)
					require('lspconfig')[server_name].setup({
						capabilities = capabilities,
						on_attach = on_attach,
					})
				end,
			})
		end,
	},
	{
		'hrsh7th/nvim-cmp',
		dependencies = {
			-- Utils
			'onsails/lspkind.nvim',
			-- Config
			'hrsh7th/cmp-nvim-lsp',
			'williamboman/mason-lspconfig.nvim',
			-- Autocompletion
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-nvim-lua',
			'hrsh7th/cmp-path',
			'saadparwaiz1/cmp_luasnip',
			-- Snippets
			'L3MON4D3/LuaSnip',
			'rafamadriz/friendly-snippets',
		},
		event = { 'BufReadPre', 'BufNewFile' },
		init = function()
			vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }
		end,
		opts = function(_, opts)
			local cmp = require('cmp')
			local luasnip = require('luasnip')
			local lspkind = require('lspkind')
			lspkind.init()

			require('luasnip.loaders.from_vscode').lazy_load()

			return vim.tbl_extend('keep', {
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
			}, opts)
		end,
	},
	{
		"folke/neodev.nvim",
		ft = "lua",
		opts = { pathStrict = true },
	},
}
