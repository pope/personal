local status_ok, zen_mode = pcall(require, 'zen-mode')
if not status_ok then
	print('zen-mode not installed')
	return
end

zen_mode.setup()
