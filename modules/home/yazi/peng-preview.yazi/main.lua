--- On-demand Camera RAW and CBZ previewer backed by `peng-preview -o`.
--- Uses `peng-preview` (positional RAW header reads, TurboJPEG DCT scaling,
--- Freedesktop shared thumbnail cache, and atomic JPEG/PNG output) so Yazi's
--- native `ya.image_show` renders previews across Kitty, Ghostty, WezTerm,
--- Foot, and tmux.
local M = {}

local function has_cache(cache)
	local cha = fs.cha(cache)
	return cha ~= nil and cha.len > 0
end

local function generate_preview(job, cache)
	local max_w = rt.preview.max_width or 1024
	local max_h = rt.preview.max_height or 1024
	local size_arg = string.format("%dx%d", max_w, max_h)
	local src_path = tostring(job.file.path or job.file.url)

	local status, err = Command("peng-preview")
		:arg({ "--size", size_arg, "--output", tostring(cache), "--", src_path })
		:status()

	if not status then
		return false, Err("Failed to start `peng-preview`: %s", err)
	elseif not status.success then
		return false, Err("`peng-preview` exited with code: %s", tostring(status.code or "signal"))
	end
	return true
end

function M:peek(job)
	local cache = ya.file_cache(job)
	if not cache then
		return
	end

	if not has_cache(cache) then
		local delay = (rt.preview.image_delay or 30) / 1000
		if delay > 0 then
			ya.sleep(delay)
		end

		if not has_cache(cache) then
			local ok, err = generate_preview(job, cache)
			if not ok then
				return ya.preview_widget(job, err)
			end
		end
	end

	local _, show_err = ya.image_show(cache, job.area)
	ya.preview_widget(job, show_err)
end

function M:seek() end

--- Intentional no-op preloader: intercepts Camera RAWs before Yazi's built-in
--- `{ mime = "image/*", run = "image" }` preloader can poison the cache with
--- 160x120 TIFF IFD0 thumbnails, without queueing batch directory work.
function M:preload()
	return true
end

return M
