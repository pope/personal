--- Linux Freedesktop.org thumbnail cache preloader/previewer for `image/*` and `video/*`.
--- Reads/writes `$XDG_CACHE_HOME/thumbnails/{xx-large,x-large,large,normal}/<md5>.png`
--- using pure-Lua PNG `Thumb::MTime` validation and `allmytoes -sxx` (with `magick`
--- and `ffmpegthumbnailer` fallbacks). Delegates `job.skip > 0` video scrubbing to `video`.
local M = {}

local THUMB_BASE_DIR = (os.getenv("XDG_CACHE_HOME") or ((os.getenv("HOME") or "") .. "/.cache")) .. "/thumbnails"

local MAGICK_ONLY_EXTS = {
	avif = true,
	heic = true,
	heif = true,
	jxl = true,
	tif = true,
	tiff = true,
}

local VIDEO_EXTS = {
	mp4 = true,
	m4v = true,
	mkv = true,
	webm = true,
	mov = true,
	avi = true,
	wmv = true,
	flv = true,
	mpg = true,
	mpeg = true,
	m2ts = true,
	mts = true,
	ts = true,
	ogv = true,
	["3gp"] = true,
}

local MD5_K = {
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
}

local MD5_S = {
	7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
	5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
	4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
	6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
}

local function trim(s)
	return s and s:match("^%s*(.-)%s*$") or ""
end

-- NOTE: `read_u32` is also in `raw-image.yazi/main.lua` and `exif-spotter.yazi/main.lua`.
local function read_u32(buf, pos, is_le)
	local b1, b2, b3, b4 = buf:byte(pos, pos + 3)
	if not b4 then
		return nil
	end
	return is_le and (b1 + b2 * 256 + b3 * 65536 + b4 * 16777216)
		or (b1 * 16777216 + b2 * 65536 + b3 * 256 + b4)
end

local function path_to_file_uri(path)
	return "file://"
		.. path:gsub("[^A-Za-z0-9%-_%.!~*'():@&=+%$,/]", function(c)
			return string.format("%%%02X", c:byte(1))
		end)
end

local function md5_hex(msg)
	local bit_len = #msg * 8
	local pad_len = (56 - (#msg + 1) % 64) % 64
	msg = msg .. "\x80" .. string.rep("\0", pad_len) .. string.pack("<I8", bit_len)
	local a0, b0, c0, d0 = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476
	for offset = 1, #msg, 64 do
		local w = { string.unpack("<I4I4I4I4I4I4I4I4I4I4I4I4I4I4I4I4", msg, offset) }
		local a, b, c, d = a0, b0, c0, d0
		for i = 0, 63 do
			local f, g
			if i < 16 then
				f = (b & c) | ((~b) & d)
				g = i
			elseif i < 32 then
				f = (d & b) | ((~d) & c)
				g = (5 * i + 1) % 16
			elseif i < 48 then
				f = b ~ c ~ d
				g = (3 * i + 5) % 16
			else
				f = c ~ (b | (~d))
				g = (7 * i) % 16
			end
			local temp = d
			d = c
			c = b
			local x = (a + f + MD5_K[i + 1] + w[g + 1]) & 0xffffffff
			local s = MD5_S[i + 1]
			b = (b + (((x << s) | (x >> (32 - s))) & 0xffffffff)) & 0xffffffff
			a = temp
		end
		a0 = (a0 + a) & 0xffffffff
		b0 = (b0 + b) & 0xffffffff
		c0 = (c0 + c) & 0xffffffff
		d0 = (d0 + d) & 0xffffffff
	end
	local raw = string.pack("<I4I4I4I4", a0, b0, c0, d0)
	return (raw:gsub(".", function(ch)
		return string.format("%02x", ch:byte(1))
	end))
end

local function is_video_job(job)
	local mime = job.mime or ""
	if mime:find("^video/") then
		return true
	end
	local ext = (job.file.url.ext or ""):lower()
	return VIDEO_EXTS[ext] or false
end

local function freedesktop_thumb_info(job)
	local path = tostring(job.file.path)
	local uri = path_to_file_uri(path)
	local digest = md5_hex(uri)
	local cha = (job.file.cha and job.file.cha.mtime and job.file.cha) or fs.cha(job.file.url)
	local mtime = cha and cha.mtime and math.floor(cha.mtime) or nil
	local xx_dir = THUMB_BASE_DIR .. "/xx-large"
	return {
		path = path,
		uri = uri,
		digest = digest,
		mtime = mtime,
		xx_dir = xx_dir,
		xx_large = xx_dir .. "/" .. digest .. ".png",
	}
end

local function is_valid_freedesktop_png(png_path, expected_mtime)
	local f = io.open(png_path, "rb")
	if not f then
		return false
	end
	local hdr = f:read(4096)
	f:close()
	if not hdr or #hdr < 33 or hdr:sub(1, 8) ~= "\x89PNG\r\n\x1a\n" then
		return false
	end
	if not expected_mtime then
		return true
	end
	local pos = 9
	while pos + 8 <= #hdr do
		local len = read_u32(hdr, pos, false)
		local ctype = hdr:sub(pos + 4, pos + 7)
		if not len or ctype == "IDAT" or ctype == "IEND" then
			break
		end
		if ctype == "tEXt" and pos + 7 + len <= #hdr then
			local payload = hdr:sub(pos + 8, pos + 7 + len)
			if payload:sub(1, 13) == "Thumb::MTime\0" then
				return tonumber(payload:sub(14)) == expected_mtime
			end
		end
		pos = pos + 12 + len
	end
	return false
end

local function find_valid_freedesktop_thumb(info)
	if is_valid_freedesktop_png(info.xx_large, info.mtime) then
		return Url(info.xx_large)
	end
	return nil
end

local function write_freedesktop_png_via_magick(src_path, info)
	fs.create("dir_all", Url(info.xx_dir))
	local nonce = math.random(100000, 999999) + math.floor((os.clock() * 1e6) % 1000000)
	local tmp_path = string.format("%s/.yazi.%d.png", info.xx_dir, nonce)
	local size = "1024x1024>"
	local escaped_uri = info.uri:gsub("%%", "%%%%")

	local cmd = require("magick").with_limit():arg({
		"-define",
		"filename:literal=true",
		"-define",
		"image:frames=0",
		"-define",
		"jpeg:size=2048x2048",
		src_path,
		"-auto-orient",
		"-strip",
	})

	if rt.preview.image_filter == "nearest" then
		cmd:arg({ "-sample", size })
	elseif rt.preview.image_filter == "catmull-rom" then
		cmd:arg({ "-filter", "catrom", "-thumbnail", size })
	elseif rt.preview.image_filter == "lanczos3" then
		cmd:arg({ "-filter", "lanczos", "-thumbnail", size })
	elseif rt.preview.image_filter == "gaussian" then
		cmd:arg({ "-filter", "gaussian", "-thumbnail", size })
	else
		cmd:arg({ "-filter", "triangle", "-thumbnail", size })
	end

	cmd:arg({
		"-define",
		"png:include-chunk=tEXt",
		"-quality",
		"15",
		"-set",
		"Thumb::URI",
		escaped_uri,
		"-set",
		"Thumb::MTime",
		tostring(info.mtime or 0),
		"-set",
		"Software",
		"Yazi",
		"PNG:" .. tmp_path,
	})

	local status, err = cmd:status()
	if not status or not status.success then
		os.remove(tmp_path)
		return nil, Err("Failed to write Freedesktop thumbnail: %s", err or (status and status.code) or "unknown")
	end
	if not os.rename(tmp_path, info.xx_large) then
		os.remove(tmp_path)
		return nil, Err("Failed to rename Freedesktop thumbnail into cache")
	end
	return Url(info.xx_large), nil
end

local function write_freedesktop_video_fallback(info)
	fs.create("dir_all", Url(info.xx_dir))
	local nonce = math.random(100000, 999999) + math.floor((os.clock() * 1e6) % 1000000)
	local raw_frame = string.format("%s/.yazi.vid.%d.jpg", info.xx_dir, nonce)
	local max_dim = math.max(rt.preview.max_width or 1024, rt.preview.max_height or 1024)

	local status = Command("ffmpegthumbnailer")
		:arg({ "-m", "-i", info.path, "-o", raw_frame, "-s", tostring(max_dim) })
		:status()
	if not status or not status.success then
		os.remove(raw_frame)
		return nil, Err("Failed to extract video thumbnail via ffmpegthumbnailer")
	end

	local res, err = write_freedesktop_png_via_magick(raw_frame, info)
	os.remove(raw_frame)
	return res, err
end

local function generate_freedesktop_thumb(job, info)
	local out = Command("allmytoes")
		:arg({ "-sxx", "-F", info.path })
		:stdout(Command.PIPED)
		:stderr(Command.PIPED)
		:output()
	if out and out.status and out.status.success then
		local thumb_path = trim(out.stdout)
		if thumb_path ~= "" then
			return Url(thumb_path), nil
		end
	end

	if is_video_job(job) then
		return write_freedesktop_video_fallback(info)
	end
	return write_freedesktop_png_via_magick(info.path, info)
end

function M:peek(job)
	-- Interactive video frame seeking (`J`/`K`) uses Yazi's built-in video previewer
	if job.skip and job.skip > 0 and is_video_job(job) then
		return require("video"):peek(job)
	end

	local start = os.clock()
	ya.sleep(math.max(0, rt.preview.image_delay / 1000 + start - os.clock()))

	local info = freedesktop_thumb_info(job)
	local thumb = find_valid_freedesktop_thumb(info)
	if thumb then
		local _, show_err = ya.image_show(thumb, job.area)
		ya.preview_widget(job, show_err)
		return
	end

	local ext = (job.file.url.ext or ""):lower()
	local is_vid = is_video_job(job)

	if not is_vid and not MAGICK_ONLY_EXTS[ext] then
		local _, show_err = ya.image_show(job.file.url, job.area)
		ya.preview_widget(job, show_err)
		generate_freedesktop_thumb(job, info)
		return
	end

	local generated, gen_err = generate_freedesktop_thumb(job, info)
	if not generated then
		return ya.preview_widget(job, gen_err)
	end
	local _, show_err = ya.image_show(generated, job.area)
	ya.preview_widget(job, show_err)
end

function M:seek(job)
	if is_video_job(job) then
		return require("video"):seek(job)
	end
end

function M:preload(job)
	if job.skip and job.skip > 0 and is_video_job(job) then
		return require("video"):preload(job)
	end

	local info = freedesktop_thumb_info(job)
	if find_valid_freedesktop_thumb(info) then
		return true
	end

	local thumb, err = generate_freedesktop_thumb(job, info)
	if not thumb then
		return true, err
	end
	return true
end

function M.thumb_info(job)
	return freedesktop_thumb_info(job)
end

function M.find_valid_thumb(info)
	return find_valid_freedesktop_thumb(info)
end

function M.write_png_via_magick(src_path, info)
	return write_freedesktop_png_via_magick(src_path, info)
end

function M:spot(job)
	return require("exif-spotter"):spot(job)
end

return M
