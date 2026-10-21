--- Zero-subprocess Camera RAW previewer/preloader (ported from peng).
--- Extracts embedded JPEG previews from .RAF and TIFF-based RAW containers
--- (.DNG, .ARW, .CR2, .NEF, .ORF, .RW2, .PEF) into Yazi's preview cache.
local M = {}

local VALID_TIFF_VERSIONS = {
	[42] = true, -- Standard TIFF (.DNG, .ARW, .CR2, .NEF, .PEF)
	[0x4F52] = true, -- Olympus .ORF ("IIRO")
	[0x5352] = true, -- Olympus .ORF ("IIRS")
	[0x0055] = true, -- Panasonic .RW2
}

-- NOTE: `read_u16` and `read_u32` are also in `exif-spotter.yazi/main.lua`
-- (and `read_u32` in `freedesktop-thumb.yazi/main.lua`). If modified here, update those as well.
local function read_u16(buf, pos, is_le)
	local b1, b2 = buf:byte(pos, pos + 1)
	if not b2 then
		return nil
	end
	return is_le and (b1 + b2 * 256) or (b1 * 256 + b2)
end

local function read_u32(buf, pos, is_le)
	local b1, b2, b3, b4 = buf:byte(pos, pos + 3)
	if not b4 then
		return nil
	end
	return is_le and (b1 + b2 * 256 + b3 * 65536 + b4 * 16777216)
		or (b1 * 16777216 + b2 * 65536 + b3 * 256 + b4)
end

local function read_at(f, hdr, off0, len)
	if off0 >= 0 and (off0 + len) <= #hdr then
		return hdr:sub(off0 + 1, off0 + len)
	end
	if not f:seek("set", off0) then
		return nil
	end
	return f:read(len)
end

local function is_lossless_jpeg(hdr_bytes)
	if not hdr_bytes or #hdr_bytes < 4 then
		return true
	end
	local b1, b2 = hdr_bytes:byte(1, 2)
	if b1 ~= 0xFF or b2 ~= 0xD8 then
		return true
	end
	local idx = 3
	while idx + 1 <= #hdr_bytes do
		if hdr_bytes:byte(idx) ~= 0xFF then
			break
		end
		local m_off = idx + 1
		while m_off <= #hdr_bytes and hdr_bytes:byte(m_off) == 0xFF do
			m_off = m_off + 1
		end
		if m_off > #hdr_bytes then
			break
		end
		local marker = hdr_bytes:byte(m_off)
		idx = m_off + 1
		if marker == 0x00 or marker == 0xD9 or marker == 0xDA then
			break
		end
		if marker == 0xC3 then
			return true
		end
		local is_standalone = (marker == 0xD8 or (marker >= 0xD0 and marker <= 0xD7))
		if not is_standalone then
			local seg_len = read_u16(hdr_bytes, idx, false)
			if not seg_len or seg_len < 2 then
				break
			end
			idx = idx + seg_len
		end
	end
	return false
end

--- Propagates outer TIFF IFD0 Orientation to the extracted JPEG preview by patching
--- an existing EXIF 0x0112 tag in-place or injecting a minimal 36-byte APP1 header.
local function apply_jpeg_orientation(jpg, orientation)
	if not orientation or orientation <= 1 then
		return jpg
	end
	local exif_pos = jpg:sub(1, 4096):find("Exif\0\0", 1, true)
	if not exif_pos then
		local app1 = "\xff\xe1\x00\x22Exif\x00\x00II\x2a\x00\x08\x00\x00\x00\x01\x00\x12\x01\x03\x00\x01\x00\x00\x00"
			.. string.char(orientation, 0, 0, 0)
			.. "\x00\x00\x00\x00"
		return jpg:sub(1, 2) .. app1 .. jpg:sub(3)
	end

	local tiff_base = exif_pos + 6
	local emagic = jpg:sub(tiff_base, tiff_base + 1)
	local ele = (emagic == "II")
	if not (ele or emagic == "MM") or read_u16(jpg, tiff_base + 2, ele) ~= 42 then
		return jpg
	end
	local ifd_rel = read_u32(jpg, tiff_base + 4, ele)
	local ifd_pos = ifd_rel and (tiff_base + ifd_rel) or nil
	local n_ent = ifd_pos and read_u16(jpg, ifd_pos, ele) or nil
	if not n_ent or n_ent == 0 or n_ent > 128 then
		return jpg
	end
	for i = 0, n_ent - 1 do
		local epos = ifd_pos + 2 + i * 12
		if read_u16(jpg, epos, ele) == 0x0112 then
			local val_bytes = ele and string.char(orientation, 0) or string.char(0, orientation)
			return jpg:sub(1, epos + 7) .. val_bytes .. jpg:sub(epos + 10)
		end
	end
	return jpg
end

local function append_sub_ifds(ctx, cnt, val_u32)
	if not cnt or cnt == 0 or cnt > 16 or not val_u32 then
		return
	end
	if cnt == 1 then
		ctx.sub_ifds[#ctx.sub_ifds + 1] = val_u32
		return
	end
	local sb = read_at(ctx.f, ctx.hdr, val_u32, cnt * 4)
	if not sb or #sb < cnt * 4 then
		return
	end
	for k = 0, cnt - 1 do
		ctx.sub_ifds[#ctx.sub_ifds + 1] = read_u32(sb, k * 4 + 1, ctx.is_le)
	end
end

local function parse_tiff_ifd(ctx, ifd_off, depth, collect_subs)
	if not ifd_off or ifd_off < 8 then
		return 0
	end
	local num_b = read_at(ctx.f, ctx.hdr, ifd_off, 2)
	local num = num_b and read_u16(num_b, 1, ctx.is_le)
	if not num or num == 0 or num > 512 then
		return 0
	end
	local entries_b = read_at(ctx.f, ctx.hdr, ifd_off + 2, num * 12 + 4)
	if not entries_b or #entries_b < num * 12 then
		return 0
	end

	local is_le = ctx.is_le
	local j_off, j_len, strip_off, strip_len
	local comp, photo, subfile, width, height
	local is_cr2_raw = false

	for i = 0, num - 1 do
		local pos = i * 12 + 1
		local tag = read_u16(entries_b, pos, is_le)
		local typ = read_u16(entries_b, pos + 2, is_le)
		local cnt = read_u32(entries_b, pos + 4, is_le)
		local val_u16 = read_u16(entries_b, pos + 8, is_le)
		local val_u32 = read_u32(entries_b, pos + 8, is_le)
		local val = (typ == 3 and cnt == 1) and val_u16 or val_u32

		if tag == 0x00FE then
			subfile = val
		elseif tag == 0x0100 then
			width = val
		elseif tag == 0x0101 then
			height = val
		elseif tag == 0x0103 then
			comp = val_u16
		elseif tag == 0x0106 then
			photo = val_u16
		elseif tag == 0x0112 and depth == 0 then
			ctx.orientation = val_u16 or 1
		elseif tag == 0x002E and cnt and cnt > 0 and val_u32 then
			-- Panasonic .RW2 JpgFromRaw
			ctx.cands[#ctx.cands + 1] = { off = val_u32, len = cnt, w = 0, h = 0 }
		elseif tag == 0xC5D8 or tag == 0xC5D9 or tag == 0xC5E0 or tag == 0xC640 then
			is_cr2_raw = true
		elseif tag == 0x0111 and cnt == 1 then
			strip_off = val
		elseif tag == 0x0117 and cnt == 1 then
			strip_len = val
		elseif tag == 0x0201 then
			j_off = val_u32
		elseif tag == 0x0202 then
			j_len = val_u32
		elseif tag == 0x014A and collect_subs then
			append_sub_ifds(ctx, cnt, val_u32)
		end
	end

	local skip_cr2_depth = (ctx.is_cr2 and depth >= 2)
	if j_off and j_len and j_off > 0 and j_len > 0 and not skip_cr2_depth then
		-- Do not attach IFD width/height to 0x0201 (JPEGInterchangeFormat), as IFD0 may
		-- report full sensor dimensions alongside a small thumbnail.
		ctx.cands[#ctx.cands + 1] = { off = j_off, len = j_len, w = 0, h = 0 }
	elseif strip_off and strip_len and strip_off > 0 and strip_len > 0 and not skip_cr2_depth and not is_cr2_raw then
		local is_jpeg_comp = (comp == 6 or comp == 7)
		local is_bayer = (photo == 32803 or photo == 34892)
		local is_reduced = (subfile == nil or (subfile % 2 == 1) or subfile == 0x10000 or subfile == 0x10001)
		if is_jpeg_comp and not is_bayer and is_reduced then
			ctx.cands[#ctx.cands + 1] = { off = strip_off, len = strip_len, w = width or 0, h = height or 0 }
		end
	end

	if #entries_b < num * 12 + 4 then
		return 0
	end
	return read_u32(entries_b, num * 12 + 1, is_le) or 0
end

--- Picks the largest valid JPEG preview first, then prefers a medium-high-res preview
--- (>= 1024px or >= 350KB) if multiple large previews exist (e.g. Leica DNG 2112x1408 vs 5248x3472).
local function select_best_candidate(valid)
	local best = valid[1]
	for _, c in ipairs(valid) do
		if c.len > best.len then
			best = c
		end
	end
	local target_dim = math.max(rt.preview.max_width or 1024, rt.preview.max_height or 1024)
	for _, c in ipairs(valid) do
		local max_dim = math.max(c.w, c.h)
		local is_large_enough = (max_dim >= target_dim or (max_dim == 0 and c.len >= 350000))
		if is_large_enough and c.len < best.len and c.len >= 50000 then
			best = c
		end
	end
	return best
end

local function extract_raw_from_file(f)
	local hdr = f:read(262144)
	if not hdr or #hdr < 16 then
		return nil, "File too small"
	end

	-- 1. Fujifilm RAF container ("FUJIFILM" magic; preview offset @ 84, length @ 88 big-endian)
	if hdr:sub(1, 8) == "FUJIFILM" and #hdr >= 92 then
		local prev_off = read_u32(hdr, 85, false)
		local prev_len = read_u32(hdr, 89, false)
		if not prev_off or not prev_len or prev_off == 0 or prev_len == 0 then
			return nil, "Invalid preview offset in RAF container"
		end
		local jpg = read_at(f, hdr, prev_off, prev_len)
		if jpg and #jpg >= 4 and jpg:byte(1) == 0xFF and jpg:byte(2) == 0xD8 then
			return jpg, nil
		end
		return nil, "Invalid embedded JPEG in RAF container"
	end

	-- 2. TIFF-based RAW container (.DNG, .ARW, .CR2, .NEF, .ORF, .RW2, .PEF)
	local magic = hdr:sub(1, 2)
	local is_le = (magic == "II")
	local tiff_ver = (is_le or magic == "MM") and read_u16(hdr, 3, is_le) or nil
	if not VALID_TIFF_VERSIONS[tiff_ver] then
		return nil, "No embedded JPEG preview found in RAW file"
	end

	local ctx = {
		f = f,
		hdr = hdr,
		is_le = is_le,
		is_cr2 = (hdr:sub(9, 12) == "CR\x02\x00"),
		cands = {},
		sub_ifds = {},
		orientation = 1,
	}

	local cur = read_u32(hdr, 5, is_le)
	local visited = {}
	for depth = 0, 7 do
		if not cur or cur == 0 or visited[cur] then
			break
		end
		visited[cur] = true
		cur = parse_tiff_ifd(ctx, cur, depth, depth == 0)
	end
	for _, s_off in ipairs(ctx.sub_ifds) do
		if s_off and s_off > 0 and not visited[s_off] then
			visited[s_off] = true
			parse_tiff_ifd(ctx, s_off, 1, false)
		end
	end

	local valid = {}
	for _, c in ipairs(ctx.cands) do
		local probe = read_at(f, hdr, c.off, math.min(c.len, 1024))
		if probe and not is_lossless_jpeg(probe) then
			valid[#valid + 1] = c
		end
	end
	if #valid == 0 then
		return nil, "No embedded JPEG preview found in RAW file"
	end

	local best = select_best_candidate(valid)
	local jpg = read_at(f, hdr, best.off, best.len)
	if not jpg or #jpg < 4 then
		return nil, "Failed to read embedded JPEG preview"
	end
	return apply_jpeg_orientation(jpg, ctx.orientation), nil
end

--- Extracts the embedded preview JPEG from a Camera RAW container (.RAF, .DNG, .ARW, .CR2, .NEF, .ORF, .RW2, .PEF)
--- using positional reads on the first 256KB header + preview slice without reading the full RAW sensor data.
local function extract_raw_preview(path)
	local f, err = io.open(path, "rb")
	if not f then
		return nil, tostring(err)
	end
	local jpg, extract_err = extract_raw_from_file(f)
	f:close()
	return jpg, extract_err
end

local IS_LINUX = (ya.target_os() == "linux")

function M.is_valid_jpeg_cache(cache_url)
	local cha = fs.cha(cache_url)
	if not cha or cha.len < 4 then
		return false
	end
	local f = io.open(tostring(cache_url), "rb")
	if not f then
		return false
	end
	local magic = f:read(2)
	f:close()
	return magic == "\xff\xd8"
end

local function write_atomic(target_str, data)
	local nonce = math.random(100000, 999999) + math.floor((os.clock() * 1e6) % 1000000)
	local tmp_str = string.format("%s.%d.tmp", target_str, nonce)
	if not fs.write(Url(tmp_str), data) then
		return false
	end
	if not os.rename(tmp_str, target_str) then
		os.remove(tmp_str)
		return false
	end
	return true
end

--- Ensures the unstripped embedded JPEG is extracted into `ya.file_cache(job)` (used for
--- instant first-hover previews, `.RAF` `<Tab>` EXIF spotting, and as input for downscaling).
function M.ensure_extracted_jpeg(job)
	local cache = ya.file_cache(job)
	if not cache then
		return nil, Err("Preview cache disabled")
	end
	local raw_jpg_url = Url(tostring(cache) .. ".raw.jpg")
	if M.is_valid_jpeg_cache(raw_jpg_url) then
		return raw_jpg_url, nil
	end

	local jpg, err = extract_raw_preview(tostring(job.file.path))
	if not jpg then
		return nil, Err("RAW preview extraction failed: %s", err or "unknown error")
	end

	if not write_atomic(tostring(raw_jpg_url), jpg) then
		return nil, Err("Failed to write RAW preview cache")
	end
	return raw_jpg_url, nil
end

function M:peek(job)
	local start = os.clock()

	-- Debounce before performing synchronous NFS I/O so rapid scrolling cancels the coroutine early
	ya.sleep(math.max(0, rt.preview.image_delay / 1000 + start - os.clock()))

	if IS_LINUX then
		local fd = require("freedesktop-thumb")
		local info = fd.thumb_info(job)
		local thumb = fd.find_valid_thumb(info)
		if thumb then
			local _, show_err = ya.image_show(thumb, job.area)
			return ya.preview_widget(job, show_err)
		end

		-- Cache miss: extract and show the embedded JPEG immediately, then write the 1024px Freedesktop PNG
		local raw_jpg_url, err = M.ensure_extracted_jpeg(job)
		if not raw_jpg_url then
			return ya.preview_widget(job, err)
		end
		local _, show_err = ya.image_show(raw_jpg_url, job.area)
		ya.preview_widget(job, show_err)
		fd.write_png_via_magick(tostring(raw_jpg_url), info)
		return
	end

	local cache = ya.file_cache(job)
	if not cache then
		return
	end
	if not M.is_valid_jpeg_cache(cache) then
		local ok, err = self:preload(job)
		if not ok or err then
			return ya.preview_widget(job, err or Err("Failed to write RAW preview cache"))
		end
	end

	local _, show_err = ya.image_show(cache, job.area)
	ya.preview_widget(job, show_err)
end

function M:seek() end

function M:preload(job)
	if IS_LINUX then
		local fd = require("freedesktop-thumb")
		local info = fd.thumb_info(job)
		if fd.find_valid_thumb(info) then
			return true
		end
		local raw_jpg_url, err = M.ensure_extracted_jpeg(job)
		if not raw_jpg_url then
			return true, err
		end
		local thumb, png_err = fd.write_png_via_magick(tostring(raw_jpg_url), info)
		if not thumb then
			return true, png_err
		end
		return true
	end

	local cache = ya.file_cache(job)
	if not cache or M.is_valid_jpeg_cache(cache) then
		return true
	end

	local raw_jpg_url, err = M.ensure_extracted_jpeg(job)
	if not raw_jpg_url then
		return true, err
	end
	return ya.image_precache(raw_jpg_url, cache)
end

function M:spot(job)
	return require("exif-spotter"):spot(job)
end

return M
