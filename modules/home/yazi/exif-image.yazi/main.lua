--- Zero-subprocess Camera RAW previewer/preloader (ported from peng)
--- + on-demand MediaInfo Spotter (<Tab>) for images, RAWs, audio, and video.
local M = {}

local SKIP_KEYS = {
	["Complete name"] = true,
	["CompleteName_Last"] = true,
	["Unique ID"] = true,
	["ImageUniqueID"] = true,
	["Format/Info"] = true,
	["Codec ID/Info"] = true,
	["MD5 of the unencoded content"] = true,
	["Conformance errors"] = true,
	["0xFFE1"] = true,
	["General compliance"] = true,
}

local VALID_TIFF_VERSIONS = {
	[42] = true, -- Standard TIFF (.DNG, .ARW, .CR2, .NEF, .PEF)
	[0x4F52] = true, -- Olympus .ORF ("IIRO")
	[0x5352] = true, -- Olympus .ORF ("IIRS")
	[0x0055] = true, -- Panasonic .RW2
}

local function trim(s)
	return s and s:match("^%s*(.-)%s*$") or ""
end

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
--- (>= 1200px or >= 350KB) if multiple large previews exist (e.g. Leica DNG 2112x1408 vs 5248x3472).
local function select_best_candidate(valid)
	local best = valid[1]
	for _, c in ipairs(valid) do
		if c.len > best.len then
			best = c
		end
	end
	local target_dim = math.max(rt.preview.max_width or 1200, rt.preview.max_height or 1200)
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

local function read_raf_meta_block(path)
	local f = io.open(path, "rb")
	if not f then
		return nil
	end
	local hdr = f:read(128)
	if not hdr or #hdr < 100 or hdr:sub(1, 8) ~= "FUJIFILM" then
		f:close()
		return nil
	end
	local meta_off = read_u32(hdr, 93, false)
	local meta_len = read_u32(hdr, 97, false)
	if not meta_off or not meta_len or meta_off == 0 or meta_len == 0 or meta_len > 65536 then
		f:close()
		return nil
	end
	local meta = f:seek("set", meta_off) and f:read(meta_len) or nil
	f:close()
	return meta
end

local function parse_fuji_raf_sensor_dims(path)
	local meta = read_raf_meta_block(path)
	if not meta or #meta < 4 then
		return nil
	end
	local count = read_u32(meta, 1, false) or 0
	local pos = 5
	local dims = {}
	for _ = 1, count do
		if pos + 4 > #meta then
			break
		end
		local tag_id = read_u16(meta, pos, false)
		local tag_len = read_u16(meta, pos + 2, false)
		pos = pos + 4
		if not tag_len or pos + tag_len - 1 > #meta then
			break
		end
		if tag_len >= 4 and (tag_id == 0x0111 or tag_id == 0x0121 or tag_id == 0x0100) then
			local h = read_u16(meta, pos, false)
			local w = read_u16(meta, pos + 2, false)
			if w and h and w > 0 and h > 0 then
				dims[tag_id] = { w = w, h = h }
			end
		end
		pos = pos + tag_len
	end
	return dims[0x0111] or dims[0x0121] or dims[0x0100]
end

local function is_valid_jpeg_cache(cache_url)
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

function M:peek(job)
	local start, cache = os.clock(), ya.file_cache(job)
	if not cache then
		return
	end

	-- Debounce before performing synchronous NFS I/O so rapid scrolling cancels the coroutine early
	ya.sleep(math.max(0, rt.preview.image_delay / 1000 + start - os.clock()))

	if not is_valid_jpeg_cache(cache) then
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
	local cache = ya.file_cache(job)
	if not cache or is_valid_jpeg_cache(cache) then
		return true
	end

	local jpg, err = extract_raw_preview(tostring(job.file.path))
	if not jpg then
		return true, Err("RAW preview extraction failed: %s", err or "unknown error")
	end

	if not write_atomic(tostring(cache), jpg) then
		return false, Err("Failed to write RAW preview cache")
	end
	return true
end

function M:spot(job)
	local rows = self:spot_base(job)
	if #rows > 0 then
		rows[#rows + 1] = ui.Row({})
	end

	ya.spot_table(
		job,
		ui.Table(ya.list_merge(rows, require("file"):spot_base(job)))
			:area(ui.Pos({ "center", w = 74, h = 26 }))
			:row(job.skip)
			:col(1)
			:col_style(th.spot.tbl_col)
			:cell_style(th.spot.tbl_cell)
			:widths({ ui.Constraint.Length(24), ui.Constraint.Fill(1) })
	)
end

local function append_mediainfo_kv(cur_sec, raw_k, raw_v)
	if not cur_sec then
		return
	end
	local k, v = trim(raw_k), trim(raw_v)
	if SKIP_KEYS[k] or v == "" then
		return
	end
	if k == "Type" and v == "Thumbnail" then
		cur_sec.is_thumb = true
	end
	if k == "Stream size" and v:find("^2%.00 Bytes") then
		return
	end
	cur_sec.rows[#cur_sec.rows + 1] = { k, v }
end

local function parse_mediainfo_sections(stdout)
	local sections = {}
	local cur_sec = nil
	for line in stdout:gmatch("[^\r\n]+") do
		local k, v = line:match("^(.-)%s+:%s+(.+)$")
		if k and v then
			append_mediainfo_kv(cur_sec, k, v)
		else
			local sec_name = trim(line)
			if sec_name ~= "" then
				cur_sec = { name = sec_name, rows = {}, is_thumb = false }
				sections[#sections + 1] = cur_sec
			end
		end
	end
	return sections
end

local function append_section_rows(rows, sec, is_raf, raf_dims, raf_lens, file_len)
	if sec.is_thumb or #sec.rows == 0 then
		return
	end
	if #rows > 0 then
		rows[#rows + 1] = ui.Row({})
	end
	local title = sec.name:gsub("^Image #1$", "Image")
	if is_raf and title == "Image" then
		title = "Embedded Preview"
	end
	rows[#rows + 1] = ui.Row({ title }):style(ui.Style():fg("green"))

	if is_raf and sec.name == "General" then
		rows[#rows + 1] = ui.Row({ "  Format:", "Fujifilm RAF" })
		if file_len then
			rows[#rows + 1] = ui.Row({ "  File size:", ya.readable_size(file_len) })
		end
		if raf_dims then
			rows[#rows + 1] = ui.Row({
				"  Sensor size:",
				string.format("%d x %d pixels", raf_dims.w, raf_dims.h),
			})
		end
		if raf_lens ~= "" then
			rows[#rows + 1] = ui.Row({ "  LensModel:", raf_lens })
		end
	end

	for _, kv in ipairs(sec.rows) do
		local k, v = kv[1], kv[2]
		local skip_raf_general = (is_raf and sec.name == "General" and (k == "File size" or k == "Format"))
		if not skip_raf_general then
			rows[#rows + 1] = ui.Row({ "  " .. k .. ":", v })
		end
	end
end

function M:spot_base(job)
	local path = tostring(job.file.path)
	local ext = (job.file.url.ext or ""):lower()
	local is_raf = (ext == "raf")

	local target_path = path
	local raf_dims = nil
	local raf_lens = ""

	-- MediaInfo does not parse the outer 148-byte FUJIFILMCCD-RAW header of .RAF files,
	-- so we feed MediaInfo and exiv2 the cached embedded JPEG preview (which contains the full Fuji EXIF)
	-- and read the raw sensor dimensions directly from the RAF metadata block.
	if is_raf then
		raf_dims = parse_fuji_raf_sensor_dims(path)
		self:preload(job)
		local cache = ya.file_cache(job)
		if cache and is_valid_jpeg_cache(cache) then
			target_path = tostring(cache)
		end
		local lens_out = Command("exiv2")
			:arg({ "-q", "-K", "Exif.Photo.LensModel", "-Pt", target_path })
			:output()
		raf_lens = lens_out and trim(lens_out.stdout:match("^[^\r\n]+")) or ""
	end

	local out = Command("mediainfo"):arg({ target_path }):output()
	local sections = out and out.stdout and parse_mediainfo_sections(out.stdout) or {}
	local file_len = job.file.cha and job.file.cha.len or nil

	local rows = {}
	for _, sec in ipairs(sections) do
		append_section_rows(rows, sec, is_raf, raf_dims, raf_lens, file_len)
	end

	-- Fallback to ya.image_info if mediainfo returned nothing
	if #rows == 0 then
		local cache = ya.file_cache(job)
		local info = (cache and is_valid_jpeg_cache(cache) and ya.image_info(cache))
			or ya.image_info(job.file.url)
		if info then
			rows[#rows + 1] = ui.Row({ "Image" }):style(ui.Style():fg("green"))
			rows[#rows + 1] = ui.Row({ "  Format:", tostring(info.format) })
			rows[#rows + 1] = ui.Row({ "  Size:", string.format("%dx%d", info.w, info.h) })
			rows[#rows + 1] = ui.Row({ "  Color:", tostring(info.color) })
		end
	end

	return rows
end

return M
