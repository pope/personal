--- On-demand MediaInfo + exiv2 Spotter (<Tab>) for images, Camera RAWs, audio, and video.
--- Uses `raw-image` to extract embedded JPEG previews for Fujifilm .RAF files.
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

local function trim(s)
	return s and s:match("^%s*(.-)%s*$") or ""
end

-- NOTE: `read_u16` and `read_u32` are also in `raw-image.yazi/main.lua`
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

function M:spot_base(job)
	local path = tostring(job.file.path)
	local ext = (job.file.url.ext or ""):lower()
	local is_raf = (ext == "raf")

	local target_path = path
	local raf_dims = nil
	local raf_lens = ""
	local raw_image = require("raw-image")

	-- MediaInfo does not parse the outer 148-byte FUJIFILMCCD-RAW header of .RAF files,
	-- so we feed MediaInfo and exiv2 the cached embedded JPEG preview (which contains the full Fuji EXIF)
	-- and read the raw sensor dimensions directly from the RAF metadata block.
	if is_raf then
		raf_dims = parse_fuji_raf_sensor_dims(path)
		local raw_jpg_url = raw_image.ensure_extracted_jpeg(job)
		if raw_jpg_url then
			target_path = tostring(raw_jpg_url)
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
		local info = (cache and raw_image.is_valid_jpeg_cache(cache) and ya.image_info(cache))
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
