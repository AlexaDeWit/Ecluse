-- Expand a ```threat-register fence into the project's threat register, rendered
-- from the threatcl model at `make site` time. The model
-- (threat-modelling/ecluse.hcl) is the single source of truth.

local DEFAULT_PATH = "threat-modelling/ecluse.hcl"

local function read_model_json(path)
  local f = assert(io.popen("threatcl export -format=json " .. path, "r"))
  local raw = f:read("a")
  f:close()
  return pandoc.json.decode(raw)
end

local function collect(model_array)
  local threats = {}
  for _, m in ipairs(model_array or {}) do
    for _, t in ipairs(m.threat or {}) do
      threats[#threats + 1] = t
    end
  end
  return threats
end

-- A coloured pill: <span class="badge severity-high">High</span>, styled in style.css.
local function badge(text, kind, value)
  local slug = (value or ""):lower():gsub("[^%w]+", "")
  return pandoc.Span({ pandoc.Str(text) }, pandoc.Attr("", { "badge", kind .. "-" .. slug }))
end

local function status_text(s) return s == "NA" and "N/A" or (s or "") end

local function slugify(name)
  return (name or ""):lower():gsub("[^%w]+", "-"):gsub("-$", "")
end

-- One scannable cell's worth of inlines wrapped as a block.
local function plain(inlines) return { pandoc.Plain(inlines) } end

-- The overview table: a row per threat, the number linking down to its detail.
local function overview_table(threats)
  local headers = {
    plain({ pandoc.Str("#") }),
    plain({ pandoc.Str("Status") }),
    plain({ pandoc.Str("Category") }),
    plain({ pandoc.Str("Threat") }),
    plain({ pandoc.Str("Element") }),
  }
  local rows = {}
  for i, t in ipairs(threats) do
    local num = tostring(i)
    local anchor = slugify(t.name)
    local status = (#(t.expandedControl or {}) > 0) and "Mitigated" or "Open"
    local category = (t.impacts and t.impacts[1]) or "—"
    local element = "System"
    
    rows[#rows + 1] = {
      plain({ pandoc.Link({ pandoc.Str(num) }, "#" .. anchor) }),
      plain({ badge(status_text(status), "status", status) }),
      plain({ pandoc.Str(category) }),
      plain({ pandoc.Str(t.name or "") }),
      plain({ pandoc.Emph({ pandoc.Str(element) }) }),
    }
  end
  local aligns = {
    pandoc.AlignRight, pandoc.AlignLeft, pandoc.AlignLeft,
    pandoc.AlignLeft, pandoc.AlignLeft,
  }
  local widths = { 0, 0, 0, 0, 0 }
  return pandoc.utils.from_simple_table(
    pandoc.SimpleTable({}, aligns, widths, headers, rows))
end

local function md_inlines(s)
  return pandoc.utils.blocks_to_inlines(pandoc.read(s, "markdown").blocks)
end

local function labelled_para(label, body)
  local inlines = { pandoc.Strong({ pandoc.Str(label) }), pandoc.Space() }
  for _, i in ipairs(md_inlines(body)) do inlines[#inlines + 1] = i end
  return pandoc.Para(inlines)
end

-- Per-threat detail: an anchored heading, a badge line, then description + mitigation.
local function detail_blocks(threats)
  local blocks = {}
  for i, t in ipairs(threats) do
    local num = tostring(i)
    local anchor = slugify(t.name)
    local status = (#(t.expandedControl or {}) > 0) and "Mitigated" or "Open"
    local category = (t.impacts and t.impacts[1]) or "—"
    local element = "System"
    
    blocks[#blocks + 1] = pandoc.Header(3,
      { pandoc.Str(num .. ". " .. (t.name or "")) },
      pandoc.Attr(anchor, { "threat" }))
    blocks[#blocks + 1] = pandoc.Para({
      badge(status_text(status), "status", status), pandoc.Space(),
      pandoc.Str(category), pandoc.Space(),
      pandoc.Str("·"), pandoc.Space(),
      pandoc.Emph({ pandoc.Str(element) }),
    })
    if t.description and t.description ~= "" then
      blocks[#blocks + 1] = labelled_para("Threat.", t.description)
    end
    
    if t.expandedControl and #t.expandedControl > 0 then
      local mitigation_texts = {}
      for _, ctrl in ipairs(t.expandedControl) do
        mitigation_texts[#mitigation_texts+1] = "**" .. ctrl.name .. "**: " .. (ctrl.description or "")
      end
      blocks[#blocks + 1] = labelled_para("Mitigation.", table.concat(mitigation_texts, "\n\n"))
    end
  end
  return blocks
end

function CodeBlock(el)
  if not el.classes:includes("threat-register") then
    return nil
  end
  local path = el.text:gsub("%s+", "")
  if path == "" then path = DEFAULT_PATH end

  local model_array = read_model_json(path)
  local threats = collect(model_array)

  local f2 = assert(io.popen("threatcl mermaid " .. path, "r"))
  local mermaid_raw = f2:read("a")
  f2:close()

  local out_blocks = {}

  if mermaid_raw and mermaid_raw:match("%S") then
    local escaped = mermaid_raw:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
    out_blocks[#out_blocks + 1] = pandoc.Header(2, { pandoc.Str("Diagrams") })
    out_blocks[#out_blocks + 1] = pandoc.RawBlock("html", '<pre class="mermaid">\n' .. escaped .. '</pre>')
  end

  out_blocks[#out_blocks + 1] = pandoc.Header(2, { pandoc.Str("Threat register") })
  out_blocks[#out_blocks + 1] = overview_table(threats)
  
  out_blocks[#out_blocks + 1] = pandoc.Header(2, { pandoc.Str("Threat detail") })
  for _, b in ipairs(detail_blocks(threats)) do out_blocks[#out_blocks + 1] = b end

  return out_blocks
end
