-- Expand a ```threat-register fence into the project's threat register, rendered
-- from the OWASP Threat Dragon model at `make site` time. The model
-- (threat-modelling/ecluse.json) is the single source of truth: the register is
-- generated on every Pages build and never hand-copied into prose, so the docs
-- cannot drift from the model.
--
-- This mirrors web/mermaid.lua — a CodeBlock carrying a known class is replaced
-- during the pandoc pass. No commit-back and no extra dependency: pandoc (>= 3.1)
-- decodes the JSON itself via pandoc.json.decode. The owning fence may name an
-- alternate model path as its body; an empty fence uses the default below.

local DEFAULT_PATH = "threat-modelling/ecluse.json"

local function read_model(path)
  local f = assert(io.open(path, "r"), "threat-register: cannot open model at " .. path)
  local raw = f:read("a")
  f:close()
  return pandoc.json.decode(raw)
end

-- Flatten every threat across all diagrams, tagging each with the diagram element
-- it hangs off (a Threat Dragon threat is nested under the cell it threatens).
local function collect(model)
  local threats = {}
  for _, diagram in ipairs(model.detail.diagrams or {}) do
    for _, cell in ipairs(diagram.cells or {}) do
      local data = cell.data
      if data and data.threats then
        for _, t in ipairs(data.threats) do
          t._element = data.name or "(unnamed)"
          threats[#threats + 1] = t
        end
      end
    end
  end
  table.sort(threats, function(a, b) return (a.number or 0) < (b.number or 0) end)
  return threats
end

-- A coloured pill: <span class="badge severity-high">High</span>, styled in style.css.
local function badge(text, kind, value)
  local slug = (value or ""):lower():gsub("[^%w]+", "")
  return pandoc.Span({ pandoc.Str(text) }, pandoc.Attr("", { "badge", kind .. "-" .. slug }))
end

local function status_text(s) return s == "NA" and "N/A" or (s or "") end

-- Threat Dragon numbers decode as Lua floats; render them as plain integers.
local function numstr(t)
  if t.number == nil then return "?" end
  return string.format("%d", t.number)
end

-- One scannable cell's worth of inlines wrapped as a block.
local function plain(inlines) return { pandoc.Plain(inlines) } end

-- The overview table: a row per threat, the number linking down to its detail.
local function overview_table(threats)
  local headers = {
    plain({ pandoc.Str("#") }),
    plain({ pandoc.Str("Severity") }),
    plain({ pandoc.Str("Status") }),
    plain({ pandoc.Str("Category") }),
    plain({ pandoc.Str("Threat") }),
    plain({ pandoc.Str("Element") }),
  }
  local rows = {}
  for _, t in ipairs(threats) do
    local num = numstr(t)
    rows[#rows + 1] = {
      plain({ pandoc.Link({ pandoc.Str(num) }, "#threat-" .. num) }),
      plain({ badge(t.severity or "?", "severity", t.severity) }),
      plain({ badge(status_text(t.status), "status", t.status) }),
      plain({ pandoc.Str(t.type or "—") }),
      plain({ pandoc.Str(t.title or "") }),
      plain({ pandoc.Emph({ pandoc.Str(t._element) }) }),
    }
  end
  local aligns = {
    pandoc.AlignRight, pandoc.AlignLeft, pandoc.AlignLeft,
    pandoc.AlignLeft, pandoc.AlignLeft, pandoc.AlignLeft,
  }
  local widths = { 0, 0, 0, 0, 0, 0 }
  return pandoc.utils.from_simple_table(
    pandoc.SimpleTable({}, aligns, widths, headers, rows))
end

-- Per-threat detail: an anchored heading, a badge line, then description + mitigation.
local function detail_blocks(threats)
  local blocks = {}
  for _, t in ipairs(threats) do
    local num = numstr(t)
    blocks[#blocks + 1] = pandoc.Header(3,
      { pandoc.Str(num .. ". " .. (t.title or "")) },
      pandoc.Attr("threat-" .. num, { "threat" }))
    blocks[#blocks + 1] = pandoc.Para({
      badge(t.severity or "?", "severity", t.severity), pandoc.Space(),
      badge(status_text(t.status), "status", t.status), pandoc.Space(),
      pandoc.Str(t.type or ""), pandoc.Space(),
      pandoc.Str("·"), pandoc.Space(),
      pandoc.Emph({ pandoc.Str(t._element) }),
    })
    if t.description and t.description ~= "" then
      blocks[#blocks + 1] = pandoc.Para({ pandoc.Strong({ pandoc.Str("Threat.") }),
        pandoc.Space(), pandoc.Str(t.description) })
    end
    if t.mitigation and t.mitigation ~= "" then
      blocks[#blocks + 1] = pandoc.Para({ pandoc.Strong({ pandoc.Str("Mitigation.") }),
        pandoc.Space(), pandoc.Str(t.mitigation) })
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
  local threats = collect(read_model(path))

  local out = { overview_table(threats) }
  out[#out + 1] = pandoc.Header(2, { pandoc.Str("Threat detail") })
  for _, b in ipairs(detail_blocks(threats)) do out[#out + 1] = b end
  return out
end
