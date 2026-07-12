-- Expand a ```threat-register fence into the project's threat register, rendered
-- from the threatcl model at `make site` time. The model
-- (threat-modelling/ecluse.hcl) is the single source of truth.

local DEFAULT_PATH = "threat-modelling/ecluse.hcl"

function CodeBlock(el)
  if not el.classes:includes("threat-register") then
    return nil
  end
  local path = el.text:gsub("%s+", "")
  if path == "" then path = DEFAULT_PATH end

  -- We expect `threatcl` to be available on PATH (provided by docsInputs in flake.nix)
  local f1 = assert(io.popen("threatcl view -raw " .. path, "r"))
  local raw_md = f1:read("a")
  f1:close()

  local f2 = assert(io.popen("threatcl mermaid " .. path, "r"))
  local mermaid_raw = f2:read("a")
  f2:close()

  local out_blocks = {}

  if mermaid_raw and mermaid_raw:match("%S") then
    out_blocks[#out_blocks + 1] = pandoc.Header(2, { pandoc.Str("Diagrams") })
    out_blocks[#out_blocks + 1] = pandoc.CodeBlock(mermaid_raw, pandoc.Attr("", {"mermaid"}))
  end

  if raw_md and raw_md:match("%S") then
    local doc = pandoc.read(raw_md, "markdown")
    for _, b in ipairs(doc.blocks) do
      out_blocks[#out_blocks + 1] = b
    end
  end

  return out_blocks
end
