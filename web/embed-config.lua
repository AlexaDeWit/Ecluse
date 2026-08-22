-- Expand a paragraph that holds only a link to config/default.yaml into the file
-- itself, as a YAML code block. Run it before links.lua, which rewrites the target.
-- The build fails when the page carries no such paragraph, so the site never ships
-- the manual without its configuration reference.
local embedded = false

function Para(elem)
  if #elem.content == 1 and elem.content[1].t == "Link" then
    local link = elem.content[1]
    if link.target == "config/default.yaml" then
      local file = assert(io.open("config/default.yaml", "r"), "embed-config: cannot open config/default.yaml")
      local content = file:read("*all")
      file:close()
      embedded = true
      return pandoc.CodeBlock((content:gsub("\n$", "")), "yaml")
    end
  end
  return elem
end

function Pandoc(doc)
  assert(embedded, "embed-config: no paragraph linking config/default.yaml to embed")
  return doc
end
