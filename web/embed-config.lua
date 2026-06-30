function Para(elem)
  if #elem.content == 1 and elem.content[1].t == "Link" then
    local link = elem.content[1]
    if link.target == "config/default.yaml" then
      local file = io.open("config/default.yaml", "r")
      if file then
        local content = file:read("*all")
        file:close()
        return pandoc.CodeBlock(content:gsub("\n$", ""), "yaml")
      else
        io.stderr:write("Warning: Could not open config/default.yaml for embedding.\n")
      end
    end
  end
  return elem
end
