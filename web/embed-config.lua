function CodeBlock(elem)
  if elem.classes:includes("embed-default-config") then
    local file = io.open("config/default.yaml", "r")
    if file then
      local content = file:read("*all")
      file:close()
      -- Strip any trailing newline from the read content for cleaner rendering
      elem.text = content:gsub("\n$", "")
    else
      io.stderr:write("Warning: Could not open config/default.yaml for embedding.\n")
    end
  end
  return elem
end
