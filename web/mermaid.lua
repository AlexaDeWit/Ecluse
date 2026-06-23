-- Pandoc renders a ```mermaid fence as <pre><code class="language-mermaid">, which
-- Mermaid.js does not pick up. Convert it to <pre class="mermaid"> (what Mermaid
-- renders), HTML-escaping the source so that literal markup in node labels (the
-- diagrams use <br/> for line breaks) survives as text instead of being parsed into
-- real elements and lost from the diagram definition.
function CodeBlock(el)
  if el.classes:includes("mermaid") then
    local escaped = el.text:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
    return pandoc.RawBlock("html", '<pre class="mermaid">' .. escaped .. "</pre>")
  end
end
