-- The repository's Markdown links are written for GitHub: relative paths to repo
-- files, with anchors. Rewrite them for the rendered site:
--   * a link to a doc we render here  -> its .html page (anchor preserved)
--   * any other in-repo path           -> the file on GitHub (so nothing dangles)
-- Absolute URLs (http/https/mailto) and pure #fragment links are left untouched.

local pages = {
  ["MOTIVATION.md"]   = "motivation.html",
  ["ALTERNATIVES.md"] = "alternatives.html",
  ["USAGE.md"]        = "usage.html",
  ["README.md"]       = "index.html",
  ["AI-DISCLOSURE.md"] = "ai-disclosure.html",
}

local blob = "https://github.com/AlexaDeWit/Ecluse/blob/main/"

function Link(el)
  local target = el.target
  -- Leave external URLs and same-page fragments alone.
  if target:match("^%a[%w+.-]*://") or target:match("^mailto:") or target:match("^#") then
    return el
  end
  local path, anchor = target:match("^([^#]*)(.*)$")
  local mapped = pages[path]
  if mapped then
    el.target = mapped .. anchor
  else
    el.target = blob .. (path:gsub("^%./", "")) .. anchor
  end
  return el
end
