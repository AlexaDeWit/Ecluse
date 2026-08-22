-- The repository's Markdown links target GitHub: relative paths to repo files,
-- with anchors. They also use absolute https://ecluse-proxy.com/ URLs for pages
-- that only exist on the rendered site. Rewrite them for the site:
--   * an absolute link to the site's own domain -> the same target, relative
--   * a link to a doc we render here            -> its .html page (anchor kept)
--   * any other in-repo path                    -> the file on GitHub (so nothing dangles)
-- Leave other absolute URLs (http/https/mailto) and pure #fragment links alone.
--
-- A rendered doc that does not live at the repository root declares its directory
-- with `-M link-base=<dir>` (see site:pandoc in Taskfile.yml). A relative target
-- resolves against that base before the lookup. So `../MOTIVATION.md` written
-- from docs/ still maps to motivation.html, and `architecture/foo.md` lands on
-- the right GitHub path.

-- Only docs rendered 1:1 belong here. Every other in-repo path, README.md and the
-- architecture documents included, falls back to GitHub, where the file and its
-- heading anchors exist.
local pages = {
  ["USAGE.md"] = "usage.html",
}

local blob = "https://github.com/AlexaDeWit/Ecluse/blob/main/"

local base = ""

-- Directory prefix ("../" per level) of the page being rendered, so a page
-- under architecture/ emits links that resolve from its own location.
local prefix = ""

-- Join a relative target onto the doc's base directory and collapse the `.` and
-- `..` segments. The result is the repo-root-relative path the pages map and the
-- GitHub fallback both expect. A trailing slash (a directory link) survives.
local function resolve(path)
  local joined = (base == "") and path or (base .. "/" .. path)
  local parts = {}
  for seg in joined:gmatch("[^/]+") do
    if seg == ".." then
      table.remove(parts)
    elseif seg ~= "." then
      parts[#parts + 1] = seg
    end
  end
  local resolved = table.concat(parts, "/")
  if joined:sub(-1) == "/" and resolved ~= "" then
    resolved = resolved .. "/"
  end
  return resolved
end

local function rewrite(el)
  local target = el.target
  -- The docs link the site's own pages absolutely, so they work for GitHub
  -- readers. Serve them relative here, so the link graph stays internal and a
  -- preview build does not depend on the live domain.
  local own = target:match("^https://ecluse%-proxy%.com/(.*)$")
  if own then
    if own == "" or own:match("^#") then own = "index.html" .. own end
    el.target = prefix .. own
    return el
  end
  -- Leave other external URLs and same-page fragments alone.
  if target:match("^%a[%w+.-]*://") or target:match("^mailto:") or target:match("^#") then
    return el
  end
  local path, anchor = target:match("^([^#]*)(.*)$")
  local resolved = resolve(path)
  local mapped = pages[resolved]
  if mapped then
    el.target = prefix .. mapped .. anchor
  else
    el.target = blob .. resolved .. anchor
  end
  return el
end

-- Two passes: a single-pass filter reads metadata after block elements, so
-- capture link-base first, then rewrite links.
return {
  {
    Meta = function(m)
      if m["link-base"] then
        base = pandoc.utils.stringify(m["link-base"])
      end
      if m["prefix"] then
        prefix = pandoc.utils.stringify(m["prefix"])
      end
    end,
  },
  { Link = rewrite },
}
