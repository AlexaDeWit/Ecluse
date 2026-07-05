-- The repository's Markdown links are written for GitHub: relative paths to repo
-- files, with anchors, and absolute https://ecluse-proxy.com/ URLs for pages that
-- only exist on the rendered site. Rewrite them for the site:
--   * an absolute link to the site's own domain -> the same target, relative
--   * a link to a doc we render here            -> its .html page (anchor preserved)
--   * any other in-repo path                    -> the file on GitHub (so nothing dangles)
-- Other absolute URLs (http/https/mailto) and pure #fragment links are left untouched.
--
-- A rendered doc that does not live at the repository root declares its directory
-- with `-M link-base=<dir>` (see site:pandoc in Taskfile.yml). Relative targets
-- resolve against that base before the lookup, so `../MOTIVATION.md` written from
-- docs/ still maps to motivation.html, and `architecture/foo.md` lands on the right
-- GitHub path.

-- Only docs rendered 1:1 belong here. README.md is deliberately absent: the
-- landing page is hand-authored, not a rendering of the README, so a README
-- link (they carry anchors like #verifying-the-image) must go to GitHub where
-- its content and anchors actually exist.
local pages = {
  ["MOTIVATION.md"]        = "motivation.html",
  ["ALTERNATIVES.md"]      = "alternatives.html",
  ["USAGE.md"]             = "usage.html",
  ["SECURITY.md"]          = "security.html",
  ["docs/architecture.md"] = "architecture.html",
}

local blob = "https://github.com/AlexaDeWit/Ecluse/blob/main/"

local base = ""

-- Join a relative target onto the doc's base directory and collapse `.` and `..`
-- segments, yielding the repo-root-relative path the pages map and the GitHub
-- fallback both expect. A trailing slash (a directory link) is preserved.
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
  -- The site's own pages, linked absolutely so they work for GitHub readers:
  -- serve them relative, so the link graph stays internal and preview builds
  -- do not depend on the live domain.
  local own = target:match("^https://ecluse%-proxy%.com/(.*)$")
  if own then
    if own == "" or own:match("^#") then own = "index.html" .. own end
    el.target = own
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
    el.target = mapped .. anchor
  else
    el.target = blob .. resolved .. anchor
  end
  return el
end

-- Two passes: metadata is read after block elements in a single-pass filter, so
-- capture link-base first, then rewrite links.
return {
  {
    Meta = function(m)
      if m["link-base"] then
        base = pandoc.utils.stringify(m["link-base"])
      end
    end,
  },
  { Link = rewrite },
}
