import { readFileSync, writeFileSync, mkdirSync } from "node:fs"
import { resolve } from "node:path"

const input = JSON.parse(readFileSync(0, "utf8"))
const dashboard = typeof input === "string" ? JSON.parse(input) : input
const output = resolve(process.argv[2] ?? "examples/datadog/preview")
mkdirSync(output, { recursive: true })
writeFileSync(resolve(output, "dashboard.json"), JSON.stringify(dashboard, null, 2) + "\n")
const colours = ["#7744b6", "#16828d", "#3778bd", "#bd6130", "#bd3659"]
const pieces = []
const escape = value => String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;")
const text = (x, y, value, size = 13, colour = "#313747", weight = 400) => pieces.push('<text x="' + x + '" y="' + y + '" font-size="' + size + '" fill="' + colour + '" font-weight="' + weight + '">' + escape(value) + "</text>")
const box = (x, y, width, height, fill = "white", stroke = "#dfe3eb") => pieces.push('<rect x="' + x + '" y="' + y + '" width="' + width + '" height="' + height + '" rx="6" fill="' + fill + '" stroke="' + stroke + '"/>')
function wrap(value, max = 48) {
  const lines = [""]
  for (const word of value.split(/\s+/)) {
    if (lines.at(-1).length + word.length > max) lines.push(word)
    else lines[lines.length - 1] += (lines.at(-1) ? " " : "") + word
  }
  return lines
}
function widget(definition, x, y, width, height) {
  box(x, y, width, height)
  wrap(definition.title).slice(0, 2).forEach((row, i) => text(x + 16, y + 26 + i * 19, row, 15, "#252b38", 600))
  if (definition.type === "query_value") {
    const examples = ["100 req/s", "0.24%", "12 ms", "280 MiB"]
    const index = ["HTTP traffic", "HTTP 5xx", "Mean HTTP", "Metadata memory"].findIndex(prefix => definition.title.startsWith(prefix))
    text(x + 16, y + 100, examples[index] ?? "Example", 32, colours[0], 600)
    text(x + 16, y + height - 20, "Illustrative value", 11, "#656d7d")
    return
  }
  const request = definition.requests[0]
  const formulas = request.formulas ?? []
  const top = y + 70, left = x + 28, chartWidth = width - 48, chartHeight = height - 120
  for (const fraction of [0, 0.5, 1]) pieces.push('<path d="M' + left + "," + (top + chartHeight * fraction) + "h" + chartWidth + '" stroke="#e8ebf1"/>')
  formulas.slice(0, 3).forEach((formula, series) => {
    const points = Array.from({ length: 40 }, (_, i) => {
      const value = 0.35 + 0.13 * Math.sin(i / 6 + series) + 0.04 * Math.cos(i / 2.5) + series * 0.14
      return (left + i * chartWidth / 39).toFixed(1) + "," + (top + chartHeight * (1 - value)).toFixed(1)
    })
    pieces.push('<polyline points="' + points.join(" ") + '" stroke="' + colours[series] + '" fill="none" stroke-width="2.4"/>')
    text(x + 16 + series * (width - 32) / 3, y + height - 15, (formula.alias ?? formula.formula).slice(0, 23), 10, colours[series])
  })
  text(x + width - 60, y + 64, definition.yaxis?.label ?? "", 11, "#656d7d")
  text(left, y + height - 34, "60 min ago", 10, "#656d7d")
  text(x + width - 44, y + height - 34, "now", 10, "#656d7d")
}
box(0, 0, 1600, 56, "#7744b6", "#7744b6")
text(28, 35, "DATADOG / ÉCLUSE OPERATOR EXAMPLE", 17, "white", 600)
text(1160, 35, "ILLUSTRATIVE DATA / NOT LIVE", 14, "white", 600)
text(30, 103, dashboard.title, 30, "#242a36", 600)
text(30, 133, "Every panel below comes from the module's exported dashboard JSON.", 15, "#656d7d")
let selectorX = 30
for (const variable of dashboard.template_variables) {
  box(selectorX, 155, 320, 38)
  text(selectorX + 12, 179, variable.name + ": " + variable.default, 13)
  selectorX += 336
}
text(30, 224, "Deployment scope remains fixed. Advisory ecosystem selection only affects advisory panels.", 13, "#656d7d")
text(30, 248, "No data is unknown. Existing health monitors are links, not inferred health indicators.", 13, "#656d7d")
let y = 290
for (const entry of dashboard.widgets) {
  const definition = entry.definition
  if (definition.type === "group") {
    text(30, y, definition.title, 21, "#252b38", 600)
    y += 20
    const columns = definition.title === "Overview" ? 4 : 3
    const step = columns === 4 ? 390 : 520
    const rowHeight = columns === 4 ? 175 : 230
    for (let i = 0; i < definition.widgets.length; i++) {
      widget(definition.widgets[i].definition, 30 + (i % columns) * step, y + Math.floor(i / columns) * rowHeight, step - 20, rowHeight - 20)
    }
    y += Math.ceil(definition.widgets.length / columns) * rowHeight + 32
  } else if (definition.type === "log_stream") {
    box(30, y, 1540, 175)
    text(48, y + 28, definition.title, 18, "#252b38", 600)
    text(48, y + 59, "TIME                 STATUS       SERVICE                  MESSAGE", 12, "#656d7d", 600)
    text(48, y + 91, "14:32:11             warn          ecluse-proxy            denied: example-package / BlockedByDefault", 13)
    text(48, y + 121, "14:31:54             error         ecluse-proxy            Upstream metadata request failed", 13)
    text(48, y + 151, "Synthetic records. Open the live dashboard to investigate the selected deployment.", 12, "#656d7d")
    y += 200
  }
}
text(30, y + 15, "Mockup only. Query existence, live results, costs, and account configuration require deployment verification.", 13, "#656d7d")
const svg = '<svg xmlns="http://www.w3.org/2000/svg" width="1600" height="' + (y + 50) + '" viewBox="0 0 1600 ' + (y + 50) + '"><style>text{font-family:DejaVu Sans,sans-serif}</style><rect width="100%" height="100%" fill="#f4f5f8"/>' + pieces.join("\n") + "</svg>\n"
writeFileSync(resolve(output, "dashboard.svg"), svg)
process.stderr.write("Rendered " + dashboard.widgets.filter(w => w.definition.type === "group").reduce((n, w) => n + w.definition.widgets.length, 0) + " metric panels from the module JSON.\n")
