# Écluse brand colours

This file names the brand colours and records the role each one plays, so the website,
future graphics, and any other surface stay consistent. The palette comes from the brand
artwork: the logo mark ([`logo.svg`](logo.svg)) and the social cards
([`../social-card.svg`](../social-card.svg),
[`../social-preview.svg`](../social-preview.svg)). Those SVGs are the source of truth.

The motif is a flight of canal locks. Warm **cream and charcoal** carry paper and ink, the
lock chambers step through **sand** and **tan**, and the **water** is a muted sage-teal.
The feel is warm, calm, and a little aged, deliberately *not* bright white.

## Core colours

| Name | Hex | In the artwork | Role |
|------|-----|----------------|------|
| **Charcoal** | `#211F1C` | the logo tile, ink and strokes | Primary text / ink |
| **Cream** | `#F4EFE3` | the logo "É", the card ground, the top lock step | Raised surfaces (panels, cards, code) |
| **Sand** | `#ECE4D2` | middle lock step | Page ground (calmer than cream) |
| **Tan** | `#DDD3BB` | lower lock step | Borders, dividers |
| **Slate** | `#4F4B43` | muted ink in the cards | Secondary / muted text |
| **Water** | `#6E928B` | the lock water (lower pound) | Accent (links) |
| **Water light** | `#9BBBB4` | the lock water (upper pound) | Lighter accent |
| **Sage / Mist** | `#8AA09B`, `#A6B8B2`, `#CBD6D2` | water gradients & mist | Decorative |

## Website tokens

`web/static/style.css` maps the core colours to semantic roles. The page ground is Sand,
not Cream, so a full screen reads warm rather than glaring. Cream becomes the
raised-surface colour: cards, code, and the header.

| Token | Value | Note |
|-------|-------|------|
| `--bg` | Sand `#ECE4D2` | page ground |
| `--surface` | Cream `#F4EFE3` | cards, code, header |
| `--fg` | Charcoal `#211F1C` | text |
| `--muted` | Slate `#4F4B43` | secondary text |
| `--accent` | `#3F6058` | Water, darkened so links clear WCAG AA on Sand |
| `--accent-strong` | `#2E4A45` | hover / active |
| `--border` | Tan `#DDD3BB` | borders & rules |

The two accent values are the only derived tones. The brand Water (`#6E928B`) is too light
for link text on a light ground. `--accent` keeps the hue and darkens it until link text
passes AA on Sand. To extend the palette with a tint or shade scale for UI states, generate
that scale from these anchors rather than picking fresh colours.

## Mode

Light only, for now. The artwork has no dark-mode variant to draw from, so a dark theme
waits until one exists. The natural ground would be Charcoal `#211F1C` with Cream text,
mirroring the logo tile.
