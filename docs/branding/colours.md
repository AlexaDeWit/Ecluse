# Écluse brand colours

The palette is taken directly from the brand artwork — the logo mark
([`logo.svg`](logo.svg)) and the social cards ([`../social-card.svg`](../social-card.svg),
[`../social-preview.svg`](../social-preview.svg)). **Those SVGs are the source of truth;**
this file names the colours and records how they are used, so the website, future
graphics, and any other surface stay consistent.

The motif is a flight of canal locks: warm **cream and charcoal** (paper and ink), the
lock chambers stepping through **sand** and **tan**, and the **water** rendered as a
muted sage-teal. The overall feel is warm, calm, and a little aged — deliberately *not*
bright white.

## Core colours

| Name | Hex | In the artwork | Role |
|------|-----|----------------|------|
| **Charcoal** | `#211F1C` | the logo tile; ink & strokes | Primary text / ink |
| **Cream** | `#F4EFE3` | the logo "É"; card ground; top lock step | Raised surfaces (panels, cards, code) |
| **Sand** | `#ECE4D2` | middle lock step | **Page ground** (calmer than cream) |
| **Tan** | `#DDD3BB` | lower lock step | Borders, dividers |
| **Slate** | `#4F4B43` | muted ink in the cards | Secondary / muted text |
| **Water** | `#6E928B` | the lock water (lower pound) | Accent (links) |
| **Water light** | `#9BBBB4` | the lock water (upper pound) | Lighter accent |
| **Sage / Mist** | `#8AA09B`, `#A6B8B2`, `#CBD6D2` | water gradients & mist | Decorative |

## Website tokens

`web/static/style.css` maps the core colours to semantic roles. The page **ground is
Sand**, not Cream, so a full screen reads warm rather than glaring; Cream becomes the
raised-surface colour (cards, code, header).

| Token | Value | Note |
|-------|-------|------|
| `--bg` | Sand `#ECE4D2` | page ground |
| `--surface` | Cream `#F4EFE3` | cards, code, header |
| `--fg` | Charcoal `#211F1C` | text |
| `--muted` | Slate `#4F4B43` | secondary text |
| `--accent` | `#3F6058` | **Water, darkened** so links clear WCAG AA on Sand |
| `--accent-strong` | `#2E4A45` | hover / active |
| `--border` | Tan `#DDD3BB` | borders & rules |

The two accent values are the only **derived** tones: the brand Water (`#6E928B`) is too
light to use as link text on a light ground, so it is darkened toward the same hue until
it passes AA. If an extended tint/shade scale is ever needed for UI states, generate it
from these anchors rather than picking fresh colours.

## Mode

**Light only, for now.** The artwork has no dark-mode variant to draw from, so a dark
theme should wait until one exists — the natural ground would be Charcoal `#211F1C` with
Cream text, mirroring the logo tile.
