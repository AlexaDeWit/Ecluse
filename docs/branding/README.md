# Écluse artwork

The illustration shows software packages travelling on a barge through a canal lock.
Two gates enclose the chamber. Code brackets distinguish the packages from ordinary cargo.
The caption states the purpose: "Control which packages reach your builds."

The É lettermark uses two water-filled channels and pointed ends that echo the gates.
Keep its acute accent and proportions intact at small sizes.

## Files

| Source | Use | Size |
| --- | --- | --- |
| [`../social-card.svg`](../social-card.svg) | Social sharing card | 1200 × 630 |
| [`banner.svg`](banner.svg) | Repository README banner | 1600 × 520 |
| [`lock-illustration.svg`](lock-illustration.svg) | Website hero without lettering | 1100 × 420 |
| [`logo.svg`](logo.svg) | Lettermark and favicon source | 128 × 128 |
| [`wordmark.svg`](wordmark.svg) | Dark lettering for light backgrounds | 580 × 220 |
| [`wordmark-inverse.svg`](wordmark-inverse.svg) | Cream lettering for dark backgrounds | 580 × 220 |

The SVGs are editable source files. Their lettering uses paths, so viewers do not need the fonts installed.
PNG exports accompany the card, banner, lettermark, and wordmarks.
`favicon-32.png` is a 32px render of the lettermark.
`social-preview.svg` and `social-preview.png` are copies of the social card that preserve the existing website asset path.

The site build publishes another copy with its SHA256 in the filename and uses that URL in Open Graph metadata.
Changing the image changes its URL, so social platforms can fetch it without reusing the previous image URL.
The author comes from the site configuration. The publication timestamp records the latest site build in UTC.
The Pages workflow computes that timestamp during its Zola build.

Use the card's 1200 × 630 dimensions in Open Graph metadata.
Upload [`../social-card.png`](../social-card.png) in GitHub's repository settings to change the social preview.
Changing the tracked file does not change that setting automatically.

## Colours

| Name | Hex |
| --- | --- |
| Charcoal | `#211F1C` |
| Cream | `#F4EFE3` |
| Sand | `#DDD3BB` |
| Stone | `#B8A789` |
| Water | `#8AA09B` |
| Deep water | `#577873` |
| Timber | `#A15E42` |

## Render the PNG files

Run this command from the repository root:

```bash
env -u IN_NIX_SHELL nix develop --command task \
  --taskfile docs/branding/Taskfile.yml render
```

The task uses resvg from the repository's pinned nixpkgs input.
It updates the PNG exports and the social-preview copies.
Review the card at 320px wide and the favicon at 16px and 32px after editing.

## Typography and licence

The illustration and lettermark are original vectors under the repository's MIT licence.
The wordmark uses Source Sans 3 Semibold. The caption uses Source Sans 3 Regular.
Both come from Adobe's [Source Sans repository](https://github.com/adobe-fonts/source-sans/tree/87b37a2daaed80fcb8e8ccb0085c4d72ddade12e).
Adobe licenses Source Sans 3 under the [SIL Open Font License 1.1](https://github.com/adobe-fonts/source-sans/blob/87b37a2daaed80fcb8e8ccb0085c4d72ddade12e/LICENSE.md).
The exported lettering contains paths, not embedded font files.

The website loads Source Sans 3 from Google Fonts for prose, headings, and navigation.
It requests normal and italic weights from 400 to 700, with system fonts as the fallback.
Code examples keep the system monospace font.
