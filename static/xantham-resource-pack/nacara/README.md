# Xantham theme for Nacara

Targets the current F#-configured `Nacara.Theme.Default` described in the supplied getting-started guide. This is an additive stylesheet, not a replacement theme. Your docs repository was not available; apply these files to that project.

## Install

1. Copy this pack's `css/` directory into the documentation project's root (next to `Program.fs`). Merge with an existing directory.
2. Add `|> Site.stylesheet "css/xantham.css"` to the existing `site` pipeline. Keep your `Theme.register theme`, plugin registrations and collections.
3. Run `dotnet run -- build`, then `dotnet run -- watch` to inspect the result.

```fsharp
let site =
    Site.create "Xantham"
    // Existing plugins, theme and collections remain here.
    |> Theme.register theme
    |> Site.stylesheet "css/xantham.css"
```

This snippet assumes your existing `theme` value and `open Nacara.Core` / `open Nacara.Theme`. It illustrates the insertion; do not replace your full site definition with it.

Nacara resolves the stylesheet relative to the project root and bundles its neighboring CSS imports. It loads site styles after the default theme. [Official customising guide](https://mangelmaxime.github.io/Nacara/plugins/themes/default/customising/).

## Files and behavior

- `css/xantham.css`: entry point.
- `css/xantham-tokens.css`: original brand swatches, light/dark semantic colors, typography, dimensions, and every syntax-token color.
- `css/xantham-components.css`: small navbar, active-navigation, code-title and link refinements; optional landing-page utility classes.
- `tokens.json`: the same values for tooling; CSS is what Nacara consumes. Keep both synchronized when editing.
- `static/branding/`: the four SVG brand assets, including corrected logo geometry.

The engine's existing theme selector sets `html[data-theme="light"]` or `html[data-theme="dark"]`, including for the System option. This pack follows that attribute; it does not install a second theme switcher or change the stored preference.

| Token | Light | Dark |
| --- | --- | --- |
| `--nacara-bg` | `#FFFFFF` | `#101426` |
| `--nacara-bg-subtle` | `#F6F3FF` | `#191E35` |
| `--nacara-primary` | `#005FCC` | `#73DBFF` |
| `--nacara-text` | `#25263D` | `#E5ECFA` |
| `--nacara-text-muted` | `#59647D` | `#ADBBD2` |
| `--tok-keyword` | `#7141AD` | `#C9A9FF` |

Light mode uses white/mist surfaces, midnight text, and a darker blue for legible links. Dark mode uses navy surfaces, sky-blue links, and violet syntax accents. Exact source-brand colors are kept as `--xantham-*`; semantic UI shades are separate. Fonts use locally available system stacks; no font downloads are required. Semantic callouts retain distinct note/tip/warning/danger hues.

Keep the default theme's spacing scale and mobile layout. `--nacara-content-width` is 76ch. Existing `Theme.css` rules are emitted later and can override this pack, including section-specific widths.

## Logo and mascot

Copy `static/branding/` into your configured static directory. Assets are optional: the stylesheet has no mandatory image URLs. Reference them from your current navbar/custom layout using your site's base-path-aware URL mechanism. For a site deployed at `/Xantham/`, for example, the public logo URL is `/Xantham/branding/xantham-logo-electric-cyan.svg`, not `/branding/...`. Retain the visible brand text and treat an adjacent logo as decorative (`alt=""`).

Optional CSS classes: `xantham-hero`, `xantham-hero__eyebrow`, `xantham-brand-mark`, `xantham-mascot`, and `xantham-mascot--midnight`. The midnight mascot class supplies a light tile so its silhouette remains visible in dark mode.

## Verification and scope

Token names and selectors were checked against the live Nacara theme stylesheet on 2026-09-14. Browser rendering was not verified: the runtime had no browser installed and the browser download timed out. Light/dark behavior follows the live theme’s documented data attribute. Main text, links, muted text, and syntax foregrounds were checked for at least 4.5:1 contrast on their intended plain surfaces. This does not constitute a full accessibility audit. The site's F# build cannot be verified without your project and dependency versions.

References: [getting started](https://mangelmaxime.github.io/Nacara/guide/getting-started/), [theming](https://mangelmaxime.github.io/Nacara/guide/theme/), [customising](https://mangelmaxime.github.io/Nacara/plugins/themes/default/customising/).
