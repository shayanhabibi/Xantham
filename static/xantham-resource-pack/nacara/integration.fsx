// Integration excerpt for your existing Program.fs, not a standalone site.
// Keep your current plugins, collections, navbar and theme configuration.
// Add this pipeline step to your existing site definition:
//
// |> Theme.register theme
// |> Site.stylesheet "css/xantham.css"
//
// Example with an existing `site` value:
open Nacara.Core
let withXanthamBranding site =
    site |> Site.stylesheet "css/xantham.css"
