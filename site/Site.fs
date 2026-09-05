module Docs.Site

open Feliz.ViewEngine
open Nacara.Core
open Nacara.Plugins
open Nacara.Theme

let versions = [SiteVersion.root "1.0"]

let navbar =
    Theme.navbar [
        NavbarSection("Guide", "guide", "/guide/introduction/")
        NavbarSection("Wire", "wire", "/wire/")
    ]
    >> Theme.navbarEnd [
        NavbarDynamicWidget(Versions.switcher (Versions.versions versions Versions.defaults))
        NavbarIcon("GitHub", "https://github.com/shayanhabibi/xantham", Icons.github)
    ]

let theme =
    Theme.defaults
    |> navbar
    |> Theme.editUrl "https://github.com/shayanhabibi/xantham/edit/main/docs"
    |> Theme.footer (Html.p [Html.text "Built with Nacara"])

let site =
    Site.create "Xantham"
    |> Site.baseUrl "/"
    |> Site.origin "https://shayanhabibi.github.io"
    |> Site.output "output"
    |> Site.staticFiles "static"
    |> Markdown.register
    |> TreeSitter.register
    |> Literate.register
    |> Sitemap.register
    |> LinkValidator.register
    |> Rumdl.register
    |> LightningCss.register
    |> Esbuild.register
    |> Nuglify.minifyHtml
    |> Versions.register versions
    |> GitHubPages.register
    |> Theme.register theme
    |> Site.collection (Theme.docs theme "content")

[<EntryPoint>]
let main argv = Nacara.run site argv