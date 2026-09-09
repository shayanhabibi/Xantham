module Docs.Site

open Feliz.ViewEngine
open Nacara.Core
open Nacara.Plugins
open Nacara.Theme

// let versions = [ SiteVersion.root "0.0" ]

let navbar =
    Theme.navbar
        [
            NavbarSection("Xantham", "xantham-cli", "/xantham-cli/")
            NavbarSection("Tsc Wire", "wire", "/wire/")
        ]
    >> Theme.navbarEnd
        [
            // NavbarDynamicWidget(Versions.switcher (Versions.versions versions Versions.defaults))
            NavbarIcon("GitHub", "https://github.com/shayanhabibi/xantham", Icons.github)
        ]
    >> Theme.menu
        "xantham-cli"
        [
            Menu.section
                "xantham"
                [

                    Menu.page "xantham-cli/index"
                    Menu.section
                        "Guide"
                        [
                            Menu.page "xantham-cli/guide/installation"
                            Menu.page "xantham-cli/guide/usage"
                        ]
                    Menu.link "Source" "https://github.com/shayanhabibi/xantham"
                ]
        ]

let theme =
    Theme.defaults
    |> navbar
    |> Theme.editUrl "https://github.com/shayanhabibi/xantham/edit/main/site"
    |> Theme.footer (
        Html.div
            [
                Html.p
                    [
                        Html.text "Copyright (c) 2026 - Shayan Habibi, Houston Haynes and contributors"
                    ]
                Html.p [ Html.text "Built with Nacara" ]
            ]
    )

let site =
    Site.create "Xantham"
    |> Site.origin "https://shayanhabibi.github.io"
    |> Site.baseUrl "/Xantham/"
    |> Site.output "../output"
    |> Site.staticFiles "static"
    |> Markdown.register
    |> TreeSitter.register
    |> Literate.register
    |> Sitemap.register
    // |> LinkValidator.register
    |> DaisyUI.registerWith (fun opts ->
        { opts with
            TailwindEntryHeader =
                opts.TailwindEntryHeader
                @ [
                    //language=css
                    "@custom-variant dark (&:where([data-theme=dark], [data-theme=dark] *));"
                ]
        })
    |> Nuglify.minifyHtml
    |> Theme.register theme
    |> Site.collection (Theme.docs theme "content")

[<EntryPoint>]
let main argv = Nacara.run site argv
