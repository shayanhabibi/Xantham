module Docs.Site

open Feliz.ViewEngine
open Nacara.Core
open Nacara.Plugins
open Nacara.Theme

// let versions = [ SiteVersion.root "0.0" ]

let apiOptions =
    { FSharpApi.defaults with
        Root = "reference"
        Title = "API reference"
        Exclude = [ "JetBrains.Annotations" ]
        Sources =
            let root = AbsolutePath.create __SOURCE_DIRECTORY__ |> AbsolutePath.directory

            [ "Xantham.TypeScript.Wire"; "Xantham.Fable.Core.TS"; "Xantham.Fable.Core" ]
            |> List.choose (fun project ->
                Glob.files root $"src/{project}/*/Release/*/{project}.dll"
                |> List.tryHead
                |> Option.map (
                    AbsolutePath.value
                    >> function
                        | dllPath when project = "Xantham.Fable.Core.TS" ->
                            FSharpApiSource.create dllPath
                            |> FSharpApiSource.searchPaths (
                                [
                                    AbsolutePath.combine
                                        root
                                        [ "src"; "Xantham.Fable.Core"; "bin"; "Release"; "net8.0" ]
                                    AbsolutePath.combine
                                        root
                                        [ "src"; "Xantham.Fable.Core"; "obj"; "Release"; "net8.0" ]
                                ]
                                |> List.map AbsolutePath.value
                            )
                        | dllPath -> FSharpApiSource.create dllPath
                ))
    }

let navbar =
    Theme.navbar
        [
            NavbarSection("Xantham", "xantham-cli", "/xantham-cli/")
            NavbarSection("Tsc Wire", "wire", "/wire/")
            NavbarDivider
            NavbarSection("Reference", "reference", "/reference/")
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

let reference =
    FSharpApi.collection "reference" DocFrontMatter.decoder apiOptions
    |> Collection.title _.Title
    |> Collection.layout (Theme.layout theme)


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
    |> FSharpApi.register apiOptions
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
    |> Site.collection reference

[<EntryPoint>]
let main argv = Nacara.run site argv
