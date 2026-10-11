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

            let searchPaths =
                ApiProjects.projects
                |> List.map (fun (project, framework) ->
                    AbsolutePath.combine root [ "src"; project; "bin"; "Release"; framework ]
                    |> AbsolutePath.value)

            ApiProjects.projects
            |> List.map (fun (project, framework) ->
                let dllPath =
                    AbsolutePath.combine root [ "src"; project; "bin"; "Release"; framework; project + ".dll" ]
                    |> AbsolutePath.value

                if not (System.IO.File.Exists dllPath) then
                    failwith $"Missing API reference assembly: {dllPath}. Run dotnet fsi build.fsx -- docs."

                FSharpApiSource.create dllPath |> FSharpApiSource.searchPaths searchPaths)
    }

let navbar =
    Theme.navbar
        [
            NavbarSection("Guide", "xantham-cli", "/xantham-cli/")
            NavbarSection("TypeScript.Wire", "wire", "/wire/")
            NavbarSection("DEV", "dev", "/dev/")
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
                            Menu.page "xantham-cli/guide/bindings"
                            Menu.page "xantham-cli/guide/packages"
                            Menu.page "xantham-cli/guide/configuration"
                            Menu.page "xantham-cli/guide/dependencies"
                            Menu.page "xantham-cli/guide/customization" |> Menu.badge Badge.New
                            Menu.page "xantham-cli/guide/troubleshooting"
                        ]
                    Menu.link "Source" "https://github.com/shayanhabibi/xantham"
                ]
        ]
    >> Theme.menu
        "wire"
        [
            Menu.section "TypeScript.Wire" [ Menu.page "wire/index"; Menu.page "wire/navigation" ]
        ]
    >> Theme.menu
        "dev"
        [
            Menu.section
                "Contributing"
                [
                    Menu.page "dev/index"
                    Menu.page "dev/setup"
                    Menu.page "dev/generator"
                    Menu.page "dev/generated-sources"
                    Menu.page "dev/hand-written"
                ]
        ]

let theme =
    Theme.defaults
    |> navbar
    |> Theme.favIcon "/branding/xantham-logo-electric-cyan.svg"
    |> Theme.editUrl "https://github.com/shayanhabibi/xantham/edit/develop/site"
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
            TailwindEntryFooter =
                opts.TailwindEntryFooter
                @ [
                    let stylesheet = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "css", "xantham.css")
                    "@import \"" + stylesheet.Replace('\\', '/') + "\";"
                ]
        })
    |> Nuglify.minifyHtml
    |> Theme.register theme
    |> Site.collection (Theme.docs theme "content")
    |> Site.collection reference

[<EntryPoint>]
let main argv = Nacara.run site argv
