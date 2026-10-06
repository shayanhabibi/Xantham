module Xantham.CustomizationExample.PartasDom

open Xantham.Generator.Customization

let extension owner direct lab =
    {
        Identity =
            {
                Id = "example.partas-dom"
                Version = "1"
                Configuration = Map.ofList [ "owner", owner; "direct", string direct; "lab", string lab ]
            }
        Transform =
            fun snapshot ->
                let selected =
                    if lab then
                        Semantic.tryFind owner [ "Input" ] snapshot
                    else
                        Semantic.tryFind "typescript/lib" [ "HTMLElement" ] snapshot
                        |> Option.bind (fun root ->
                            Semantic.descendants root snapshot
                            |> List.tryFind (fun source ->
                                Semantic.package source snapshot = owner
                                && Semantic.path source snapshot = [ "Input" ]))

                match selected with
                | None ->
                    Error
                        [
                            Diagnostic.create
                                "example.partas-dom/missing-input"
                                "Expected an exported Input inheriting from the compiler's HTMLElement."
                                None
                        ]
                | Some input ->
                    let members = Semantic.properties input snapshot

                    let properties =
                        if lab then
                            members
                        else
                            members
                            |> List.filter (fun p ->
                                List.contains (Semantic.jsName p snapshot) [ "value"; "title"; "tagName" ])

                    let companion =
                        Companion.create "Partas.Solid.CustomizationAcceptance" "InputProperties" input snapshot
                        |> Companion.withProperties properties

                    let companion =
                        if direct then
                            Companion.directProperties companion
                        else
                            companion

                    let edits = Edits.empty |> Edits.emitCompanion companion

                    let edits =
                        if lab && direct then
                            members
                            |> List.filter (fun p -> Semantic.jsName p snapshot = "disabled")
                            |> List.collect (fun p -> Semantic.outputTargets p snapshot)
                            |> List.fold
                                (fun edits target -> Edits.replaceInterop target (Interop.property "enabled") edits)
                                edits
                        else
                            edits

                    let edits =
                        if lab && direct then
                            match Semantic.tryFind owner [ "OddKeys" ] snapshot with
                            | Some odd ->
                                edits
                                |> Edits.emitCompanion (
                                    Companion.create
                                        "Partas.Solid.CustomizationAcceptance"
                                        "OddKeysProperties"
                                        odd
                                        snapshot
                                    |> Companion.directProperties
                                )
                            | None -> edits
                        else
                            edits

                    Ok edits
    }
