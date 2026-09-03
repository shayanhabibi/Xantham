module Xantham.Generator.Shape.ExportNames

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type the first in harvest order names it and `shape-aliases` abbreviates the rest.
///
/// Two exports of two *different* types under one name are two declarations, and F# admits one
/// name per declaration: the later takes the numeric suffix `shape-anonymous` uses for the same
/// clash. `@cloudflare/workers-types` declares `WorkflowSleepDuration` twice, once in global
/// scope and once in an ambient module of the same file.
let nameExports: Pass<ShapeModel> =
    Pass.pure' "name-exports" (fun ctx model ->
        let fallback = defaultExportName ctx

        let claim (taken: Set<string>) (preferred: string) =
            if not (Set.contains preferred taken) then
                preferred
            else
                Seq.initInfinite (fun i -> $"{preferred}{i + 2}")
                |> Seq.find (fun candidate -> not (Set.contains candidate taken))

        let names, orders, _ =
            model.Harvest.Exports
            |> List.fold
                (fun (names, orders, taken) export ->
                    if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                        names, orders, taken
                    else
                        match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                        | Some typeId when not (Map.containsKey typeId names) ->
                            let name = claim taken (fsName fallback export)
                            Map.add typeId name names, Map.add typeId export.Order orders, Set.add name taken
                        | _ -> names, orders, taken)
                (model.DeclNames, model.DeclOrders, model.DeclNames |> Map.toList |> List.map snd |> Set.ofList)

        { model with
            DeclNames = names
            DeclOrders = orders
        })
