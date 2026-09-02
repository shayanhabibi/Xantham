module Xantham.Generator.Shape.ExportNames

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type the first in harvest order names it and `shape-aliases` abbreviates the rest.
let nameExports: Pass<ShapeModel> =
    Pass.pure' "name-exports" (fun ctx model ->
        let fallback = defaultExportName ctx

        let names, orders =
            model.Harvest.Exports
            |> List.fold
                (fun (names, orders) export ->
                    if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                        names, orders
                    else
                        match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                        | Some typeId when not (Map.containsKey typeId names) ->
                            Map.add typeId (fsName fallback export) names, Map.add typeId export.Order orders
                        | _ -> names, orders)
                (model.DeclNames, model.DeclOrders)

        { model with
            DeclNames = names
            DeclOrders = orders
        })
