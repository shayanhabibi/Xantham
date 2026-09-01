/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; the tier's invariant is that every export of the entry module appears
/// exactly once, aliases followed to their origin.
module Xantham.Generator.Harvest

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Parses the ordering key out of a symbol's first declaration handle. A handle is
/// `index.kind.path` where only the path may contain further dots.
let declOrder (declarations: string[] voption) : DeclOrder option =
    match declarations with
    | ValueSome handles when handles.Length > 0 ->
        match handles[0].Split([| '.' |], 3) with
        | [| index; _kind; path |] ->
            match System.Int32.TryParse index with
            | true, index -> Some { File = path; NodeIndex = index }
            | _ -> None
        | _ -> None
    | _ -> None

/// The entry module's exports, each followed through `getAliasedSymbol` to its origin so that
/// re-exports and default-export aliases land on the declaring symbol.
let harvestExports: Pass<HarvestModel> =
    { Name = "harvest-exports"
      Run =
        fun ctx model ->
            async {
                let! moduleSymbol =
                    ctx.Session.getSymbolOfSourceFile (DocumentIdentifier.FileName ctx.EntryFile)

                match moduleSymbol with
                | ValueNone ->
                    return
                        Degraded(
                            model,
                            [ Finding.make
                                  Escape
                                  "<module>"
                                  $"{ctx.EntryFile} has no module symbol - nothing exported, nothing harvested" ]
                        )
                | ValueSome moduleSymbol ->
                    let! exports = ctx.Session.getExportsOfModule moduleSymbol.Id
                    let exports = exports |> ValueOption.defaultValue [||]

                    // Fan out the alias-following freely - the mailbox coalesces it - but
                    // Async.Parallel's result order is input order, so the fold is deterministic.
                    let! resolved =
                        exports
                        |> Array.map (fun export ->
                            async {
                                if export.Flags.HasFlag SymbolFlags.Alias then
                                    let! origin = ctx.Session.getAliasedSymbol export.Id
                                    return export.Name, origin
                                else
                                    return export.Name, export
                            })
                        |> Async.Parallel

                    let harvested =
                        resolved
                        |> Array.sortBy fst
                        |> Array.map (fun (name, origin) ->
                            { ExportName = name
                              Symbol = origin
                              Docs = ""
                              Tags = []
                              Order = declOrder origin.Declarations })
                        |> Array.toList

                    return Advanced { model with Exports = harvested }
            } }

/// Documentation for every harvested export, from the checker rather than the syntax tree so
/// merged declarations already read as one.
let harvestDocs: Pass<HarvestModel> =
    { Name = "harvest-docs"
      Run =
        fun ctx model ->
            async {
                let! documented =
                    model.Exports
                    |> List.map (fun export ->
                        async {
                            let! docs = ctx.Session.getDocumentationComment export.Symbol.Id
                            let! tags = ctx.Session.getJsDocTags export.Symbol.Id

                            return
                                { export with
                                    Docs = docs
                                    Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue [] }
                        })
                    |> Async.Parallel

                return Advanced { model with Exports = Array.toList documented }
            } }

/// Fixes the output order: source order of the first declaration, then export name as the
/// tiebreak. Exports with no declaration handle sort last.
let orderExports: Pass<HarvestModel> =
    Pass.pure' "harvest-order" (fun _ model ->
        { model with
            Exports =
                model.Exports
                |> List.sortBy (fun export ->
                    (match export.Order with
                     | Some order -> order.File, order.NodeIndex
                     | None -> "￿", System.Int32.MaxValue),
                    export.ExportName) })

/// The tier's pass list, in execution order.
let passes: Pass<HarvestModel> list = [ harvestExports; harvestDocs; orderExports ]
