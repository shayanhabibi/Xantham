/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; the tier's invariant is that every export of the entry module appears
/// exactly once, aliases followed to their origin.
module Xantham.Generator.Harvest

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The entry module's exports, each followed through `getAliasedSymbol` to its origin so that
/// re-exports and default-export aliases land on the declaring symbol.
///
/// A global type library (`@cloudflare/workers-types`, `@types/*` that declare no module) has
/// no module symbol at all. That is not an error and not an escape: the pass advances with
/// nothing, and `harvest-globals` picks the file's ambient declarations up instead.
let harvestExports: Pass<HarvestModel> =
    {
        Name = "harvest-exports"
        Run =
            fun ctx model ->
                async {
                    let! moduleSymbol =
                        ctx.Session.getSymbolOfSourceFile (DocumentIdentifier.FileName ctx.EntryFile)

                    match moduleSymbol with
                    | ValueNone -> return Advanced model
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
                                {
                                    ExportName = name
                                    Symbol = origin
                                    Docs = ""
                                    Tags = []
                                    Origin = FromModule
                                    Order = Grouping.declOrder origin.Declarations
                                })
                            |> Array.toList

                        return Advanced { model with Exports = harvested }
                }
    }

/// The entry package's ambient global declarations, for a package that declares no module at
/// all. Asking the checker for the symbols in scope at the top of the entry file returns the
/// whole global environment - three thousand names for `@cloudflare/workers-types`, two
/// thirds of them `lib.dom.d.ts` - so the result is filtered to the symbols the package
/// itself declares, by the same O7 placement the resolve tier groups types with.
///
/// Runs only when `harvest-exports` found nothing: a package with a module symbol may also
/// augment the global scope, and folding those globals into its exports would emit names the
/// package does not export.
let harvestGlobals: Pass<HarvestModel> =
    {
        Name = "harvest-globals"
        Run =
            fun ctx model ->
                async {
                    if not (List.isEmpty model.Exports) then
                        return Advanced model
                    else
                        // Types and values both: a global library is mostly interfaces and aliases,
                        // but `declare function`/`declare var` are exactly what needs `[<Global>]`.
                        let! symbols =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Type ||| SymbolFlags.Value,
                                file = DocumentIdentifier.FileName ctx.EntryFile,
                                position = 0
                            )

                        let ours =
                            symbols
                            |> Array.filter (fun symbol ->
                                Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage)

                        // An ambient module declaration (`declare module "cloudflare:email"`) is a
                        // global-scope symbol whose name is its quoted specifier. It is a module,
                        // not a type: its members are importable from that specifier, and emitting
                        // it as a declaration would need a nested module with its own imports. Until
                        // that exists, dropping it loudly beats a name F# cannot write.
                        let writable, unwritable =
                            ours |> Array.partition (fun symbol -> Naming.isWritableTypeName symbol.Name)

                        let findings =
                            unwritable
                            |> Array.map (fun symbol ->
                                let what =
                                    if symbol.Name.StartsWith "\"" then
                                        HarvestGlobals.AmbientModuleDropped
                                    else
                                        HarvestGlobals.UnwritableGlobalDropped

                                Finding.make symbol.Name what)
                            |> Array.toList

                        let harvested =
                            writable
                            |> Array.map (fun symbol ->
                                {
                                    ExportName = symbol.Name
                                    Symbol = symbol
                                    Docs = ""
                                    Tags = []
                                    Origin = FromGlobal
                                    Order = Grouping.declOrder symbol.Declarations
                                })
                            |> Array.toList

                        if List.isEmpty harvested && List.isEmpty findings then
                            return
                                Degraded(
                                    model,
                                    [ Finding.make "<module>" (HarvestGlobals.NothingHarvested ctx.EntryFile) ]
                                )
                        else
                            let model = { model with Exports = harvested }

                            return
                                if List.isEmpty findings then
                                    Advanced model
                                else
                                    Degraded(model, findings)
                }
    }

/// Documentation for every harvested export, from the checker rather than the syntax tree so
/// merged declarations already read as one.
let harvestDocs: Pass<HarvestModel> =
    {
        Name = "harvest-docs"
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
                                        Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                                    }
                            })
                        |> Async.Parallel

                    return
                        Advanced
                            { model with
                                Exports = Array.toList documented
                            }
                }
    }

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
                    export.ExportName)
        })

/// The tier's pass list, in execution order.
let passes: Pass<HarvestModel> list =
    [ harvestExports; harvestGlobals; harvestDocs; orderExports ]
