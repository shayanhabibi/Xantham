/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; the tier's invariant is that every export of the entry module appears
/// exactly once, aliases followed to their origin.
module Xantham.Generator.Harvest

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The namespace symbols among `symbols`, by id, for the declarations written inside them to
/// nest under. An ambient module declaration is a namespace symbol whose name is its quoted
/// specifier (`"cloudflare:workers"`), which heads no F# module, so the map holds only names a
/// declaration can take a path segment from.
let private namespacesAmong (symbols: SymbolResponse seq) =
    symbols
    |> Seq.filter (fun symbol -> hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name)
    |> Seq.map (fun symbol -> symbol.Id, symbol.Name)
    |> Map.ofSeq

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

                        // A namespace the entry file declares without exporting is still the
                        // owner of the types an exported signature reaches through it, so the
                        // scope at the top of the file is asked for rather than the export list.
                        let! inScope =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName ctx.EntryFile,
                                position = 0
                            )

                        let namespaces =
                            inScope
                            |> Array.filter (fun symbol ->
                                Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage)
                            |> Array.append (resolved |> Array.map snd)
                            |> namespacesAmong

                        return
                            Advanced
                                { model with
                                    Exports = harvested
                                    Namespaces = namespaces
                                }
                }
    }

/// One export of an ambient module, followed through `getAliasedSymbol` so that
/// `export { _connect as connect }` lands on the declaring symbol under the exported name.
let private followAlias (ctx: Context) (export: SymbolResponse) =
    async {
        if export.Flags.HasFlag SymbolFlags.Alias then
            let! origin = ctx.Session.getAliasedSymbol export.Id
            return export.Name, origin
        else
            return export.Name, export
    }

/// One ambient module declaration, harvested: its exports under the specifier they import
/// from, the findings the declaration raises, and the symbol id of the namespace it re-exports
/// where it is written `export = Namespace`.
///
/// `getExportsOfModule` resolves `export =` in place, so the exports of `cloudflare:workers` are
/// the members of `CloudflareWorkersModule`, and `getParentOfSymbol` over any of them returns
/// that namespace. Such a namespace is the module's body, reachable through the specifier
/// alone; a `[<Global>]` binding to it reads `undefined`.
let private harvestAmbientModule (ctx: Context) (moduleSymbol: SymbolResponse) =
    async {
        let specifier = moduleSymbol.Name.Trim '"'

        if specifier.Contains "*" then
            return
                [],
                [
                    Finding.make moduleSymbol.Name (HarvestGlobals.AmbientModuleWildcard specifier)
                ],
                None
        else
            let! exports = ctx.Session.getExportsOfModule moduleSymbol.Id
            let exports = exports |> ValueOption.defaultValue [||]

            if exports.Length = 0 then
                return [], [ Finding.make moduleSymbol.Name HarvestGlobals.AmbientModuleDropped ], None
            else
                let! resolved = exports |> Array.map (followAlias ctx) |> Async.Parallel
                let! parent = ctx.Session.getParentOfSymbol exports[0].Id

                let body =
                    parent
                    |> ValueOption.filter (fun (p: SymbolResponse) -> p.Name <> moduleSymbol.Name)
                    |> ValueOption.map (fun p -> p.Id, p.Name)
                    |> ValueOption.toOption

                let harvested =
                    resolved
                    |> Array.sortBy fst
                    |> Array.map (fun (name, origin) ->
                        {
                            ExportName = name
                            Symbol = origin
                            Docs = ""
                            Tags = []
                            Origin = FromAmbientModule specifier
                            Order = Grouping.declOrder origin.Declarations
                        })
                    |> Array.toList

                let findings =
                    [
                        Finding.make
                            moduleSymbol.Name
                            (HarvestGlobals.AmbientModuleHarvested(specifier, harvested.Length))

                        match body with
                        | Some(_, name) -> Finding.make name (HarvestGlobals.NamespaceIsModuleBody(name, specifier))
                        | None -> ()
                    ]

                return harvested, findings, body |> Option.map fst
    }

/// The entry package's ambient global declarations, for a package that declares no module at
/// all. Asking the checker for the symbols in scope at the top of the entry file returns the
/// whole global environment - three thousand names for `@cloudflare/workers-types`, two
/// thirds of them `lib.dom.d.ts` - so the result is filtered to the symbols the package
/// itself declares, by the same O7 placement the resolve tier groups types with.
///
/// An ambient module declaration (`declare module "cloudflare:email"`) arrives here too, under
/// a symbol name that is its quoted specifier. Its exports are harvested under
/// `FromAmbientModule`: the types are declared beside the package's globals, and the values
/// carry the specifier's own import.
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

                        // A namespace of types alone is neither a type nor a value, so
                        // `TailStream` arrives under `Module` and nowhere else. Its members
                        // reach the shape tier through the types that refer to them, and the
                        // namespace is what separates two declarations of one name.
                        let! declared =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName ctx.EntryFile,
                                position = 0
                            )

                        let namespaces =
                            declared
                            |> Array.filter (fun symbol ->
                                Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage)
                            |> namespacesAmong

                        // An ambient module declaration is a global-scope symbol whose name is
                        // its quoted specifier. Its exports are harvested under that specifier;
                        // the specifier itself heads no declaration.
                        let writable, unwritable =
                            ours |> Array.partition (fun symbol -> Naming.isWritableTypeName symbol.Name)

                        let modules, unnameable =
                            unwritable |> Array.partition (fun symbol -> symbol.Name.StartsWith "\"")

                        let! fromModules =
                            modules
                            |> Array.sortBy _.Name
                            |> Array.map (harvestAmbientModule ctx)
                            |> Async.Parallel

                        let moduleBodies =
                            fromModules |> Array.choose (fun (_, _, body) -> body) |> Set.ofArray

                        let findings =
                            [
                                for symbol in unnameable do
                                    Finding.make symbol.Name HarvestGlobals.UnwritableGlobalDropped

                                for _, moduleFindings, _ in fromModules do
                                    yield! moduleFindings
                            ]

                        let harvested =
                            [
                                for symbol in writable do
                                    if not (Set.contains symbol.Id moduleBodies) then
                                        {
                                            ExportName = symbol.Name
                                            Symbol = symbol
                                            Docs = ""
                                            Tags = []
                                            Origin = FromGlobal
                                            Order = Grouping.declOrder symbol.Declarations
                                        }

                                for exports, _, _ in fromModules do
                                    yield! exports
                            ]

                        if List.isEmpty harvested && List.isEmpty findings then
                            return
                                Degraded(
                                    model,
                                    [ Finding.make "<module>" (HarvestGlobals.NothingHarvested ctx.EntryFile) ]
                                )
                        else
                            let model =
                                { model with
                                    Exports = harvested
                                    Namespaces = namespaces
                                }

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
