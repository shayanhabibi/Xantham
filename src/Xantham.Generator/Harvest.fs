/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; every entry-module export and exported type member of a harvested
/// namespace appears once per public origin and name, aliases followed to their origin.
module Xantham.Generator.Harvest

open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// Where the entry file's whole in-scope environment resolved, grouped and counted, for a run
/// where none of it classified as the entry package (reading `Grouping.classify`'s existing
/// groups, not adding one). Most populous group first, so a reference chain dominated by one
/// group names that group up front.
let private elsewhere (packageDir: string<dirPath>) (symbols: SymbolResponse[]) =
    symbols
    |> Array.countBy (fun symbol -> Grouping.classify packageDir (ValueSome symbol))
    |> Array.sortByDescending snd
    |> Array.map (fun (origin, count) ->
        let label =
            GeneratorConfig.groupKey origin
            |> Option.map (fun name -> name / uom<npmDependency>)
            |> Option.defaultValue "unclassified declarations"

        $"{count} in {label}")
    |> String.concat ", "

/// `path` spelled relative to `packageDir` with forward separators, so a finding that carries a
/// file reads the same on every machine. A path outside the package keeps its own spelling.
let internal underPackage (packageDir: string) (path: string) =
    let relative = System.IO.Path.GetRelativePath(packageDir, path)

    if relative.StartsWith ".." then
        path.Replace('\\', '/')
    else
        relative.Replace('\\', '/')

/// The namespace symbols among `symbols`, by id, for the declarations written inside them to
/// nest under. An ambient module declaration is a namespace symbol whose name is its quoted
/// specifier (`"cloudflare:workers"`), which heads no F# module, so the map holds only names a
/// declaration can take a path segment from.
let private namespacesAmong (symbols: SymbolResponse seq) =
    symbols
    |> Seq.filter (fun symbol -> hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name)
    |> Seq.map (fun symbol -> symbol.SymbolId, symbol.SymbolName)
    |> Map.ofSeq

/// One export, followed through `getAliasedSymbol` so that `export { _connect as connect }`
/// lands on the declaring symbol under the exported name.
let private followAlias (ctx: Context) (export: SymbolResponse) =
    async {
        if export.Flags.HasFlag SymbolFlags.Alias then
            let! origin = ctx.Session.getAliasedSymbol export.Reference
            return export.Name, origin
        else
            return export.Name, export
    }

/// A public path's exports, harvested under `origin`, with the resolved origin symbols (for
/// `namespacesAmong`). A path backed by a global-script file - a global type library such as
/// `@cloudflare/workers-types` or a module-free `@types/*` - yields an empty list, and
/// `harvest-globals` supplies its ambient declarations alongside the module exports.
let private harvestPublicPath
    (ctx: Context)
    (origin: ExportOrigin)
    (file: string<declFile>)
    : Async<HarvestedExport list * SymbolResponse list> =
    async {
        let! moduleSymbol =
            ctx.Session.getSymbolOfSourceFile (DocumentIdentifier.FileName(file / uom<_>))

        match moduleSymbol with
        | ValueNone -> return [], []
        | ValueSome moduleSymbol ->
            let! exports = ctx.Session.getExportsOfModule moduleSymbol.Reference
            let exports = exports |> ValueOption.defaultValue [||]

            // Fan out the alias-following freely - the mailbox coalesces it - but
            // Async.Parallel's result order is input order, so the fold is deterministic.
            let! resolved = exports |> Array.map (followAlias ctx) |> Async.Parallel

            let valueExport = ExportProvenance.reader ctx

            let! harvested =
                resolved
                |> Array.sortBy fst
                |> Array.map (fun (name, symbol) ->
                    async {
                        let! hasValueExport = valueExport moduleSymbol name

                        return
                            {
                                ExportName = name
                                Symbol = symbol
                                DeclaringNamespace = None
                                NamespacePath = None
                                HasValueExport = hasValueExport
                                Docs = ""
                                Tags = []
                                Origin = origin
                                Order = Grouping.declOrder symbol.Declarations
                            }
                    })
                |> Async.Sequential

            return harvested |> Array.toList, resolved |> Array.map snd |> Array.toList
    }

/// Every public path's exports, root first then each subpath in `PublicPaths` order, each
/// followed through `getAliasedSymbol` to its origin so that re-exports and default-export
/// aliases land on the declaring symbol. The root binds `[<Import(name, package)>]`; a
/// subpath binds under its own specifier, `"<runtime>/<key without the leading ./>"`.
///
/// A skipped `exports` key - a wildcard, or a key without declarations - raises the finding
/// recorded for it in `Context.SkippedPaths`, degrading the model and leaving the run intact.
let harvestExports: Pass<HarvestModel> =
    {
        Name = "harvest-exports"
        Run =
            fun ctx model ->
                async {
                    let runtime =
                        GeneratorConfig.runtimePackage ctx.Config ctx.PackageName / uom<importSpecifier>

                    let originOf (path: PublicPath) =
                        if path.Key = "." then
                            FromModule
                        else
                            FromAmbientModule($"{runtime}/{path.Key.Substring 2}" * uom<importSpecifier>)

                    let! perPath =
                        ctx.PublicPaths
                        |> List.map (fun path -> harvestPublicPath ctx (originOf path) path.File)
                        |> Async.Sequential

                    let harvested = perPath |> Array.toList |> List.collect fst
                    let resolvedSymbols = perPath |> Array.toList |> List.collect snd

                    let skipped =
                        ctx.SkippedPaths |> List.map (fun (key, finding) -> Finding.make key finding)

                    // A namespace a public path's file declares without exporting is still the
                    // owner of the types an exported signature reaches through it, so the scope
                    // at the top of every path's file is asked for rather than the export list.
                    let! inScope =
                        ctx.PublicPaths
                        |> List.map (fun path ->
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName(path.File / uom<_>),
                                position = 0
                            ))
                        |> Async.Sequential

                    let namespaces =
                        inScope
                        |> Array.toList
                        |> List.collect Array.toList
                        |> List.filter (fun symbol ->
                            Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage)
                        |> List.append resolvedSymbols
                        |> namespacesAmong

                    let model =
                        { model with
                            Exports = harvested
                            Namespaces = namespaces
                        }

                    return
                        if List.isEmpty skipped then
                            Advanced model
                        else
                            Degraded(model, skipped)
                }
    }

let private isModuleLocal (ctx: Context) (symbol: SymbolResponse) =
    async {
        let! declarations =
            symbol.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.map (fun handle ->
                async {
                    match NodeHandle.parse handle with
                    | ValueNone -> return false
                    | ValueSome handle ->
                        let! source = ctx.Session.getSourceFile (DocumentIdentifier.FileName handle.Path)

                        match source with
                        | ValueNone -> return false
                        | ValueSome source ->
                            let globalBlocks =
                                Ast.moduleAugmentations source
                                |> Seq.map (Node.ofIndex<AnyNode> source)
                                |> Seq.filter (fun name ->
                                    name.Kind = SyntaxKind.Identifier && name.Text = ValueSome "global")
                                |> Seq.choose (Node.parent >> ValueOption.toOption)
                                |> Seq.toArray

                            let rec scope (node: Node<AnyNode>) =
                                if globalBlocks |> Array.exists (Node.sameAs node) then
                                    Some false
                                elif
                                    node.Kind = SyntaxKind.ModuleDeclaration
                                    && ModuleDeclaration.name (Node.retag<AnyNode, ModuleDeclaration> node)
                                       |> ValueOption.exists (fun name -> name.Kind = SyntaxKind.StringLiteral)
                                then
                                    Some true
                                else
                                    Node.parent node |> ValueOption.toOption |> Option.bind scope

                            match scope (Node.ofIndex<AnyNode> source handle.Index) with
                            | Some local -> return local
                            | None ->
                                let! moduleSymbol =
                                    ctx.Session.getSymbolOfSourceFile (DocumentIdentifier.FileName handle.Path)

                                return ValueOption.isSome moduleSymbol
                })
            |> Async.Sequential

        return not (Array.isEmpty declarations) && declarations |> Array.forall id
    }

/// An ambient module's exports, findings, and optional namespace body or module-local root
/// value omitted from the global exports.
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
            let! exports = ctx.Session.getExportsOfModule moduleSymbol.Reference
            let exports = exports |> ValueOption.defaultValue [||]

            let! resolved, rootValue =
                async {
                    if exports.Length > 0 then
                        let! resolved = exports |> Array.map (followAlias ctx) |> Async.Parallel
                        return resolved, None
                    else
                        let! direct = ctx.Session.Sessionless.getExportsOfSymbol moduleSymbol.Reference

                        match
                            direct
                            |> ValueOption.defaultValue [||]
                            |> Array.tryFind (fun symbol -> symbol.Name = "export=")
                        with
                        | Some assignment ->
                            let! _, target = followAlias ctx assignment

                            if
                                hasAny SymbolFlags.Value target.Flags
                                && target.Declarations |> ValueOption.exists (Array.isEmpty >> not)
                            then
                                let! localRoot = isModuleLocal ctx target
                                return [| "default", target |], (if localRoot then Some target.SymbolId else None)
                            else
                                return [||], None
                        | None -> return [||], None
                }

            if resolved.Length = 0 then
                return [], [ Finding.make moduleSymbol.Name HarvestGlobals.AmbientModuleDropped ], None
            else
                let! parent =
                    if exports.Length = 0 then
                        async.Return ValueNone
                    else
                        ctx.Session.Sessionless.getParentOfSymbol exports[0].Reference

                let body =
                    parent
                    |> ValueOption.filter (fun (p: SymbolResponse) -> p.Name <> moduleSymbol.Name)
                    |> ValueOption.map (fun p -> p.SymbolId, p.Name)
                    |> ValueOption.toOption

                let valueExport = ExportProvenance.reader ctx

                let! harvested =
                    resolved
                    |> Array.sortBy fst
                    |> Array.map (fun (name, origin) ->
                        async {
                            let! hasValueExport = valueExport moduleSymbol name

                            return
                                {
                                    ExportName = name
                                    Symbol = origin
                                    DeclaringNamespace = None
                                    NamespacePath = None
                                    HasValueExport = hasValueExport
                                    Docs = ""
                                    Tags = []
                                    Origin = FromAmbientModule(specifier * uom<importSpecifier>)
                                    Order = Grouping.declOrder origin.Declarations
                                }
                        })
                    |> Async.Sequential

                let findings =
                    [
                        Finding.make
                            moduleSymbol.Name
                            (HarvestGlobals.AmbientModuleHarvested(specifier, harvested.Length))

                        match body with
                        | Some(_, name) -> Finding.make name (HarvestGlobals.NamespaceIsModuleBody(name, specifier))
                        | None -> ()
                    ]

                return harvested |> Array.toList, findings, rootValue |> Option.orElse (body |> Option.map fst)
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
/// For `@types/node`, collapses `node:X` and bare `X` ambient-module specifiers onto one
/// module, spelled `node:X`: every builtin's exports bind `[<Import(name, "node:X")>]`,
/// and a builtin split across both spellings nests under one F# module. Members from both
/// spellings are carried under the collapsed specifier; a divergence between the two
/// spellings' export sets additionally raises a finding.
let private collapseNodeAliases (ctx: Context) (exports: HarvestedExport list) =
    if ctx.PackageName / uom<npmDependency> <> "@types/node" then
        exports, []
    else
        let bare (specifier: string) =
            if specifier.StartsWith "node:" then
                specifier.Substring 5
            else
                specifier

        let findings =
            exports
            |> List.choose (fun export ->
                match export.Origin with
                | FromAmbientModule specifier -> Some(specifier / uom<importSpecifier>, export.ExportName)
                | _ -> None)
            |> List.groupBy (fst >> bare)
            |> List.choose (fun (name, occurrences) ->
                let spellings = occurrences |> List.map fst |> List.distinct

                let exportsOf spelling =
                    occurrences
                    |> List.filter (fun (specifier, _) -> specifier = spelling)
                    |> List.map snd
                    |> Set.ofList

                match spellings with
                | [ _ ] -> None
                | _ when spellings |> List.map exportsOf |> List.distinct |> List.length = 1 -> None
                | _ -> Some(Finding.make name (HarvestGlobals.AmbientModuleAliasDivergent(name, spellings))))

        let collapsed =
            exports
            |> List.map (fun export ->
                match export.Origin with
                | FromAmbientModule specifier ->
                    { export with
                        Origin =
                            FromAmbientModule($"node:{bare (specifier / uom<importSpecifier>)}" * uom<importSpecifier>)
                    }
                | _ -> export)
            |> List.groupBy (fun export -> export.Origin, export.ExportName)
            |> List.map (fun (_, occurrences) ->
                occurrences
                |> List.tryFind _.HasValueExport
                |> Option.defaultValue (List.head occurrences))

        collapsed, findings

/// Harvests package globals when a public input is a global script, module exports are empty,
/// or compiler libraries ship alongside the entry package.
/// A mixed run reads the global script's scope and retains the public module exports.
let harvestGlobals: Pass<HarvestModel> =
    {
        Name = "harvest-globals"
        Run =
            fun ctx model ->
                async {
                    let! globalFiles =
                        ctx.PublicPaths
                        |> List.map (fun path ->
                            async {
                                let! symbol =
                                    ctx.Session.getSymbolOfSourceFile (
                                        DocumentIdentifier.FileName(path.File / uom<declFile>)
                                    )

                                return if ValueOption.isNone symbol then Some path.File else None
                            })
                        |> Async.Sequential

                    let globalFile = globalFiles |> Array.tryPick id
                    let shipsCompilerLib = GeneratorConfig.disposition ctx.Config CompilerLib = Ship

                    let! libraryFile =
                        async {
                            if shipsCompilerLib && Option.isNone globalFile then
                                // Module locals can shadow library names and are not globals.
                                // A library file exposes the program's actual global scope.
                                let! files = ctx.Session.getSourceFileNames ()

                                let! libraries =
                                    files
                                    |> Array.map (fun file ->
                                        async {
                                            let! metadata =
                                                ctx.Session.getSourceFileMetadata (DocumentIdentifier.FileName file)

                                            return
                                                if metadata |> ValueOption.exists _.IsDefaultLibrary then
                                                    Some(file * uom<declFile>)
                                                else
                                                    None
                                        })
                                    |> Async.Parallel

                                return libraries |> Array.tryPick id
                            else
                                return None
                        }

                    if
                        not (List.isEmpty model.Exports)
                        && Option.isNone globalFile
                        && Option.isNone libraryFile
                    then
                        return Advanced model
                    else
                        let scopeFile =
                            globalFile |> Option.orElse libraryFile |> Option.defaultValue ctx.EntryFile

                        // Types and values both: a global library is mostly interfaces and aliases,
                        // but `declare function`/`declare var` are exactly what needs `[<Global>]`.
                        let! symbols =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Type ||| SymbolFlags.Value,
                                file = DocumentIdentifier.FileName(scopeFile / uom<declFile>),
                                position = 0
                            )

                        // A run that ships the compiler-lib group (`xantham.json`'s
                        // `"typescript/lib": "ship"`) harvests its declarations alongside the
                        // entry package's own: the shape tier already renders such a name in
                        // full rather than through `libBinding` (`Shape/Spec.fs`), so a
                        // declaration this pass withholds never reaches anything downstream that
                        // could act on it.
                        let admits origin =
                            origin = EntryPackage || (origin = CompilerLib && shipsCompilerLib)

                        let ours =
                            symbols
                            |> Array.filter (fun symbol -> admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))

                        // A name a `lib.*.d.ts` declaration precedes classifies as the compiler
                        // lib (`Grouping.classify` reads only the first declaration), so such a
                        // symbol never reaches `ours` above, unless `shipsCompilerLib` admitted it
                        // there already. This still finds the rest: any of the symbol's
                        // declarations sitting under the package directory is this package's own
                        // contribution to the merge, lost to `Exports` all the same.
                        let shadowedByLib =
                            symbols
                            |> Array.filter (fun symbol ->
                                not (admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))
                                && Grouping.declaresUnderPackage ctx.PackageDir symbol)
                            |> Array.length

                        // A namespace of types alone is neither a type nor a value, so
                        // `TailStream` arrives under `Module` and nowhere else. Its members
                        // reach the shape tier through the types that refer to them, and the
                        // namespace is what separates two declarations of one name.
                        let! declared =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName(scopeFile / uom<declFile>),
                                position = 0
                            )

                        let namespaces =
                            declared
                            |> Array.filter (fun symbol -> admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))
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
                                    if not (Set.contains symbol.SymbolId moduleBodies) then
                                        {
                                            ExportName = symbol.Name
                                            Symbol = symbol
                                            DeclaringNamespace = None
                                            NamespacePath = None
                                            HasValueExport = hasAny SymbolFlags.Value symbol.Flags
                                            Docs = ""
                                            Tags = []
                                            Origin = FromGlobal
                                            Order = Grouping.declOrder symbol.Declarations
                                        }

                                for symbol in declared do
                                    if
                                        admits (Grouping.classify ctx.PackageDir (ValueSome symbol))
                                        && Naming.isWritableTypeName symbol.Name
                                        && not (hasAny (SymbolFlags.Type ||| SymbolFlags.Value) symbol.Flags)
                                        && not (Set.contains symbol.SymbolId moduleBodies)
                                    then
                                        {
                                            ExportName = symbol.Name
                                            Symbol = symbol
                                            DeclaringNamespace = None
                                            NamespacePath = None
                                            HasValueExport = false
                                            Docs = ""
                                            Tags = []
                                            Origin = FromGlobal
                                            Order = Grouping.declOrder symbol.Declarations
                                        }

                                for exports, _, _ in fromModules do
                                    yield! exports
                            ]

                        let harvested, aliasFindings = collapseNodeAliases ctx harvested
                        let findings = findings @ aliasFindings

                        if List.isEmpty model.Exports && List.isEmpty harvested && List.isEmpty findings then
                            return
                                Degraded(
                                    { model with
                                        ShadowedByLib = shadowedByLib
                                    },
                                    [
                                        Finding.make
                                            "<module>"
                                            (HarvestGlobals.NothingHarvested(
                                                underPackage
                                                    (ctx.PackageDir / uom<dirPath>)
                                                    (ctx.EntryFile / uom<declFile>),
                                                symbols.Length,
                                                elsewhere ctx.PackageDir symbols
                                            ))
                                    ]
                                )
                        else
                            let model =
                                { model with
                                    Exports =
                                        model.Exports @ harvested
                                        |> List.distinctBy (fun export ->
                                            export.Origin, export.ExportName, export.Symbol.SymbolId)
                                    Namespaces =
                                        namespaces
                                        |> Map.fold (fun names id name -> Map.add id name names) model.Namespaces
                                    ShadowedByLib = shadowedByLib
                                }

                            return
                                if List.isEmpty findings then
                                    Advanced model
                                else
                                    Degraded(model, findings)
                }
    }

/// The exported interfaces, aliases, classes and enums of every harvested namespace, nested
/// namespaces included, each harvested under its namespace's origin. A member carries no value
/// export of its own: its value binds through the namespace object. Every walked namespace joins
/// `Namespaces`, a nested one under its dotted path (`Pool.Inner`). A module package's global
/// namespaces (`declare global { namespace X }`) are harvested as global exports first. Only
/// namespaces and members whose group ships are walked.
let harvestNamespaceMembers: Pass<HarvestModel> =
    {
        Name = "harvest-namespace-members"
        Run =
            fun ctx model ->
                async {
                    let ships (symbol: SymbolResponse) =
                        GeneratorConfig.disposition ctx.Config (Grouping.classify ctx.PackageDir (ValueSome symbol)) =
                            Ship

                    let isNamespace (symbol: SymbolResponse) =
                        hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name

                    let memberKinds =
                        SymbolFlags.Interface
                        ||| SymbolFlags.TypeAlias
                        ||| SymbolFlags.Class
                        ||| SymbolFlags.Enum

                    // A module's local scope can shadow globals, and `declare global` members
                    // have the augmentation symbol as their parent. Read the explicit global
                    // blocks instead of inferring global ownership from an absent parent.
                    let harvestedIds = model.Exports |> List.map _.Symbol.SymbolId |> Set.ofList
                    let! files = ctx.Session.getSourceFileNames ()

                    let! globalNamespaces =
                        files
                        |> Array.filter (fun file ->
                            not ((underPackage (ctx.PackageDir / uom<dirPath>) file).StartsWith ".."))
                        |> Array.map (fun file ->
                            async {
                                let! source = ctx.Session.getSourceFile (DocumentIdentifier.FileName file)

                                let declarations =
                                    source
                                    |> ValueOption.toOption
                                    |> Option.map (fun source ->
                                        Ast.moduleAugmentations source
                                        |> Seq.map (Node.ofIndex<AnyNode> source)
                                        |> Seq.filter (fun name ->
                                            name.Kind = SyntaxKind.Identifier && name.Text = ValueSome "global")
                                        |> Seq.choose (Node.parent >> ValueOption.toOption)
                                        |> Seq.filter (fun node -> node.Kind = SyntaxKind.ModuleDeclaration)
                                        |> Seq.map Node.retag<AnyNode, ModuleDeclaration>
                                        |> Seq.collect (fun node ->
                                            match ModuleDeclaration.body node with
                                            | ValueSome body when body.Kind = SyntaxKind.ModuleBlock ->
                                                ModuleBlock.statements (Node.retag<ModuleBody, ModuleBlock> body)
                                            | _ -> Seq.empty)
                                        |> Seq.filter (fun node -> node.Kind = SyntaxKind.ModuleDeclaration)
                                        |> Seq.choose (fun node ->
                                            ModuleDeclaration.name (Node.retag<Statement, ModuleDeclaration> node)
                                            |> ValueOption.toOption)
                                        |> Seq.toArray)
                                    |> Option.defaultValue [||]

                                let! symbols =
                                    declarations
                                    |> Array.map (fun node ->
                                        ctx.Session.getSymbolAtLocation (
                                            NodeHandle.format
                                                {
                                                    Index = Node.index node
                                                    Kind = node.Kind
                                                    Path = Ast.path (Node.file node)
                                                }
                                        ))
                                    |> Async.Sequential

                                return symbols |> Array.choose ValueOption.toOption
                            })
                        |> Async.Sequential

                    let packageGlobals =
                        globalNamespaces
                        |> Array.collect id
                        |> Array.filter (fun symbol ->
                            isNamespace symbol
                            && Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage
                            && not (Set.contains symbol.SymbolId harvestedIds))
                        |> Array.distinctBy _.SymbolId
                        |> Array.sortBy _.Name
                        |> Array.map (fun symbol ->
                            {
                                ExportName = symbol.Name
                                Symbol = symbol
                                DeclaringNamespace = None
                                NamespacePath = None
                                HasValueExport = hasAny SymbolFlags.Value symbol.Flags
                                Docs = ""
                                Tags = []
                                Origin = FromGlobal
                                Order = Grouping.declOrder symbol.Declarations
                            })
                        |> Array.toList

                    let canonicalPath (symbol: SymbolResponse) fallback =
                        Map.tryFind symbol.SymbolId model.Namespaces
                        |> Option.map (fun name -> name / uom<symbolName>)
                        |> Option.defaultValue fallback

                    let rec walk
                        visited
                        (path: string)
                        (sourcePath: string)
                        (origin: ExportOrigin)
                        (ns: SymbolResponse)
                        =
                        async {
                            let! exports = ctx.Session.getExportsOfModule ns.Reference

                            let! resolved =
                                exports
                                |> ValueOption.defaultValue [||]
                                |> Array.map (followAlias ctx)
                                |> Async.Parallel

                            let candidates =
                                resolved |> Array.sortBy fst |> Array.filter (snd >> ships) |> Array.toList

                            let members =
                                candidates
                                |> List.filter (fun (_, symbol) -> hasAny memberKinds symbol.Flags)
                                |> List.map (fun (name, symbol) ->
                                    {
                                        ExportName = name
                                        Symbol = symbol
                                        DeclaringNamespace = Some ns.SymbolId
                                        NamespacePath = Some path
                                        HasValueExport = false
                                        Docs = ""
                                        Tags = []
                                        Origin = origin
                                        Order = Grouping.declOrder symbol.Declarations
                                    })

                            let rec descend visited found named pending =
                                async {
                                    match pending with
                                    | [] -> return visited, found, named
                                    | (exportName, (symbol: SymbolResponse)) :: rest when
                                        isNamespace symbol && not (Set.contains symbol.SymbolId visited)
                                        ->
                                        let nested = $"{path}.{exportName}"
                                        let sourceNested = canonicalPath symbol $"{sourcePath}.{symbol.Name}"

                                        let! _, inner, innerNamed =
                                            walk (Set.add symbol.SymbolId visited) nested sourceNested origin symbol

                                        return!
                                            descend
                                                visited
                                                (found @ inner)
                                                (named
                                                 @ [ symbol.SymbolId, sourceNested * uom<symbolName> ]
                                                 @ innerNamed)
                                                rest
                                    | _ :: rest -> return! descend visited found named rest
                                }

                            return! descend visited members [] candidates
                        }

                    let exports = model.Exports @ packageGlobals

                    let seeds =
                        exports
                        |> List.filter (fun export -> isNamespace export.Symbol && ships export.Symbol)
                        |> List.distinctBy (fun export -> export.Origin, export.ExportName, export.Symbol.SymbolId)
                        |> List.sortBy (fun export -> export.ExportName <> export.Symbol.Name)

                    let pathOf (seed: HarvestedExport) =
                        if seed.ExportName <> seed.Symbol.Name && seed.ExportName <> "default" then
                            seed.ExportName
                        else
                            Map.tryFind seed.Symbol.SymbolId model.Namespaces
                            |> Option.map (fun name -> name / uom<symbolName>)
                            |> Option.defaultValue seed.Symbol.Name

                    let! _, harvested, named =
                        seeds
                        |> List.fold
                            (fun state (seed: HarvestedExport) ->
                                async {
                                    let! visited, found, named = state
                                    let path = pathOf seed
                                    let sourcePath = canonicalPath seed.Symbol seed.Symbol.Name

                                    let! _, inner, innerNamed =
                                        walk
                                            (Set.singleton seed.Symbol.SymbolId)
                                            path
                                            sourcePath
                                            seed.Origin
                                            seed.Symbol

                                    return
                                        visited,
                                        found @ inner,
                                        named @ [ seed.Symbol.SymbolId, sourcePath * uom<symbolName> ] @ innerNamed
                                })
                            (async.Return(seeds |> List.map _.Symbol.SymbolId |> Set.ofList, [], []))

                    return
                        Advanced
                            { model with
                                Exports =
                                    exports @ harvested
                                    |> List.distinctBy (fun export ->
                                        export.Origin,
                                        export.DeclaringNamespace,
                                        export.NamespacePath,
                                        export.ExportName,
                                        export.Symbol.SymbolId)
                                Namespaces =
                                    named
                                    |> List.fold
                                        (fun names (id, name) ->
                                            if Map.containsKey id names then
                                                names
                                            else
                                                Map.add id name names)
                                        model.Namespaces
                            }
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
                                let! docs = ctx.Session.getDocumentationComment export.Symbol.Reference
                                let! tags = ctx.Session.getJsDocTags export.Symbol.Reference

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

/// Entry declarations first, then package-relative dependency and compiler sources.
/// Node index and export name break ties; exports without declarations sort last. Occurrences
/// of one declaration then order by public path: the declaring namespace's own path before an
/// alias path such as `globalThis.Intl`, then by ordinal path. Occurrences that still tie keep
/// harvest-pass order, entry module first. The order will be identical across runs whatever
/// order the checker enumerates scopes in.
let orderExports: Pass<HarvestModel> =
    Pass.pure' "harvest-order" (fun ctx model ->
        let aliasPath (export: HarvestedExport) =
            match export.NamespacePath with
            | None -> false
            | Some path ->
                export.DeclaringNamespace
                |> Option.bind (fun id -> Map.tryFind id model.Namespaces)
                |> Option.forall (fun source -> source / uom<symbolName> <> path)

        { model with
            Exports =
                model.Exports
                |> List.sortBy (fun export ->
                    (match export.Order with
                     | Some order -> Grouping.sourceOrderKey ctx.PackageDir (order.File / uom<node>), order.NodeIndex
                     | None -> (2, "", ""), (System.Int32.MaxValue * uom<Measure.nodeId>)),
                    export.ExportName,
                    aliasPath export,
                    export.NamespacePath |> Option.defaultValue "")
        })

/// Runtime ambient class exports available to declarations reached through dependencies.
let harvestAmbientClasses: Pass<HarvestModel> =
    {
        Name = "harvest-ambient-classes"
        Run =
            fun ctx model ->
                async {
                    let! modules =
                        ctx.Session.getSymbolsInScope (
                            SymbolFlags.Module,
                            file = DocumentIdentifier.FileName(ctx.EntryFile / uom<declFile>),
                            position = 0
                        )

                    let! exports =
                        modules
                        |> Array.filter (fun symbol ->
                            symbol.Name.StartsWith "\""
                            && GeneratorConfig.disposition
                                ctx.Config
                                (Grouping.classify ctx.PackageDir (ValueSome symbol))
                                =
                                Ship)
                        |> Array.sortBy _.Name
                        |> Array.map (harvestAmbientModule ctx)
                        |> Async.Sequential

                    let classes =
                        exports
                        |> Array.toList
                        |> List.collect (fun (exports, _, _) -> exports)
                        |> List.filter (fun export ->
                            export.HasValueExport
                            && hasAny SymbolFlags.Class export.Symbol.Flags
                            && GeneratorConfig.disposition
                                ctx.Config
                                (Grouping.classify ctx.PackageDir (ValueSome export.Symbol))
                                =
                                Ship)

                    return Advanced { model with AmbientClasses = classes }
                }
    }

/// The tier's pass list, in execution order.
let passes: Pass<HarvestModel> list =
    [
        harvestExports
        harvestGlobals
        harvestNamespaceMembers
        harvestAmbientClasses
        harvestDocs
        orderExports
    ]
