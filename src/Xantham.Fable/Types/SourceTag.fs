/// <summary>
/// This module is a silo contained type and implementation for the creation of the SourceTag.
/// </summary>
module Xantham.Fable.Types.SourceTag

open System.Collections.Generic
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Tracer
open Fable.Core.DynamicExtensions


type ConditionalPackageExport =
    | Types of PackageExportKind
    | Default of PackageExportKind
    | Browser of PackageExportKind
    | Development of PackageExportKind
    | Production of PackageExportKind
    | NodeAddons of PackageExportKind
    | Node of PackageExportKind
    | Import of PackageExportKind
    | Require of PackageExportKind
    | ModuleSync of PackageExportKind
    | Unknown of key: string * value: PackageExportKind

and PackageExportKind =
    | String of string
    | Conditional of ConditionalPackageExport list

/// <summary>
/// Export metadata from a <c>package.json</c> file.
/// This serves no purpose in the <c>Xantham.Fable</c> encoder at this time, and
/// is purposed as metadata for consumers.
/// </summary>
type PackageExport =
    | Abbrev of string * PackageExportKind
    | Full of (string * PackageExportKind) list

/// <summary>
/// LibEs default packages are not tracked further.
/// The DU serves as the point of identification so that descendant nodes will
/// cease searching up their ancestry for a package.
/// </summary>
type SourceKind =
    | LibEs
    | Package of Ts.Symbol

type SourceGuard =
    inherit GuardTracer
    abstract Source: Ts.SourceFile
    abstract PackageDirectory: string voption
    abstract PackageJsonContent: PackageJsonPathFields voption
    
type SourceTag =
    inherit GuardedTracer<SourceKind, SourceGuard>
    
type PackageInfo = {
    Name: string
    Version: string
    AssociatedTags: SourceTag array
}

/// <summary>
/// Storage for the Ts.Program on the source file.
/// </summary>
let private programKey = SymbolTypeKey.create<Ts.Program> "Program"
/// <summary>
/// Stores the export metadata that will be provided for consumers.
/// </summary>
let private exportKey = SymbolTypeKey.create<PackageExport voption> "PackageExports"
/// <summary>
/// Tracks dependencies of a source tag
/// </summary>
let private dependencyKeys = SymbolTypeKey.create<SourceTag array> "Dependencies"
/// <summary>
/// The raw ts package id object.
/// </summary>
let private tsPackageIdKey = SymbolTypeKey.create<Ts.PackageId voption> "PackageId"
/// <summary>
/// Tracks tags that are dependent on this tag.
/// </summary>
let private dependentKeys = SymbolTypeKey.create<HashSet<SourceTag>> "Dependents"
/// <summary>
/// The submodule name.
/// </summary>
let private subModuleNameKey = SymbolTypeKey.create<string voption> "SubModuleName"
/// <summary>
/// Package info
/// </summary>
let private packageInfoKey = SymbolTypeKey.create<PackageInfo voption> "PackageInfo"
/// <summary>
/// The package id.
/// </summary>
let private packageIdKey = SymbolTypeKey.create<PackageId voption> "PackageId"

module PackageExportKind =
    let rec readValue: obj -> _ = function
        | :? string as value -> PackageExportKind.String value
        | value ->
            readConditionalValues value
            |> PackageExportKind.Conditional
    and private readConditionalValues values =
        Fable.Core.JS.Constructors.Object.entries values |> _.AsArray
        |> Array.map (fun (key, value) -> createConditionalValue key value)
        |> Array.toList
    and private createConditionalValue (key: string) value =
        match key with
        | "types" -> Types(readValue value)
        | "default" -> Default(readValue value)
        | "browser" -> Browser(readValue value)
        | "development" -> Development(readValue value)
        | "production" -> Production(readValue value)
        | "node-addons" -> NodeAddons(readValue value)
        | "node" -> Node(readValue value)
        | "import" -> Import(readValue value)
        | "require" -> Require(readValue value)
        | "module-sync" -> ModuleSync(readValue value)
        | _ -> Unknown(key, readValue value)
module PackageExport =
    let private tryFromExports (jsonFields: PackageJsonPathFields) =
        match jsonFields.exports with
        | Some (:? string as export) ->
            PackageExport.Abbrev(".", PackageExportKind.String export)
            |> Some
        | Some exports ->
            Fable.Core.JS.Constructors.Object.entries exports |> _.AsArray
            |> Array.map (fun (key, value) -> key, PackageExportKind.readValue value)
            |> Array.toList
            |> PackageExport.Full
            |> Some
        | None -> None
    let inline private makeDummyAbbrev (value: string) = Abbrev(".", PackageExportKind.String(value))
    let private tryFromTypes (jsonFields: PackageJsonPathFields) =
        Option.map makeDummyAbbrev jsonFields.types
    let private tryFromTypings (jsonFields: PackageJsonPathFields) =
        Option.map makeDummyAbbrev jsonFields.typings
    let private tryFromMain (jsonFields: PackageJsonPathFields) =
        Option.map makeDummyAbbrev jsonFields.main
    let tryFromFields (jsonFields: PackageJsonPathFields) =
        tryFromExports jsonFields
        |> Option.orElse (tryFromTypes jsonFields)
        |> Option.orElse (tryFromTypings jsonFields)
        |> Option.orElse (tryFromMain jsonFields)
        

module private SourceGuard =
    let inline apply  (guard: SourceGuard) (fn: SourceGuard -> unit)= fn guard; guard
    let inline seen (node: Ts.SourceFile) = GuardTracer.has node
    let inline fromNode (node: Ts.SourceFile) = GuardTracer.fromNode node :?> SourceGuard
    let inline setSource (node: Ts.SourceFile) (guard: SourceGuard) = guard[nameof guard.Source] <- node
    let inline setPackageDirectory (packageDirectory: string) (guard: SourceGuard) = guard[nameof guard.PackageDirectory] <- packageDirectory
    let inline setPackageJsonContent (packageJsonContent: PackageJsonPathFields) (guard: SourceGuard) = guard[nameof guard.PackageJsonContent] <- packageJsonContent
    let inline withSource node guard = setSource node |> apply guard
    let inline withPackageDirectory packageDirectory guard = setPackageDirectory packageDirectory |> apply guard
    let inline withPackageJsonContent packageJsonContent guard = setPackageJsonContent packageJsonContent |> apply guard
    
    let inline getExports (guard: SourceGuard) =
        SymbolTypeKey.accessOrInit exportKey (fun () ->
            guard.PackageJsonContent
            |> ValueOption.bind (
                PackageExport.tryFromFields
                >> Option.toValueOption
                )
            ) guard
    let inline tryGetPackageNameFromPackageDirectory (guard: SourceGuard) =
        guard.PackageDirectory
        |> ValueOption.bind (fun packageDirectory ->
            if packageDirectory.Contains("node_modules", System.StringComparison.OrdinalIgnoreCase) then
                let pathParts = packageDirectory.Split(Node.Api.path.sep, System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)
                pathParts
                |> Array.tryFindIndex _.Equals("node_modules", System.StringComparison.OrdinalIgnoreCase)
                |> Option.bind (
                    (+) 1
                    >> Array.skip
                    >> funApply pathParts
                    >> function
                        | [||] -> None
                        | parts -> Some(String.concat Node.Api.path.sep parts)
                )
                |> Option.toValueOption
            else ValueNone
            )
    let inline tryGetPackageNameFromPackageJsonContent (guard: SourceGuard) =
        guard.PackageJsonContent
        |> ValueOption.bind (_.name >> Option.toValueOption)
    let inline tryGetPackageName (guard: SourceGuard) =
        tryGetPackageNameFromPackageJsonContent guard
        |> ValueOption.orElseWith (fun () -> tryGetPackageNameFromPackageDirectory guard)
    let inline tryGetPackageVersion (guard: SourceGuard) =
        guard.PackageJsonContent
        |> ValueOption.bind (_.version >> Option.toValueOption)

module private SourceTag =
    let inline apply (tag: SourceTag) (fn: SourceTag -> unit) = fn tag; tag
    let inline seen node = Tracer.has<SourceKind> node
    let inline setProgram (program: Ts.Program) (tag: SourceTag) =
        SymbolTypeKey.set programKey program tag
    let inline withProgram program tag = setProgram program |> apply tag
    let inline initTracer (sourceKind: SourceKind) (target: ^T) =
        // Set the value on target to sourceKind
        Tracer.set<SourceKind> sourceKind target
        // Skip conditional value presence
        let result = Tracer.unsafeGet<SourceKind> target
        // trigger type safety mechanism
        result.Imprint
        // cast to the correct type
        result :?> GuardedTracer<SourceKind, SourceGuard>
    let inline getOrInit program guard sourceKind target =
        let seen = seen target
        let tracer =
            Tracer.get<SourceKind> target
            |> ValueOption.defaultWith (fun () ->
                initTracer sourceKind target
                :?> SourceTag |> withProgram program
                :> Tracer<_>
                )
            :?> GuardedTracer<SourceKind, SourceGuard>
        if GuardedTracer.hasGuard tracer |> not then tracer.Guard <- guard
        seen,
        tracer :?> SourceTag
    
    let inline getDependents (tag: SourceTag) =
        SymbolTypeKey.access dependentKeys tag |> ValueOption.defaultValue(HashSet())
        :> IReadOnlySet<SourceTag>
    let inline iterDependents (fn: HashSet<SourceTag> -> unit) (tag: SourceTag) =
        SymbolTypeKey.accessOrInit dependentKeys (fun () -> HashSet()) tag |> fn
        tag
    let inline getDependencies (tag: SourceTag) =
        SymbolTypeKey.access dependencyKeys tag |> ValueOption.defaultValue [||]
    let inline iterDependencies (fn: ResizeArray<SourceTag> -> unit) (tag: SourceTag) =
        SymbolTypeKey.accessOrInit dependencyKeys (fun () -> ResizeArray().AsArray) tag |> unbox<ResizeArray<SourceTag>> |> fn
        tag
    let inline getTsPackageId (tag: SourceTag) =
        SymbolTypeKey.access tsPackageIdKey tag |> ValueOption.flatten
    let inline tryGetPackageName (tag: SourceTag) =
        getTsPackageId tag
        |> ValueOption.map _.name
        |> ValueOption.orElseWith (fun () -> SourceGuard.tryGetPackageName tag.Guard)
    let inline tryGetPackageVersion (tag: SourceTag) =
        getTsPackageId tag
        |> ValueOption.map _.version
        |> ValueOption.orElseWith (fun () -> SourceGuard.tryGetPackageVersion tag.Guard)
    let inline tryGetSubModuleName (tag: SourceTag) =
        SymbolTypeKey.access subModuleNameKey tag |> ValueOption.flatten
    let inline tryGetPackageInfo (tag: SourceTag) =
        SymbolTypeKey.access packageInfoKey tag |> ValueOption.flatten
    let inline setPackageInfo (packageInfo: PackageInfo) (tag: SourceTag) =
        SymbolTypeKey.set packageInfoKey packageInfo tag
    let inline tryGetPackageId (tag: SourceTag) =
        SymbolTypeKey.access packageIdKey tag |> ValueOption.flatten
    let inline setPackageId packageId (tag: SourceTag) =
        SymbolTypeKey.set packageIdKey (ValueSome packageId) tag
    
        
        
        
        

/// <summary>
/// Creating the guard is less expensive than the tag. We do the boiler plate here.
/// </summary>
type SourceGuardSRTPCreator =
    static member Seen(node: Ts.SourceFile) = GuardTracer.has node
    static member Create(node: Ts.SourceFile) =
        let guard =
            SourceGuard.fromNode node
            |> SourceGuard.withSource node
        match node.packageJsonScope with
        | Some packageJsonScope ->
            guard
            |> SourceGuard.withPackageDirectory packageJsonScope.packageDirectory
            |> SourceGuard.withPackageJsonContent packageJsonScope.contents.packageJsonContent
        | None -> guard

type SourceTag with
    static member Create(program: Ts.Program, sourceFile: Ts.SourceFile) =
        let createTracerState seen sourceTag = if seen then TagState.createVisited sourceTag else TagState.createUnvisited sourceTag
        let seenGuard = SourceGuardSRTPCreator.Seen sourceFile
        let guard = SourceGuardSRTPCreator.Create(sourceFile)
        let guardState = if seenGuard then TagState.createVisited guard else TagState.createUnvisited guard
        
        let createResultStateTuple seen sourceTag =
            createTracerState seen sourceTag,
            guardState
            
        let checker = program.getTypeChecker()
        
        if program.isSourceFileDefaultLibrary sourceFile then
            SourceTag.getOrInit program guard SourceKind.LibEs sourceFile
            ||> createResultStateTuple
        else
            checker.getSymbolAtLocation sourceFile
            |> Option.defaultWith (fun () ->
                Log.error "Invariant: source file that is not a default library should have a symbol"
                Log.traceTo 2 sourceFile
                failwith "Invariant: source file that is not a default library should have a symbol"
                )
            |> fun symbol -> SourceKind.Package symbol, symbol
            ||> SourceTag.getOrInit program guard
            ||> createResultStateTuple
    static member inline CreateValue(program,sourceFile) =
        SourceTag.Create(program, sourceFile)
        |> fst |> _.Value



type SourceGuard with
    /// <summary>
    /// If they exist, and they are conditional exports/entry points, then the values are provided
    /// in the order they are defined.
    /// </summary>
    member this.Exports: PackageExport voption = SourceGuard.getExports this

type SourceTag with
    member inline this.Exports = this.Guard.Exports
    member this.TsPackageId = SourceTag.getTsPackageId this
    member this.Dependents = SourceTag.getDependents this
    member this.Dependencies = SourceTag.getDependencies this
    member this.PackageName = SourceTag.tryGetPackageName this
    member this.PackageVersion = SourceTag.tryGetPackageVersion this
    member this.SubModuleName = SourceTag.tryGetSubModuleName this
    member this.PackageInfo = SourceTag.tryGetPackageInfo this


type PackageId with
    static member inline Create(packageName: string, packageVersion: string) =
        PackageId(packageName, packageVersion)
    static member inline Create(packageId: Ts.PackageId) =
        PackageId.Create(packageId.name, packageId.version)

let private packageCache = SymbolTypeKey.create<Dictionary<PackageId, PackageInfo>> "PackageCache"

type Ts.Program with
    member this.SeedResolvedModules() =
        this.forEachResolvedModule(fun resolvedModule moduleName _ sourceFilePath ->
            let sourceFile = this.getSourceFile(sourceFilePath)
            // if we can't find the source file for the source file path then we early exit.
            match sourceFile with
            | None -> ()
            | Some sourceFile -> 
            let sourceFileTag = SourceTag.CreateValue(this, sourceFile)
            match resolvedModule.resolvedModule with
            | Some resolvedModule ->
                let resolvedSourceFile =
                    this.getSourceFile(resolvedModule.resolvedFileName)
                    |> Option.orElseWith(fun () ->
                        let removeExtension (path: string) =
                            let extension = Node.Api.path.extname path
                            let extensionless = resolvedModule.resolvedFileName.Substring(0, path.Length - extension.Length)
                            extensionless
                        let extensionless = resolvedModule.resolvedFileName.Substring(0, resolvedModule.resolvedFileName.Length - resolvedModule.extension.Length)
                        this.getSourceFiles().AsArray
                        |> Array.tryFind (_.fileName >> removeExtension >> (=) extensionless)
                        |> Option.orElseWith(fun () ->
                            this.getSourceFiles().AsArray
                            |> Array.tryFind (_.fileName >> removeExtension >> removeExtension >> (=) extensionless)
                            )
                        )
                match resolvedSourceFile with
                | None -> Log.error $"Invariant: Unable to resolve source file for %A{resolvedModule} {moduleName}"
                | Some resolvedSourceFile ->
                let resolvedSourceTag = SourceTag.CreateValue(this, resolvedSourceFile)
                
                resolvedModule.packageId
                |> Option.iter (SourceTag.setPackageId >> funApply resolvedSourceTag)
                
                resolvedSourceTag
                |> SourceTag.iterDependents (_.Add(sourceFileTag) >> ignore)
                |> ignore
                
                sourceFileTag
                |> SourceTag.iterDependencies _.Add(resolvedSourceTag)
                |> ignore
                
                let cache = SymbolTypeKey.accessOrInit packageCache (fun () -> Dictionary()) this
                // adds the current iterated source tag to the package cache
                // using the key created from the given package name and package version.
                let addSourceTagToPackage packageName packageVersion =
                    let packageKey = PackageId.Create(packageName, packageVersion)
                    match cache.TryGetValue(packageKey) with
                    | true, packageInfo ->
                        if packageInfo.AssociatedTags |> Array.contains resolvedSourceTag then
                            ()
                        else
                            packageInfo.AssociatedTags
                            |> unbox<ResizeArray<SourceTag>>
                            |> _.Add(resolvedSourceTag)
                        SymbolTypeKey.set packageInfoKey packageInfo resolvedSourceTag
                    | _ ->
                        let packageInfo = { Name = packageName; Version = packageVersion; AssociatedTags = [| resolvedSourceTag |] }
                        cache[ packageKey ] <- packageInfo
                        SymbolTypeKey.set packageInfoKey packageInfo resolvedSourceTag
                // If the module comes with a package id, then we use that. Otherwise
                // we try to use the package name and version from the source file tag which
                // has a couple different fallbacks.
                match resolvedModule.packageId with
                | Some packageId ->
                    // if we have the package id, we use the given submodule name
                    SymbolTypeKey.accessOrInit subModuleNameKey (fun () ->
                        if System.String.IsNullOrWhiteSpace(packageId.subModuleName)
                        then ValueNone
                        else ValueSome packageId.subModuleName) resolvedSourceTag
                    |> ignore
                    addSourceTagToPackage packageId.name packageId.version
                | None when resolvedSourceTag.Value.IsPackage && resolvedSourceTag.PackageName.IsSome && resolvedSourceTag.PackageVersion.IsSome ->
                    // if we don't have the package id, we use the moduleName from the callback
                    SymbolTypeKey.accessOrInit subModuleNameKey (fun () ->
                        if System.String.IsNullOrWhiteSpace(moduleName)
                        then ValueNone
                        else ValueSome moduleName
                        ) resolvedSourceTag
                    |> ignore
                    let packageName = resolvedSourceTag.PackageName.Value
                    let packageVersion = resolvedSourceTag.PackageVersion.Value
                    addSourceTagToPackage packageName packageVersion
                // Failing that, noop
                | _ -> ()
            | _ -> ()
            )

type CompilePackageCacheError =
    | NoPackageCache

let private subModuleTagKey = SymbolTypeKey.create<SubModule voption> "SubModule"
let private subModuleTagIdKey = SymbolTypeKey.create<SubModuleId voption> "SubModuleId"
let private subModuleCache = SymbolTypeKey.create<Dictionary<SubModuleId, SourceTag>> "SubModuleCache"

type PackageCache = {
    PackageInfoCache: Dictionary<PackageId, PackageInfo>
    SubModuleCache: Dictionary<SubModuleId, SourceTag>
    Packages: Package array
    SubModuleRelations: SubModuleRelation array
    SubModules: SubModule array
}

let private compiledPackageCache = SymbolTypeKey.create<Result<PackageCache, CompilePackageCacheError>> "CompiledPackageCache"

type Ts.Program with
    member this.CompilePackageCache() =
        let relationCache = HashSet()
        let subModuleCollector = ResizeArray<SubModule>()
        let packageCache =
            match SymbolTypeKey.access packageCache this with
            | ValueSome cache -> Ok cache
            | ValueNone -> Result.Error CompilePackageCacheError.NoPackageCache
        let prepareCache (cache: Dictionary<PackageId, PackageInfo>) =
            cache
            |> Seq.toArray
            |> Array.map (fun kv ->
                let info = kv.Value
                let key = kv.Key
                let subModules =
                    info.AssociatedTags
                    |> Array.map (fun tag ->
                        let subModuleName =
                            tag.SubModuleName
                            |> ValueOption.defaultValue ""
                        let subModule =
                            {
                                Package = key
                                Name = subModuleName
                                Path = tag.Guard.Source.fileName
                            }
                        let subModuleKey = subModule.Key
                        subModuleCollector.Add(subModule)
                        tag
                        |> SymbolTypeKey.set subModuleTagKey (ValueSome subModule)
                        tag
                        |> SymbolTypeKey.set subModuleTagIdKey (ValueSome subModuleKey)
                        this
                        |> SymbolTypeKey.accessOrInit subModuleCache (fun () -> Dictionary())
                        |> _.Add(subModuleKey, tag)
                        subModuleKey)
                // now that we have the subModules collected, we will collect relationships
                let relations =
                    info.AssociatedTags
                    |> Array.collect (fun tag ->
                        SymbolTypeKey.access subModuleTagIdKey tag
                        |> ValueOption.flatten
                        |> ValueOption.map (fun subModuleId ->
                            // we only care for dependency mappings within the same package.
                            tag.Dependencies
                            |> Seq.choose (
                                SymbolTypeKey.access subModuleTagIdKey
                                >> ValueOption.flatten
                                >> ValueOption.bind (fun dependencyId ->
                                    let value = {
                                        Dependent = subModuleId
                                        Dependency = dependencyId
                                    }
                                    relationCache.Add value |> ignore
                                    match dependencyId with
                                    | SubModuleId(packageId, _) when packageId = key -> ValueSome value
                                    | _ -> ValueNone)
                                >> ValueOption.toOption
                                )
                            |> Seq.toArray
                            )
                        |> ValueOption.defaultValue [||]
                        )
                let assumedEntries =
                    subModules
                    |> Array.filter (fun subModule ->
                        relations
                        |> Array.exists (_.Dependency >> (=) subModule)
                        |> not)
                {
                    Name = info.Name
                    Version = info.Version
                    SubModules = subModules
                    Entry = assumedEntries
                })
        packageCache
        |> Result.map (fun cache -> {
                PackageInfoCache = cache
                Packages = prepareCache cache
                SubModuleCache = SymbolTypeKey.unsafeAccess subModuleCache this
                SubModuleRelations = relationCache |> Seq.toArray
                SubModules = subModuleCollector.AsArray
            })
    member this.PackageCache =
        SymbolTypeKey.accessOrInit compiledPackageCache this.CompilePackageCache this

type SourceTag with
    member this.SubModule =
        SymbolTypeKey.access subModuleTagKey this
        |> ValueOption.flatten
let private exportPointCanonicalKey = SymbolTypeKey.create<ExportPoint> "ExportPointCanonical"
let private exportPointsKey = SymbolTypeKey.create<HashSet<ExportPoint>> "ExportPoints"
let private exportPointKey = SymbolTypeKey.create<ExportPoint> "ExportPoint"
type Ts.Program with
    member this.SeedExportPoints() =
        let checker = this.getTypeChecker()
        let sourceFiles =
            this.getSourceFiles().AsArray
            |> Array.choose (fun sf ->
                checker.getSymbolAtLocation sf
                |> Option.map (fun symbol -> SourceTag.CreateValue(this, sf), symbol)
                )
        for sourceFile, symbol in sourceFiles do
            let subModule = sourceFile.SubModule
            if subModule.IsNone then () else
            let subModule = subModule.Value
            let exports =
                symbol.exports
                |> unbox<Fable.Core.JS.Map<string, Ts.Symbol> option>
                |> Option.defaultValue (Fable.Core.JS.Constructors.Map.Create [])
            exports.forEach(fun exportSymbol exportName _ ->
                let exportPoint = {
                    Name = exportName
                    SubModule = subModule.Key
                }
                SymbolTypeKey.set exportPointKey exportPoint exportSymbol
                if exportSymbol.flags.HasFlag(Ts.SymbolFlags.Alias) then
                    checker.getAliasedSymbol exportSymbol
                else exportSymbol
                |> SymbolTypeKey.accessOrInit exportPointsKey (fun () -> HashSet())
                |> _.Add(exportPoint)
                |> ignore
                )

type Ts.Program with
    member this.GetExportCollection(symbol: Ts.Symbol) =
        let checker = this.getTypeChecker()
        if symbol.flags.HasFlag(Ts.SymbolFlags.Alias) then
            checker.getAliasedSymbol symbol
        else symbol
        |> fun symbol ->
            let exportPoints =
                SymbolTypeKey.access exportPointsKey symbol
                |> ValueOption.map Seq.toArray
            match SymbolTypeKey.access exportPointKey symbol with
            | ValueSome exportPoint when exportPoints.IsSome ->
                {
                    Canonical = exportPoint
                    Aliases = exportPoints.Value
                }
                |> ValueSome
            | ValueNone when exportPoints.IsSome ->
                {
                    Canonical = exportPoints.Value |> Array.head
                    Aliases = exportPoints.Value
                }
                |> ValueSome
            | _ -> ValueNone
            
                

