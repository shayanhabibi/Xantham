/// <summary>
/// Lazy resolved type graph for use in generators.
/// </summary>
///
/// <remarks>
/// <para>
/// The wire format produced by the decoder (<c>DecodedResult</c>) represents the TypeScript type graph
/// as a pair of flat maps keyed by <c>TypeKey</c>: one for structural types (<c>TsType</c>) and one for
/// export declarations (<c>TsExportDeclaration</c>). References between types are expressed as
/// <c>TypeKey</c> values that must be resolved against those maps. While correct, this representation
/// is inconvenient for generators: maps must be threaded through every rendering function, and every
/// type dereference requires an explicit lookup.
/// </para>
/// <para>
/// <c>ArenaInterner</c> pre-resolves this key graph into a lazy object graph. Each <c>TypeKey</c>
/// reference becomes a <c>Lazy&lt;ResolvedType&gt;</c> — following a reference is simply forcing a
/// lazy value, with no map access needed at call sites. Results are memoised in a dictionary keyed by
/// <c>TypeKey</c>, so types that share the same key resolve to the same object instance. Combined with
/// <c>[&lt;ReferenceEquality&gt;]</c> on all record types, this enables identity-based sharing
/// detection in generators (e.g. recognising that two members reference the same <c>TypeParameter</c>)
/// without carrying keys around.
/// </para>
/// <para>
/// Cycles in the type graph (e.g. recursive interfaces, self-referential type aliases) are broken
/// naturally by the lazy boundaries around all <c>TypeKey</c> dereferences. Construction of a node
/// never forces any of its outgoing lazy references, so re-entrant calls to <c>resolve</c> cannot
/// occur during graph initialisation. By the time a consumer forces a lazy, the referent is already
/// in the cache.
/// </para>
///
/// <b>Prefer this representation in generators over raw <c>DecodedResult</c> maps when:</b>
/// <list type="bullet">
///   <item>Rendering functions should not need access to global maps.</item>
///   <item>Structural sharing or identity equality between types is meaningful to the output.</item>
///   <item>The generator traverses the type graph recursively (pattern matching on <c>ResolvedType</c>
///         is more ergonomic than repeated map lookups).</item>
/// </list>
///
/// <b>Prefer raw <c>DecodedResult</c> maps when:</b>
/// <list type="bullet">
///   <item>Only a small subset of the type graph is needed — the interner allocates shells for all
///         top-level exports at construction time.</item>
///   <item><c>TypeKey</c> identity must be preserved for output naming or cross-referencing (keys are
///         not carried in the resolved graph).</item>
///   <item>Startup cost is a concern — <c>ArenaInterner.create</c> walks and shells the entire export
///         map eagerly, deferring only nested type resolution.</item>
/// </list>
/// </remarks>
module Xantham.Decoder.ArenaInterner

open System.Collections.Concurrent
open System.Collections.Frozen
open System.Collections.Generic
open FSharp.Control
open Xantham
open Xantham.Decoder
open Xantham.Decoder.Types.Graph

#nowarn 44

/// <summary>
/// Bit flags describing structural anomalies in a single segment of a fully
/// qualified TypeScript name (e.g. embedded quotation marks, slashes, or periods).
/// </summary>
/// <category index="4">Resolved Type Graph</category>
[<System.Flags>]
type QualifiedNamePartDiagnostic =
    | ContainsQuotationMarks = (1 <<< 0)
    | ContainsSlash = (1 <<< 1)
    | ContainsPeriod = (1 <<< 2)

/// <summary>
/// A single segment of a fully qualified TypeScript name. Tagged either as
/// <c>Normal</c> (no anomalies) or <c>Abnormal</c> together with diagnostic flags
/// indicating which anomalies the segment contains.
/// </summary>
/// <category index="4">Resolved Type Graph</category>
[<Struct>]
type QualifiedNamePart =
    | Abnormal of part: string * diagnostic: QualifiedNamePartDiagnostic
    | Normal of part: string
    member this.Value =
        match this with
        | Abnormal(part, _)
        | Normal(part) -> part

/// <summary>
/// The resolved (lazy-graph) form of a structural TypeScript type. Each case
/// directly references its dependencies (rather than carrying <c>TypeKey</c>s),
/// with cycles broken by the surrounding <see cref="T:LazyResolvedType"/> wrappers.
/// </summary>
/// <category index="4">Resolved Type Graph</category>
type [<RequireQualifiedAccess>] ResolvedType =
    | GlobalThis
    | Conditional of ConditionalType
    | Interface of Interface
    | Class of Class
    | Primitive of TypeKindPrimitive
    | Union of Union
    | Intersection of Intersection
    | Literal of TsLiteral
    | IndexedAccess of IndexAccessType
    | Index of Index
    | TypeReference of TypeReference
    | Array of ResolvedType
    | Enum of EnumType
    | EnumCase of EnumCase
    | TypeParameter of TypeParameter
    | ReadOnly of ResolvedType
    | Tuple of Tuple
    | Predicate of Predicate
    | TypeLiteral of TypeLiteral
    | TemplateLiteral of TemplateLiteral
    | Optional of TypeReference
    | Substitution of SubstitutionType
    | TypeQuery of TypeQuery

/// <summary>
/// The resolved (lazy-graph) form of a top-level TypeScript export declaration.
/// Holds direct references into the resolved type graph rather than <c>TypeKey</c>s.
/// </summary>
/// <category index="4">Resolved Type Graph</category>
and [<RequireQualifiedAccess>] ResolvedExport =
    | Variable of Variable
    | Interface of Interface
    | TypeAlias of TypeAlias
    | Class of Class
    | Enum of EnumType
    | Function of Function list
    | Module of Module

/// <summary>
/// Pairing of raw input data (<typeparamref name="'RawData"/>, typically a <c>TypeKey</c>)
/// with a lazily computed resolved value (<typeparamref name="'LazyResult"/>).
/// Used to defer resolution of cyclic dependencies in the type graph.
/// </summary>
/// <category index="4">Resolved Type Graph</category>
and LazyContainer<'RawData, 'LazyResult> = {
    /// The raw input value (typically a <c>TypeKey</c>) that produced the lazy result.
    Data: 'RawData
    /// The lazy resolution of the raw data into a richer object graph value.
    Result: Lazy<'LazyResult>
} with
    static member inline DummyTypeKey = TypeKindPrimitive.Unknown.TypeKey
    static member CreateLazyTypeKeyDummy<'LazyResult> (value: Lazy<'LazyResult>): LazyContainer<TypeKey, 'LazyResult> = { Data = LazyContainer<_,_>.DummyTypeKey; Result = value }
    static member CreateTypeKeyDummy<'LazyResult> (value: 'LazyResult): LazyContainer<TypeKey, 'LazyResult> = { Data = LazyContainer<_,_>.DummyTypeKey; Result = Lazy.CreateFromValue value }
    static member inline CreateFromValue<'RawData, 'LazyResult> (value: 'LazyResult): LazyContainer<'RawData, 'LazyResult> = { Data = Unchecked.defaultof<_>; Result = Lazy.CreateFromValue value }
    /// <summary>Force and return the lazy result.</summary>
    member inline this.Value with get() = this.Result.Value
    member inline this.IsValueCreated with get() = this.Result.IsValueCreated
    member inline this.Raw with get() = this.Data
and LazyResolvedType = LazyContainer<TypeKey, ResolvedType>
and LazyResolvedExport = LazyContainer<TypeKey, ResolvedExport>
and LazyPackage = LazyContainer<PackageId, ResolvedPackage>
and LazySubModule = LazyContainer<SubModuleId, ResolvedSubModule>
and LazyExportPoint = LazyContainer<Xantham.ExportPoint, ResolvedExportPoint>

and [<ReferenceEquality>] ResolvedPackage = {
    Name: string
    Version: string
    Json: Export voption
    SubModules: LazySubModule list
    Entry: LazySubModule list
}

and [<ReferenceEquality>] ResolvedSubModule = {
    Name: string
    Path: string
    Package: LazyPackage
    Dependees: LazySubModule list
    Dependencies: LazySubModule list
    Exports: LazyResolvedExport list
}

and [<ReferenceEquality>] ResolvedExportPoint = {
    Name: string
    SubModule: LazySubModule
}

and ResolvedExportCollection = {
    Canonical: ResolvedExportPoint
    Aliases: ResolvedExportPoint list
}

and Source =
    | LibEs of fileName: string
    | PackageInternal of LazySubModule
    | Package of ResolvedExportCollection
    | UnknownDeclared of fileName: string

and [<ReferenceEquality>] Module = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    IsNamespace: bool
    IsRecursive: bool
    Exports: ResolvedExport list
}

and [<ReferenceEquality>] TypeQuery = {
    FullyQualifiedName: QualifiedNamePart list
    Type: LazyResolvedType
}

and [<ReferenceEquality>] Index = {
    Type: LazyResolvedType
}

and [<ReferenceEquality>] TupleElementType = {
    Type: LazyResolvedType
    IsOptional: bool
    IsRest: bool
}

and [<ReferenceEquality>] TupleElement =
    | Variadic of LazyResolvedType
    | FixedLabel of string * TupleElementType
    | Fixed of TupleElementType
    member this.Label =
        match this with
        | FixedLabel(label, _) -> ValueSome label
        | _ -> ValueNone
    member this.IsOptional =
        match this with
        | Fixed { IsOptional = value }
        | FixedLabel(_, { IsOptional = value }) -> value
        | _ -> false
    member this.IsRest =
        match this with
        | Fixed { IsRest = value }
        | FixedLabel(_, { IsRest = value }) -> value
        | Variadic _ -> true
    member inline this.Name = this.Label
    member this.Type =
        match this with
        | Variadic value
        | Fixed { Type = value }
        | FixedLabel(_, { Type = value }) -> value
    
and [<ReferenceEquality>] Tuple = {
    IsReadOnly: bool
    FixedLength: int
    MinRequired: int
    Types: TupleElement list
}

and [<ReferenceEquality>] Union = {
    Types: LazyResolvedType list
}
and [<ReferenceEquality>] Intersection = {
    Types: LazyResolvedType list
}

and [<ReferenceEquality>] TemplateLiteral = {
    Text: string list
    Types: LazyResolvedType list
}

and [<ReferenceEquality>] Predicate = {
    Type: LazyResolvedType
    ParameterName: Name<Case.camel>
    IsAssertion: bool
}

and [<ReferenceEquality>] EnumType = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    Members: Lazy<EnumCase> list
    Documentation: TsComment list
}
and [<ReferenceEquality>] EnumCase = {
    Parent: Lazy<EnumType>
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    Value: TsLiteral
    Documentation: TsComment list
}
and [<ReferenceEquality>] Variable = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.camel>
    Type: LazyResolvedType
    Documentation: TsComment list
}
and [<ReferenceEquality>] Parameter = {
    Name: Name<Case.camel>
    IsOptional: bool
    IsSpread: bool
    Type: LazyResolvedType
    Documentation: TsComment list
}
and [<ReferenceEquality>] TypeParameter = {
    Name: Name<Case.typar>
    Constraint: LazyResolvedType option
    Default: LazyResolvedType option
    Documentation: TsComment list
}
and [<ReferenceEquality>] Method = {
    Name: Name<Case.camel>
    Parameters: Parameter list
    Type: LazyResolvedType
    TypeParameters: Lazy<TypeParameter> list
    Documentation: TsComment list
    IsOptional: bool
    IsStatic: bool
} with interface IOverloadable

and [<ReferenceEquality>] CallSignature = {
    Documentation: TsComment list
    Parameters: Parameter list
    Type: LazyResolvedType
    TypeParameters: Lazy<TypeParameter> list
} with interface IOverloadable

and [<ReferenceEquality>] ConstructSignature = {
    Type: LazyResolvedType
    Parameters: Parameter list
    TypeParameters: Lazy<TypeParameter> list
} with interface IOverloadable

and [<ReferenceEquality>] Constructor = {
    Documentation: TsComment list
    Parameters: Parameter list
} with interface IOverloadable

and [<ReferenceEquality>] Property = {
    Name: Name<Case.camel>
    Type: LazyResolvedType
    Documentation: TsComment list
    IsOptional: bool
    IsStatic: bool
    IsPrivate: bool
    Accessor: TsAccessor
}

and [<ReferenceEquality>] GetAccessor = {
    Name: Name<Case.camel>
    Type: LazyResolvedType
    IsStatic: bool
    IsPrivate: bool
}

and [<ReferenceEquality>] SetAccessor = {
    Name: Name<Case.camel>
    ArgumentType: LazyResolvedType
    IsStatic: bool
    IsPrivate: bool
    Documentation: TsComment list
}
and [<ReferenceEquality>] IndexSignature = {
    Parameters: Parameter list
    Type: LazyResolvedType
    IsReadOnly: bool
}
and [<ReferenceEquality>] Function = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Documentation: TsComment list
    IsDeclared: bool
    Name: Name<Case.camel>
    Type: LazyResolvedType
    Parameters: Parameter list
    TypeParameters: Lazy<TypeParameter> list
    SignatureKey: Lazy<TypeLiteral>
} with interface IOverloadable

and [<ReferenceEquality>] ConditionalType = {
    Check: LazyResolvedType
    Extends: LazyResolvedType
    True: LazyResolvedType
    False: LazyResolvedType
}

and [<RequireQualifiedAccess>] Member =
    | Method of Method list
    | Property of Property
    | GetAccessor of GetAccessor
    | SetAccessor of SetAccessor
    | CallSignature of CallSignature list
    | IndexSignature of IndexSignature
    | ConstructSignature of ConstructSignature list

and [<ReferenceEquality>] TypeReference = {
    Type: LazyResolvedType
    TypeArguments: LazyResolvedType list
    ResolvedType: LazyResolvedType option
}
and [<ReferenceEquality>] InterfaceHeritage = {
    Extends: TypeReference list
}
and [<ReferenceEquality>] ClassHeritage = {
    Implements: TypeReference option
    Extends: TypeReference list
}
and [<ReferenceEquality>] Interface = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    Members: Member list
    TypeParameters: Lazy<TypeParameter> list
    Documentation: TsComment list
    Heritage: InterfaceHeritage
}
and [<ReferenceEquality>] Class = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    Members: Member list
    TypeParameters: Lazy<TypeParameter> list
    Constructors: Constructor list
    Heritage: ClassHeritage
}
and [<ReferenceEquality>] TypeLiteral = {
    Members: Member list
}

and [<ReferenceEquality>] IndexAccessType = {
    Object: LazyResolvedType
    Index: LazyResolvedType
}
and [<ReferenceEquality>] TypeAlias = {
    [<System.Obsolete "Match against the Source field instead.">]
    IsLibEs: bool
    Source: Source
    FullyQualifiedName: QualifiedNamePart list
    Name: Name<Case.pascal>
    Type: LazyResolvedType
    TypeParameters: Lazy<TypeParameter> list
    Documentation: TsComment list
}
and [<ReferenceEquality>] SubstitutionType = {
    Base: LazyResolvedType
    Constraint: LazyResolvedType
}

/// <summary>
/// Lazily-resolved object graph view of a <c>DecodedResult</c>. Following a
/// reference forces a <c>Lazy&lt;ResolvedType&gt;</c>, materialising a node on
/// demand. Cycles are broken by lazy boundaries (construction never forces
/// outgoing lazies). Exports are shelled eagerly; nested types are deferred.
/// </summary>
/// <category index="4">Resolved Type Graph</category>
type ArenaInterner = {
    /// Resolve a <c>TypeKey</c> to a structural <see cref="T:ResolvedType"/>.
    ResolveType: TypeKey -> ResolvedType
    /// Resolve a <c>TypeKey</c> to a <see cref="T:ResolvedExport"/> if it identifies an
    /// exported declaration; otherwise return the structural type as <c>Error</c>.
    ResolveExport: TypeKey -> Result<ResolvedExport, ResolvedType>
    ResolvePackage: PackageId -> ResolvedPackage
    ResolveSubModule: SubModuleId -> ResolvedSubModule
    ResolveExportPoint: Xantham.ExportPoint -> ResolvedExportPoint
    /// Cache of all resolved structural types keyed by their <c>TypeKey</c>.
    ResolvedTypes: IDictionary<TypeKey, ResolvedType>
    /// Cache of all resolved export declarations keyed by their <c>TypeKey</c>.
    ResolvedExports: IDictionary<TypeKey, ResolvedExport>
    ResolvedPackages: IDictionary<PackageId, ResolvedPackage>
    ResolvedSubModules: IDictionary<SubModuleId, ResolvedSubModule>
    ResolvedExportPoints: IDictionary<Xantham.ExportPoint, ResolvedExportPoint>
    /// Map from source module path to the list of resolved exports declared in that module.
    /// Note: Lists should be singletons where `Xantham.Source.IsPackage`.
    ExportMap: Map<Xantham.Source, LazyResolvedExport list>
    /// <summary>
    /// WARNING: Evaluation of the graph can be expensive.<br/>
    /// It is useful only when used in combination with the resolve type and resolve export
    /// dictionaries to handle dependencies correctly.
    /// </summary>
    Graph: Lazy<Graph>
} with
    override this.ToString() = $"ArenaInterner(%d{this.ExportMap.Count})"


module private QualifiedNamePart =
    let create (value: string) =
        let hasQuotations = value.Contains '"'
        let hasSlashes = value.Contains '/' || value.Contains '\\'
        let hasPeriod = value.Contains '.'
        [|
            hasQuotations, QualifiedNamePartDiagnostic.ContainsQuotationMarks
            hasSlashes, QualifiedNamePartDiagnostic.ContainsSlash
            hasPeriod, QualifiedNamePartDiagnostic.ContainsPeriod
        |]
        |> Array.fold (fun acc (has, diagnostic) ->
            if has then acc ||| diagnostic else acc) (enum 0)
        |> function
            | diagnostic when diagnostic = enum 0 -> QualifiedNamePart.Normal value
            | diagnostic -> QualifiedNamePart.Abnormal(value, diagnostic)

/// <summary>
/// Functions for constructing and walking an <see cref="T:ArenaInterner"/>.
/// </summary>
module ArenaInterner =
    let inline private lazyResolve key  value = {
        Data = key
        Result = value
    }
    let inline private lazyResolveWith map key =
        lazy map key
        |> lazyResolve key
    /// <summary>
    /// Build an <see cref="T:ArenaInterner"/> from a <c>DecodedResult</c>.
    /// Exports are shelled into their resolved form eagerly; structural types
    /// referenced from those exports are resolved lazily on first access.
    /// </summary>
    /// <param name="decodedResult">The decoded type/export maps to lift into the lazy graph.</param>
    let create (decodedResult: DecodedResult) =
        let typeMap = decodedResult.TypeMap
        let typeExportMap = decodedResult.ExportTypeMap
        let resolved = ConcurrentDictionary<TypeKey, ResolvedType>()
        let resolvedExports = ConcurrentDictionary<TypeKey, ResolvedExport>()
        let resolvedPackages = ConcurrentDictionary<PackageId, ResolvedPackage>()
        let resolvedSubModules = ConcurrentDictionary<SubModuleId, ResolvedSubModule>()
        let resolvedExportPoints = ConcurrentDictionary<Xantham.ExportPoint, ResolvedExportPoint>()
        let libEsSet = decodedResult.LibEsExports.ToFrozenSet()
        let isLibEs = libEsSet.Contains
        
        let rec resolve (typeKey: TypeKey): ResolvedType =
            match resolvedExports.TryGetValue(typeKey) with
            | true, ResolvedExport.Class cls -> ResolvedType.Class cls
            | true, ResolvedExport.Enum enum -> ResolvedType.Enum enum
            | true, ResolvedExport.Interface iface -> ResolvedType.Interface iface
            | _ ->
                match resolved.TryGetValue(typeKey) with
                | true, value -> value
                | _ ->
                    // Self-referential `TsType.TypeQuery` short-circuit. The encoder
                    // logs `[CIRCREF]` warnings and emits these anyway (observed
                    // ingesting worker-bundler: `TsTypeQuery { FullyQualifiedName =
                    // [MessagePort]; Type = 4587 }` at TypeKey 4587). Without this
                    // short-circuit the resolver builds a `ResolvedType.TypeQuery`
                    // whose inner `Type` lazy resolves to itself, and downstream
                    // prerender loops through it indefinitely. Collapse to
                    // `NonPrimitive` (renders to F# `obj`) — preserves compilation,
                    // matches the semantic loss the encoder already accepted, and
                    // breaks the cycle at its root rather than catching it at every
                    // consumer site.
                    let resolvedValue =
                        match typeMap[typeKey] with
                        | TsType.TypeQuery tq when tq.Type = typeKey ->
                            ResolvedType.Primitive TypeKindPrimitive.NonPrimitive
                        | t -> buildFrom (isLibEs typeKey) t
                    resolved[typeKey] <- resolvedValue
                    resolvedValue
        and resolveExport (typeKey: TypeKey) =
            match resolvedExports.TryGetValue(typeKey) with
            | true, value -> Ok value
            | _ when typeExportMap.ContainsKey(typeKey) ->
                // TS declaration merging: when a consumer package augments
                // a lib.dom interface (e.g. @cloudflare/workers-types
                // redeclaring `Request`), the encoder emits two views per
                // TypeKey — the export view at `typeExportMap[K]` carries
                // the merged interface (TS-resolved, often labeled
                // Source="typescript"), and `typeMap[K]` carries the
                // consumer's declaration alone. The merged export view
                // is the right body to emit; the trick is keeping its
                // body but freeing it from source-based ignore filters
                // that would skip pure-TS-toolchain declarations.
                //
                // Augmentation signals (either is sufficient):
                //   1. The two views have different Source values — the
                //      type view names a consumer source; use it.
                //   2. The two views have different member counts — the
                //      consumer redeclared (whatever Source label the
                //      encoder applied); strip Source so the filter sees
                //      nothing to ignore.
                let export = typeExportMap[typeKey]
                // TODO - is this still required?
                // let augmentedSource (eMembers: TsMember list) (tMembers: TsMember list)
                //                     (eSource: string option) (tSource: string option) =
                //     if eSource <> tSource && tSource.IsSome then tSource
                //     elif List.length eMembers <> List.length tMembers then None
                //     else eSource
                // let export =
                //     match export, typeMap.TryGetValue(typeKey) with
                //     | TsExportDeclaration.Interface eIface, (true, TsType.Interface tIface) ->
                //         let newSource = augmentedSource eIface.Members tIface.Members eIface.Source tIface.Source
                //         if newSource = eIface.Source then export
                //         else TsExportDeclaration.Interface { eIface with Source = newSource }
                //     | TsExportDeclaration.Class eCls, (true, TsType.Class tCls) ->
                //         let newSource = augmentedSource eCls.Members tCls.Members eCls.Source tCls.Source
                //         if newSource = eCls.Source then export
                //         else TsExportDeclaration.Class { eCls with Source = newSource }
                //     | _ -> export
                let resolvedValue = buildFromExport (isLibEs typeKey) export
                resolvedExports[typeKey] <- resolvedValue
                resolvedValue
                |> Ok
            | _ ->
                resolve typeKey
                |> Error
        and lazyResolveExport (typeKey: TypeKey) =
            if typeExportMap.ContainsKey(typeKey) then
                lazyResolveWith (resolveExport >> Result.toOption >> Option.get) typeKey
                |> Some
            else None
        and buildFromExport (isLibEs: bool) (export: TsExportDeclaration): ResolvedExport =
            let inline lazyResolve typ =
                {
                    Data = typ
                    Result = lazy (resolve typ)
                }
            match export with
            | TsExportDeclaration.Variable tsVariable -> ResolvedExport.Variable {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsVariable.Metadata
                    FullyQualifiedName = tsVariable.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Variable.Name = Name.Camel.create tsVariable.Name
                    Type = lazyResolve tsVariable.Type
                    Documentation = tsVariable.Documentation
                }
            | TsExportDeclaration.Interface tsInterface -> ResolvedExport.Interface {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsInterface.Metadata
                    FullyQualifiedName = tsInterface.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsInterface.Name
                    Members = tsInterface.Members |> List.map buildFromMember
                    TypeParameters =
                        tsInterface.TypeParameters
                        |> List.map buildFromTypeParameter
                    Documentation = tsInterface.Documentation
                    Heritage = { Extends = tsInterface.Heritage.Extends |> List.map buildFromTypeReference }
                }
            | TsExportDeclaration.Class tsClass -> ResolvedExport.Class {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsClass.Metadata
                    FullyQualifiedName = tsClass.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsClass.Name
                    Members = tsClass.Members |> List.map buildFromMember
                    TypeParameters = tsClass.TypeParameters |> List.map buildFromTypeParameter
                    Constructors = tsClass.Constructors |> List.map (fun value -> {
                        Constructor.Documentation = value.Documentation
                        Parameters = value.Parameters |> List.map buildFromParameter
                    })
                    Heritage = {
                        Implements = tsClass.Heritage.Implements |> Option.map buildFromTypeReference
                        Extends = tsClass.Heritage.Extends |> List.map buildFromTypeReference
                    }
                }
            | TsExportDeclaration.TypeAlias tsTypeAlias ->
                ResolvedExport.TypeAlias {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsTypeAlias.Metadata
                    FullyQualifiedName = tsTypeAlias.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsTypeAlias.Name
                    Type = tsTypeAlias.Type |> lazyResolve
                    TypeParameters =
                        tsTypeAlias.TypeParameters
                        |> List.map buildFromTypeParameter
                    Documentation = tsTypeAlias.Documentation
                }
            | TsExportDeclaration.Enum tsEnumType -> ResolvedExport.Enum {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsEnumType.Metadata
                    FullyQualifiedName = tsEnumType.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsEnumType.Name
                    Members = tsEnumType.Members |> List.map (fun value ->
                        lazy {
                            Parent =
                                lazy
                                    match resolve value.Parent with
                                    | ResolvedType.Enum enum -> enum
                                    | _ -> failwith "Inlining an enum returned a non enum type."
                            FullyQualifiedName = value.FullyQualifiedName |> List.map QualifiedNamePart.create
                            Name = Name.Pascal.create value.Name
                            Value = value.Value
                            Documentation = value.Documentation
                        })
                    Documentation = tsEnumType.Documentation
                }
            | TsExportDeclaration.Module tsModule ->
                ResolvedExport.Module {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsModule.Metadata
                    FullyQualifiedName = tsModule.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsModule.Name
                    IsNamespace = tsModule.IsNamespace
                    IsRecursive = tsModule.IsRecursive
                    Exports =
                        tsModule.Exports
                        |> List.map (buildFromExport isLibEs)
                }
            | TsExportDeclaration.Function tsOverloadableConstruct ->
                tsOverloadableConstruct.ToList()
                |> List.map (fun func ->
                    {
                        IsLibEs = isLibEs
                        Function.Source = buildSourceFromMetadata func.Metadata
                        FullyQualifiedName = func.FullyQualifiedName |> List.map QualifiedNamePart.create
                        Documentation = func.Documentation
                        IsDeclared = func.IsDeclared
                        Name = Name.Camel.create func.Name
                        Type = lazyResolve func.Type
                        Parameters =
                            func.Parameters
                            |> List.map buildFromParameter
                        TypeParameters =
                            func.TypeParameters
                            |> List.map buildFromTypeParameter
                        SignatureKey =
                            lazy
                            match resolve func.SignatureKey with
                            | ResolvedType.TypeLiteral typeLiteral -> typeLiteral
                            | _ -> failwith "Inlining a function returned a non type literal."
                    })
                |> ResolvedExport.Function
        and buildFrom (isLibEs: bool) (typ: TsType): ResolvedType =
            let inline lazyResolve typ =
                {
                    Data = typ
                    Result = lazy (resolve typ)
                }
                // lazy (resolve typ)
            let inline lazyResolveOption typ =
                typ |> Option.map lazyResolve
            match typ with
            | TsType.GlobalThis -> ResolvedType.GlobalThis
            | TsType.Conditional tsConditionalType -> ResolvedType.Conditional {
                    Check = lazyResolve tsConditionalType.Check
                    Extends = lazyResolve tsConditionalType.Extends
                    True = lazyResolve tsConditionalType.True
                    False = lazyResolve tsConditionalType.False
                }
            | TsType.Interface tsInterface -> ResolvedType.Interface {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsInterface.Metadata
                    FullyQualifiedName = tsInterface.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsInterface.Name
                    Members = tsInterface.Members |> List.map buildFromMember
                    TypeParameters =
                        tsInterface.TypeParameters
                        |> List.map buildFromTypeParameter
                    Documentation = tsInterface.Documentation
                    Heritage = { Extends = tsInterface.Heritage.Extends |> List.map buildFromTypeReference }
                }
            | TsType.Class tsClass -> ResolvedType.Class {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsClass.Metadata
                    FullyQualifiedName = tsClass.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsClass.Name
                    Members = tsClass.Members |> List.map buildFromMember
                    TypeParameters = tsClass.TypeParameters |> List.map buildFromTypeParameter
                    Constructors = tsClass.Constructors |> List.map (fun value -> {
                        Constructor.Documentation = value.Documentation
                        Parameters = value.Parameters |> List.map buildFromParameter
                    })
                    Heritage = {
                        Implements = tsClass.Heritage.Implements |> Option.map buildFromTypeReference
                        Extends = tsClass.Heritage.Extends |> List.map buildFromTypeReference
                    }
                }
            | TsType.Primitive typeKindPrimitive -> ResolvedType.Primitive typeKindPrimitive
            | TsType.Enum tsEnumType -> ResolvedType.Enum {
                    IsLibEs = isLibEs
                    Source = buildSourceFromMetadata tsEnumType.Metadata
                    FullyQualifiedName = tsEnumType.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsEnumType.Name
                    Members = tsEnumType.Members |> List.map (fun value ->
                        lazy {
                            Parent =
                                lazy
                                    match resolve value.Parent with
                                    | ResolvedType.Enum enum -> enum
                                    | _ -> failwith "Inlining an enum returned a non enum type."
                            FullyQualifiedName = value.FullyQualifiedName |> List.map QualifiedNamePart.create
                            Name = Name.Pascal.create value.Name
                            Value = value.Value
                            Documentation = value.Documentation
                        })
                    Documentation = tsEnumType.Documentation
                }
            | TsType.EnumCase tsEnumCase ->
                ResolvedType.EnumCase {
                    Parent = lazy (
                        match resolve tsEnumCase.Parent with
                        | ResolvedType.Enum enum -> enum
                        | _ -> failwith "Inlining an enum case returned a non enum type."
                        )
                    FullyQualifiedName = tsEnumCase.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Name = Name.Pascal.create tsEnumCase.Name 
                    Value = tsEnumCase.Value
                    Documentation = tsEnumCase.Documentation
                }
            | TsType.Union tsTypeUnion -> ResolvedType.Union { Types = tsTypeUnion.Types |> List.map lazyResolve }
            | TsType.Intersection tsTypeIntersection -> ResolvedType.Intersection { Types = tsTypeIntersection.Types |> List.map lazyResolve }
            | TsType.Literal tsLiteral -> ResolvedType.Literal tsLiteral
            | TsType.IndexedAccess tsIndexAccessType -> ResolvedType.IndexedAccess {
                Object = lazyResolve tsIndexAccessType.Object
                Index = lazyResolve tsIndexAccessType.Index
                }
            | TsType.TypeReference tsTypeReference ->
                buildFromTypeReference tsTypeReference
                |> ResolvedType.TypeReference
            | TsType.Array tsType -> ResolvedType.Array <| buildFrom isLibEs tsType
            | TsType.TypeParameter tsTypeParameter ->
                ResolvedType.TypeParameter {
                    Name = Name.Typar.create tsTypeParameter.Name
                    Constraint = lazyResolveOption tsTypeParameter.Constraint
                    Default = lazyResolveOption tsTypeParameter.Default
                    Documentation = tsTypeParameter.Documentation
                }
            | TsType.ReadOnly tsType -> ResolvedType.ReadOnly <| buildFrom isLibEs tsType
            | TsType.Tuple tsTuple -> ResolvedType.Tuple {
                    Tuple.FixedLength = tsTuple.FixedLength
                    MinRequired = tsTuple.MinRequired
                    IsReadOnly = tsTuple.IsReadOnly
                    Types =
                        let makeElement (ele: TsTupleElementType) = {
                            TupleElementType.Type = lazyResolve ele.Type
                            IsOptional = ele.IsOptional
                            IsRest = ele.IsRest
                        }
                        tsTuple.Types
                        |> List.map (function
                            | TsTupleElement.Fixed tsTupleElementType ->
                                TupleElement.Fixed (makeElement tsTupleElementType)
                            | FixedLabeled(s, tsTupleElementType) ->
                                TupleElement.FixedLabel(s, makeElement tsTupleElementType)
                            | TsTupleElement.Variadic i -> TupleElement.Variadic (lazyResolve i))
                }
            | TsType.Index tsIndex ->
                ResolvedType.Index { Type = lazyResolve tsIndex.Type }
            | TsType.Predicate tsTypePredicate ->
                ResolvedType.Predicate {
                    Type = lazyResolve tsTypePredicate.Type
                    ParameterName = Name.Camel.create tsTypePredicate.ParameterName
                    IsAssertion = tsTypePredicate.IsAssertion
                }
            | TsType.TypeLiteral tsTypeLiteral -> ResolvedType.TypeLiteral {
                    Members = tsTypeLiteral.Members |> List.map buildFromMember
                }
            | TsType.TemplateLiteral tsTemplateLiteralType -> ResolvedType.TemplateLiteral {
                    Text = tsTemplateLiteralType.Texts
                    Types = tsTemplateLiteralType.Types |> List.map lazyResolve
                }
            | TsType.Optional tsTypeReference ->
                buildFromTypeReference tsTypeReference
                |> ResolvedType.Optional
            | TsType.Substitution tsSubstitutionType -> ResolvedType.Substitution {
                    Base = lazyResolve tsSubstitutionType.Base
                    Constraint = lazyResolve tsSubstitutionType.Constraint
                }
            | TsType.TypeQuery tsTypeQuery -> ResolvedType.TypeQuery {
                    FullyQualifiedName = tsTypeQuery.FullyQualifiedName |> List.map QualifiedNamePart.create
                    Type = lazyResolve tsTypeQuery.Type
                }
        and buildFromTypeParameter (key: TypeKey, _) =
            lazy
            match resolve key with
            | ResolvedType.TypeParameter typ -> typ
            | _ -> failwith "Inlining a type parameter returned a non type parameter."
        and buildFromTypeReference (typ: TsTypeReference) =
            {
                TypeReference.Type = { Data = typ.Type; Result = lazy resolve typ.Type }
                TypeArguments = typ.TypeArguments |> List.map (fun arg -> { Data = arg; Result = lazy (resolve arg) } )
                ResolvedType = typ.ResolvedType |> Option.map (fun arg -> { Data = arg; Result = lazy (resolve arg) } )
            }
        and buildFromMember = function
            | TsMember.Method tsMethod ->
                tsMethod.ToList()
                |> List.map (fun method -> {
                    Method.Name = Name.Camel.create method.Name
                    Parameters = method.Parameters |> List.map buildFromParameter
                    Type = { Data = method.Type; Result = lazy resolve method.Type }
                    TypeParameters = method.TypeParameters |> List.map buildFromTypeParameter
                    Documentation = method.Documentation
                    IsOptional = method.IsOptional
                    IsStatic = method.IsStatic
                })
                |> Member.Method
            | TsMember.Property tsProperty -> Member.Property {
                Property.Name = Name.Camel.create tsProperty.Name
                Type = { Data = tsProperty.Type; Result = lazy resolve tsProperty.Type }
                Documentation = tsProperty.Documentation
                IsOptional = tsProperty.IsOptional
                IsStatic = tsProperty.IsStatic
                IsPrivate = tsProperty.IsPrivate
                Accessor = tsProperty.Accessor
                }
            | TsMember.GetAccessor tsGetAccessor -> Member.GetAccessor {
                    GetAccessor.Name = Name.Camel.create tsGetAccessor.Name
                    Type = { Data = tsGetAccessor.Type; Result = lazy resolve tsGetAccessor.Type }
                    IsStatic = tsGetAccessor.IsStatic
                    IsPrivate = tsGetAccessor.IsPrivate
                }
            | TsMember.SetAccessor tsSetAccessor -> Member.SetAccessor {
                SetAccessor.Name = Name.Camel.create tsSetAccessor.Name
                ArgumentType = { Data = tsSetAccessor.ArgumentType; Result = lazy resolve tsSetAccessor.ArgumentType }
                IsStatic = tsSetAccessor.IsStatic
                IsPrivate = tsSetAccessor.IsPrivate
                Documentation = tsSetAccessor.Documentation
                }
            | TsMember.CallSignature tsOverloadableConstruct ->
                tsOverloadableConstruct.ToList()
                |> List.map (fun value -> {
                    CallSignature.Documentation = value.Documentation
                    Parameters = value.Parameters |> List.map buildFromParameter
                    Type = { Data = value.Type; Result = lazy resolve value.Type }
                    TypeParameters = value.TypeParameters |> List.map buildFromTypeParameter
                })
                |> Member.CallSignature
            | TsMember.IndexSignature tsIndexSignature ->
                Member.IndexSignature {
                    IndexSignature.Parameters = tsIndexSignature.Parameters |> List.map buildFromParameter
                    Type = { Data = tsIndexSignature.Type; Result = lazy resolve tsIndexSignature.Type }
                    IsReadOnly = tsIndexSignature.IsReadOnly
                }
            | TsMember.ConstructSignature tsOverloadableConstruct ->
                tsOverloadableConstruct.ToList()
                |> List.map (fun value -> {
                    ConstructSignature.Type = { Data = value.Type; Result = lazy resolve value.Type }
                    Parameters = value.Parameters |> List.map buildFromParameter
                    TypeParameters = value.TypeParameters |> List.map buildFromTypeParameter
                })
                |> Member.ConstructSignature

        and buildFromParameter (para: TsParameter) = {
            Parameter.Name = Name.Camel.create para.Name
            IsOptional = para.IsOptional
            IsSpread = para.IsSpread
            Type = { Data = para.Type; Result = lazy resolve para.Type }
            Documentation = para.Documentation
        }
        and resolvePackage (packageId: PackageId) =
            match resolvedPackages.TryGetValue(packageId) with
            | true, value -> value
            | _ ->
                let package = decodedResult.PackageMap[packageId]
                let resolvedValue = buildFromPackage package
                resolvedPackages[packageId] <- resolvedValue
                resolvedValue
        and buildFromPackage (package: Package) =
            {
                Name = package.Name
                Version = package.Version
                Json = package.Json
                SubModules =
                    package.SubModules
                    |> List.map (lazyResolveWith resolveSubModule)
                Entry =
                    package.Entry
                    |> List.map (lazyResolveWith resolveSubModule)
            }
        and resolveSubModule (subModuleId: SubModuleId): ResolvedSubModule =
            match resolvedSubModules.TryGetValue(subModuleId) with
            | true, value -> value
            | _ ->
                let subModule = decodedResult.SubModuleMap[subModuleId]
                let resolvedValue = buildFromSubModule subModuleId subModule
                resolvedSubModules[subModuleId] <- resolvedValue
                resolvedValue
        and buildFromSubModule (subModuleId: SubModuleId) (subModule: SubModule): ResolvedSubModule =
            {
                Name = subModule.Name
                Path = subModule.Path
                Package = lazyResolveWith resolvePackage subModule.Package
                Dependees =
                    decodedResult.SourceDependeeMap
                    |> Map.tryFind subModuleId
                    |> Option.map (List.map (_.Dependency >> lazyResolveWith resolveSubModule))
                    |> Option.defaultValue []
                Dependencies =
                    decodedResult.SourceDependencyMap
                    |> Map.tryFind subModuleId
                    |> Option.map (List.map (_.Dependency >> lazyResolveWith resolveSubModule))
                    |> Option.defaultValue []
                Exports =
                    decodedResult.ExportMap
                    |> Map.tryFind (ValueSome subModuleId)
                    |> Option.defaultValue Set.empty
                    |> Set.toList
                    |> List.choose lazyResolveExport
            }
        and resolveExportPoint (exportPoint: ExportPoint): ResolvedExportPoint =
            match resolvedExportPoints.TryGetValue(exportPoint) with
            | true, value -> value
            | _ ->
                let resolvedValue = buildFromExportPoint exportPoint
                resolvedExportPoints[exportPoint] <- resolvedValue
                resolvedValue
        and buildFromExportPoint (exportPoint: ExportPoint) =
            { Name = exportPoint.Name
              SubModule = lazyResolveWith resolveSubModule exportPoint.SubModule }
        and buildSourceFromMetadata (metadata: Metadata) =
            match metadata.Source with
            | Xantham.Source.LibEs fileName -> Source.LibEs fileName
            | Xantham.Source.PackageInternal subModuleId ->
                lazyResolveWith resolveSubModule subModuleId |> Source.PackageInternal
            | Xantham.Source.Package exports ->
                {
                    Canonical = exports.Canonical |> resolveExportPoint
                    Aliases = exports.Aliases |> List.map resolveExportPoint
                }
                |> Source.Package
            | Xantham.Source.UnknownDeclared fileName -> Source.UnknownDeclared fileName
        let exportMap =
            typeExportMap
            |> Seq.map (fun (KeyValue(key, value)) ->
                match value with
                | TsExportDeclaration.Variable { Metadata = metadata } 
                | TsExportDeclaration.Interface { Metadata = metadata } 
                | TsExportDeclaration.TypeAlias { Metadata = metadata } 
                | TsExportDeclaration.Class { Metadata = metadata } 
                | TsExportDeclaration.Enum { Metadata = metadata } 
                | TsExportDeclaration.Module { Metadata = metadata } -> metadata.Source
                | TsExportDeclaration.Function funs -> funs.ValueOrHead.Metadata.Source
                , (lazyResolveExport key).Value
                )
            |> Seq.groupBy fst
            |> Seq.map (fun (source, values) ->
                source, values |> Seq.map snd |> List.ofSeq
                )
            |> Map.ofSeq
        {
            ResolveType = resolve
            ResolveExport = resolveExport
            ResolveExportPoint = resolveExportPoint
            ResolvePackage = resolvePackage
            ResolveSubModule = resolveSubModule
            ExportMap = exportMap
            Graph = lazy Graph.create false decodedResult
            ResolvedTypes = resolved
            ResolvedExports = resolvedExports
            ResolvedSubModules = resolvedSubModules
            ResolvedPackages = resolvedPackages
            ResolvedExportPoints = resolvedExportPoints
        }
        

/// <summary>
/// Active pattern for ergonomic forcing of a <see cref="T:LazyContainer`2"/>:
/// matching <c>Resolve t</c> binds <c>t</c> to the materialised lazy result.
/// </summary>
let inline (|Resolve|) (value: LazyContainer<_, 'T>) = value.Value