module Xantham.Decoder.Runtime

open System.Collections.Frozen
open System.Collections.Generic
open Xantham
open Xantham.Decoder

module Map =
    let inline choose (fn: 'Key -> 'Item -> 'Outcome option) (map: Map<'Key, 'Item>) = Map.fold (fun acc key item -> match fn key item with Some value -> Map.add key value acc | None -> acc) (Map []) map

type IDictionaryTypeAccess =
    abstract member Conditionals: FrozenDictionary<TypeKey, TsConditionalType>
    abstract member Interfaces: FrozenDictionary<TypeKey, TsInterface>
    abstract member Classes: FrozenDictionary<TypeKey, TsClass>
    abstract member Primitives: FrozenDictionary<TypeKey, TypeKindPrimitive>
    abstract member EnumCases: FrozenDictionary<TypeKey, TsEnumCase>
    abstract member PathToEnum: FrozenDictionary<string list, TypeKey>
    abstract member Enums: FrozenDictionary<TypeKey, TsEnumType>
    abstract member TypeAliases: FrozenDictionary<TypeKey, TsTypeAlias>
    abstract member Unions: FrozenDictionary<TypeKey, TsTypeUnion>
    abstract member Intersections: FrozenDictionary<TypeKey, TsTypeIntersection>
    abstract member Literals: FrozenDictionary<TypeKey, TsLiteral>
    abstract member IndexedAccess: FrozenDictionary<TypeKey, TsIndexAccessType>
    abstract member Modules: FrozenDictionary<TypeKey, TsModule>
    abstract member TypeReferences: FrozenDictionary<TypeKey, TsTypeReference>
    abstract member Arrays: FrozenDictionary<TypeKey, TsType>
    abstract member TypeParameters: FrozenDictionary<TypeKey, TsTypeParameter>
    abstract member ReadOnlys: FrozenDictionary<TypeKey, TsType>
    abstract member Tuples: FrozenDictionary<TypeKey, TsTuple>
    abstract member Indexes: FrozenDictionary<TypeKey, TsIndex>
    abstract member Predicates: FrozenDictionary<TypeKey, TsTypePredicate>
    abstract member TypeLiterals: FrozenDictionary<TypeKey, TsTypeLiteral>
    abstract member TemplateLiterals: FrozenDictionary<TypeKey, TsTemplateLiteralType>
    abstract member Optional: FrozenDictionary<TypeKey, TsTypeReference>
    abstract member Substitutions: FrozenDictionary<TypeKey, TsSubstitutionType>

type IDictionaryTypeAccess with
    member this.TryGetEnumForCase (key: TsEnumCase) =
        key.FullyQualifiedName
        |> List.truncate (List.length key.FullyQualifiedName - 1)
        |> this.PathToEnum.TryFind 

type XanthamTree (config: RuntimeOptions) =
    let decodedResult =
        match Decoder.readWithOptions config with
        | Ok result -> result
        | Error error -> failwith error
    let typeMap = decodedResult.TypeMap.ToFrozenDictionary()
    let libSet = decodedResult.LibSet.ToFrozenSet()
    let exports = decodedResult.Exports
    let topLevelKeys = decodedResult.PrimaryExports
    let duplicateMap =
        decodedResult.DuplicateMap
        |> Map.map (fun _ (v, vals) -> vals |> Array.insertAt 0 v)
        |> _.ToFrozenDictionary()
    /// Determines if the given key is a type from the standard library.
    let isEsType key = libSet.Contains key
    /// Gets the type for the given key
    let getTypeForKey key =
        match typeMap.TryFind key with
        | Some node -> node
        | _ -> failwith $"Type not found for key {key}"
    let rec getSourceFromGlueType = function
        | TsType.TypeLiteral _
        | TsType.Primitive _ -> None
        | TsType.EnumCase { Source = source }
        | TsType.Module { Source = source }
        | TsType.TypeAlias { Source = source }
        | TsType.Enum { Source = source }
        | TsType.Interface { Source = source }
        | TsType.Class { Source = source } -> source
        | TsType.ReadOnly node
        | TsType.Array node -> getSourceFromGlueType node
        | _ -> None
    let typeMapConditionals = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Conditional value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapInterfaces = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Interface value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapClasses = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Class value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapPrimitives = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Primitive value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapEnums = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Enum value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapEnumCases = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.EnumCase value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapTypeAliases = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.TypeAlias value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapUnions = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Union value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapIntersections = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Intersection value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapLiterals = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Literal value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapIndexedAccess = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.IndexedAccess value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapModules = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Module value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    // Type references have generated type keys when the resolved type is known, as the resolved type is the true returned type.
    let typeMapTypeReferences =
        decodedResult.TypeMap
        |> Map.choose (fun _ -> function TsType.TypeReference value -> Some value | _ -> None)
        |> Seq.map(
            fun kv ->
                match kv.Value.ResolvedType with
                | Some resolvedTypeKey ->
                    resolvedTypeKey, kv.Value
                | None -> kv.Key, kv.Value
            )
        |> Map
        |> _.ToFrozenDictionary()
    let typeMapArrays = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Array value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapTypeParameters = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.TypeParameter value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapReadOnlys = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.ReadOnly value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapTuples = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Tuple value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapIndexes = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Index value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapPredicates = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Predicate value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapTypeLiterals = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.TypeLiteral value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapTemplateLiterals = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.TemplateLiteral value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapOptional = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Optional value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapSubstitutions = decodedResult.TypeMap |> Map.choose (fun _ -> function TsType.Substitution value -> Some value | _ -> None) |> _.ToFrozenDictionary()
    let typeMapEnumCasesToTypes =
        typeMapEnums
        |> Seq.map (fun kv -> kv.Value.FullyQualifiedName, kv.Key)
        |> Map.ofSeq
        |> _.ToFrozenDictionary()
    let sourceMap =
        decodedResult.TypeMap
        |> Map.fold (fun acc key value ->
            let source = getSourceFromGlueType value |> Option.defaultValue ""
            acc |> Map.change source (function
                | Some sources -> sources |> Map.add key value |> Some
                | None -> Map [key, value] |> Some)
            ) Map.empty
        |> Map.map (fun _ -> _.ToFrozenDictionary())
        |> _.ToFrozenDictionary()
    /// <summary>
    /// The duplicate map containing the raw nodes for each type key before
    /// selection was made. The head of each array is the <i>winning</i> node.
    /// </summary>
    member val DuplicateMap = duplicateMap with get
    /// <summary>
    /// A map of type keys to TsTypes.
    /// </summary>
    member val TypeDict = typeMap with get
    /// <summary>
    /// A set of keys which refer to types from the standard library.
    /// </summary>
    member val LibSet = libSet with get
    /// <summary>
    /// Index of the TypeDict by the source file path.
    /// </summary>
    member val SourceDict = sourceMap with get
    /// <summary>
    /// All exportable definitions from crawling the source file, and their imports.
    /// </summary>
    member val Exports = exports with get
    /// <summary>
    /// All the primary exports from the decoded file; a subset of the TypeDict.
    /// </summary>
    member val PrimaryExports = topLevelKeys with get
    interface IDictionaryTypeAccess with
        member this.Enums = typeMapEnums
        member this.EnumCases = typeMapEnumCases
        member this.Conditionals = typeMapConditionals
        member this.Interfaces = typeMapInterfaces
        member this.Classes = typeMapClasses
        member this.Primitives = typeMapPrimitives
        member this.TypeAliases = typeMapTypeAliases
        member this.Unions = typeMapUnions
        member this.Intersections = typeMapIntersections
        member this.Literals = typeMapLiterals
        member this.IndexedAccess = typeMapIndexedAccess
        member this.Modules = typeMapModules
        member this.TypeReferences = typeMapTypeReferences
        member this.Arrays = typeMapArrays
        member this.TypeParameters = typeMapTypeParameters
        member this.ReadOnlys = typeMapReadOnlys
        member this.Tuples = typeMapTuples
        member this.Indexes = typeMapIndexes
        member this.Predicates = typeMapPredicates
        member this.TypeLiterals = typeMapTypeLiterals
        member this.PathToEnum = typeMapEnumCasesToTypes
        member this.TemplateLiterals = typeMapTemplateLiterals
        member this.Optional = typeMapOptional
        member this.Substitutions = typeMapSubstitutions

    /// <summary>
    /// A convenience property for accessing filtered GlueTypes and non-GlueTypes
    /// in independent dictionaries.
    /// </summary>
    member inline this.Dicts = this :> IDictionaryTypeAccess
    /// <summary>
    /// Determines if the given key refers to a type from the standard library.
    /// </summary>
    member this.IsEsType = isEsType
    /// <summary>
    /// Gets the GlueType for the given key, or throws an exception if the key did not match a GlueType.
    /// </summary>
    /// <param name="key"></param>
    member this.GetTypeForKey key =
        getTypeForKey key
    
    new(fileName: string) = XanthamTree({
        File = fileName
        Remap = ValueNone
        DuplicateRemap = ValueNone
    })

/// Creates a utility type with common handling operations and optimized access
/// using FrozenDictionaries and FrozenSets for the result of a decoded xantham json file.
let create (fileName: string) = XanthamTree(fileName)

[<EntryPoint>]
let main _ =
    create <| System.IO.Path.Join(__SOURCE_DIRECTORY__,"../Xantham.Fable/output.json")
    0