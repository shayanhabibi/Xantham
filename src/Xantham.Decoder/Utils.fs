/// <summary>
/// Utility functions and types, mostly for internal Decoder use.
/// </summary>
module private Xantham.Decoder.Utils

open System.Collections.Generic
open Xantham

/// <summary>
/// Collects keys from <c>TsMember</c>'s
/// </summary>
let rec private getKeysFromMember (memberInfo: TsMember) =
    match memberInfo with
    | TsMember.Method glueMethod -> glueMethod.ToArray() |> Array.map _.Type
    | TsMember.Property glueProperty -> [| glueProperty.Type |]
    | TsMember.GetAccessor glueGetAccessor -> [| glueGetAccessor.Type |]
    | TsMember.SetAccessor glueSetAccessor -> [| glueSetAccessor.ArgumentType |]
    | TsMember.CallSignature glueCallSignature ->
        glueCallSignature.ToArray()
        |> Array.collect (fun glueCallSignature ->
            [|
                glueCallSignature.Type
                yield! glueCallSignature.Parameters |> List.map _.Type
            |]
        )
    | TsMember.IndexSignature glueIndexSignature ->
        [|
            glueIndexSignature.Type
            yield! glueIndexSignature.Parameters |> List.map _.Type
        |]
    | TsMember.ConstructSignature glueConstruct ->
        glueConstruct.ToArray() |> Array.collect (fun glueConstruct ->
            [|
                glueConstruct.Type
                yield! glueConstruct.Parameters |> List.map _.Type
            |])

/// <summary>
/// <b>For Debugging Purposes Primarily:</b><br/>
/// Recursively collects all keys from a TsType.
/// </summary>
let rec getKeys typ =
    match typ with
    | TsType.Array node -> getKeys node
    | TsType.GlobalThis -> [||]
    | TsType.Conditional glueConditionalType ->
        [|
            glueConditionalType.Check
            glueConditionalType.Extends
            glueConditionalType.True
            glueConditionalType.False
        |]
    | TsType.Interface glueInterface -> [|
        yield!
            glueInterface.Members
            |> List.toArray
            |> Array.collect getKeysFromMember
        yield! glueInterface.TypeParameters |> List.map fst
        yield!
            glueInterface.TypeParameters
            |> List.choose (snd >> _.Default)
        yield!
            glueInterface.TypeParameters
            |> List.choose (snd >> _.Constraint)
        yield!
            glueInterface.Heritage.Extends
            |> List.collect (fun heritage -> heritage.Type :: heritage.TypeArguments)
        |]
    | TsType.Class glueClass -> [|
        yield!
            glueClass.Members
            |> List.toArray
            |> Array.Parallel.collect getKeysFromMember
        yield! glueClass.TypeParameters |> List.map fst
        yield!
            glueClass.TypeParameters
            |> List.choose (snd >> _.Default)
        yield!
            glueClass.TypeParameters
            |> List.choose (snd >> _.Constraint)
        yield!
            glueClass.Heritage.Extends
            |> List.collect (fun heritage -> heritage.Type :: heritage.TypeArguments)
        match glueClass.Heritage.Implements with
        | Some value ->
            value.Type
            yield! value.TypeArguments
        | _ -> ()
        yield!
            glueClass.Constructors
            |> List.collect (_.Parameters >> List.map _.Type)
        |]
    | TsType.Primitive _ -> [||]
    | TsType.Enum _ -> [||]

    | TsType.Intersection (TsTypeIntersection values) 
    | TsType.Union (TsTypeUnion values) -> List.toArray values
    | TsType.Literal _ -> [||]
    | TsType.IndexedAccess glueIndexAccessType ->
        [|
            glueIndexAccessType.Object
            glueIndexAccessType.Index
        |]
    | TsType.TypeReference glueTypeReference ->
        [|
            glueTypeReference.Type
            yield!
                glueTypeReference.TypeArguments
        |]
    | TsType.TypeParameter glueTypeParameter ->
        [|
            match glueTypeParameter.Constraint with Some v -> v | _ -> ()
            match glueTypeParameter.Default with Some v -> v | _ -> ()
        |]
    | TsType.ReadOnly glueType -> getKeys glueType
    | TsType.Tuple glueTuple ->
        glueTuple.Types
        |> List.map (function
            | TsTupleElement.Variadic t
            | TsTupleElement.FixedLabeled(_, { Type = t })
            | TsTupleElement.Fixed { Type = t } -> t
            )
        |> List.toArray
    | TsType.Index glueIndex -> glueIndex.Type |> Array.singleton
    | TsType.Predicate glueTypePredicate -> glueTypePredicate.Type |> Array.singleton
    | TsType.TypeLiteral glueTypeLiteral -> glueTypeLiteral.Members |> List.toArray |> Array.Parallel.collect getKeysFromMember
    | TsType.EnumCase _ -> [||]
    | TsType.TemplateLiteral tsTemplateLiteralType -> tsTemplateLiteralType.Types |> List.toArray
    | TsType.Optional tsTypeReference -> getKeys (TsType.TypeReference tsTypeReference)
    | TsType.Substitution tsSubstitutionType -> [| tsSubstitutionType.Base; tsSubstitutionType.Constraint |]

/// <summary>
/// Recursively collects all keys from a <c>TsExportDeclaration</c>.
/// </summary>
and getKeysFromExport export =
    match export with
    | TsExportDeclaration.Variable glueVariable -> [| glueVariable.Type |]
    | TsExportDeclaration.Function glueFunctionDeclaration -> glueFunctionDeclaration.ToArray() |> Array.collect (fun glueFunctionDeclaration ->
        [|
            glueFunctionDeclaration.Type
            yield!
                glueFunctionDeclaration.Parameters
                |> List.map _.Type
            yield!
                glueFunctionDeclaration.TypeParameters
                |> List.collect (function
                    typ,{ Constraint = c; Default = d } ->
                        [
                            typ
                            match c with Some v -> v | _ -> ()
                            match d with Some v -> v | _ -> ()
                        ])
        |])
    | TsExportDeclaration.TypeAlias glueTypeAlias -> [|
            glueTypeAlias.Type
            yield!
                glueTypeAlias.TypeParameters
                |> List.collect (function
                    typ,{ Constraint = c; Default = d } ->
                        [
                            typ
                            match c with Some v -> v | _ -> ()
                            match d with Some v -> v | _ -> ()
                        ])
        |]
    | TsExportDeclaration.Module glueModuleDeclaration -> glueModuleDeclaration.Exports |> List.toArray |> Array.collect getKeysFromExport
    | TsExportDeclaration.Interface tsInterface -> TsType.Interface tsInterface |> getKeys
    | TsExportDeclaration.Class tsClass -> TsType.Class tsClass |> getKeys
    | TsExportDeclaration.Enum _ -> [||]

/// <summary>
/// Uses the given compression remap dictionary to repopulat the <c>types</c> map with the new type shapes that
/// use the new type keys.
/// </summary>
let private compressWithMap (types: Map<TypeKey, TsType>) (compressions: Dictionary<TypeKey, TypeKey>) =
    let inline swap key = compressions[key]
    let swapParameter (parameter: TsParameter) = { parameter with Type = swap parameter.Type }
    let swapMember = function
        | TsMember.Method glueMethod ->
            glueMethod.Values
            |> Array.map (fun glueMethod -> {
                glueMethod with
                    Parameters = glueMethod.Parameters |> List.map swapParameter
                    Type = swap glueMethod.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.Method
        | TsMember.Property tsProperty -> TsMember.Property { tsProperty with Type = swap tsProperty.Type }
        | TsMember.GetAccessor tsGetAccessor -> TsMember.GetAccessor { tsGetAccessor with Type = swap tsGetAccessor.Type }
        | TsMember.SetAccessor tsSetAccessor -> TsMember.SetAccessor { tsSetAccessor with ArgumentType = swap tsSetAccessor.ArgumentType }
        | TsMember.CallSignature tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun callSignature -> {
                callSignature with
                    Parameters = callSignature.Parameters |> List.map swapParameter
                    Type = swap callSignature.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.CallSignature
        | TsMember.IndexSignature tsIndexSignature ->
            TsMember.IndexSignature {
                tsIndexSignature with
                    Parameters = tsIndexSignature.Parameters |> List.map swapParameter
                    Type = swap tsIndexSignature.Type
            }
        | TsMember.ConstructSignature tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun constructSignature -> {
                constructSignature with
                    Parameters = constructSignature.Parameters |> List.map swapParameter
                    Type = swap constructSignature.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.ConstructSignature
    let swapTypar (typar: TsTypeParameter) = {
        typar with
            Default = typar.Default |> Option.map swap
            Constraint = typar.Constraint |> Option.map swap
    }
    let rec swapType = function
        | TsType.TypeReference value ->
            TsType.TypeReference {
                Type = swap value.Type
                TypeArguments = value.TypeArguments |> List.map swap
                ResolvedType = value.ResolvedType |> Option.map swap
            }
        | TsType.GlobalThis -> TsType.GlobalThis
        | TsType.Conditional tsConditionalType ->
            TsType.Conditional {
                Check = swap tsConditionalType.Check
                Extends = swap tsConditionalType.Extends
                True = swap tsConditionalType.True
                False = swap tsConditionalType.False
            }
        | TsType.Interface tsInterface ->
            TsType.Interface {
                tsInterface with
                    Members = tsInterface.Members |> List.map swapMember
                    TypeParameters =
                        tsInterface.TypeParameters
                        |> List.map (fun (key, typeParameter) ->
                            swap key,
                            swapTypar typeParameter
                            )
                    Heritage.Extends = tsInterface.Heritage.Extends |> List.map (fun heritage -> {
                        Type = swap heritage.Type
                        TypeArguments = heritage.TypeArguments |> List.map swap
                        ResolvedType = heritage.ResolvedType |> Option.map swap
                    })
            }
        | TsType.Class tsInterface ->
            TsType.Class {
                tsInterface with
                    Members = tsInterface.Members |> List.map swapMember
                    TypeParameters =
                        tsInterface.TypeParameters
                        |> List.map (fun (key, typeParameter) ->
                            swap key,
                            swapTypar typeParameter
                            )
                    Constructors = tsInterface.Constructors |> List.map (fun constructor -> {
                        constructor with
                            Parameters = constructor.Parameters |> List.map swapParameter
                    })
                    Heritage.Extends = tsInterface.Heritage.Extends |> List.map (fun heritage -> {
                        Type = swap heritage.Type
                        TypeArguments = heritage.TypeArguments |> List.map swap
                        ResolvedType = heritage.ResolvedType |> Option.map swap
                    })
                    Heritage.Implements = tsInterface.Heritage.Implements |> Option.map (fun heritage -> {
                        Type = swap heritage.Type
                        TypeArguments = heritage.TypeArguments |> List.map swap
                        ResolvedType = heritage.ResolvedType |> Option.map swap
                    })
            }
        | TsType.Enum _
        | TsType.EnumCase _
        | TsType.Literal _
        | TsType.Primitive _ as tsType -> tsType
        | TsType.Union (TsTypeUnion values) ->
            values
            |> List.map swap
            |> TsTypeUnion
            |> TsType.Union
        | TsType.Intersection (TsTypeIntersection values) ->
            values
            |> List.map swap
            |> TsTypeIntersection
            |> TsType.Intersection
        | TsType.IndexedAccess tsIndexAccessType ->
            TsType.IndexedAccess {
                Object = swap tsIndexAccessType.Object
                Index = swap tsIndexAccessType.Index
            }
        | TsType.Array tsType -> swapType tsType |> TsType.Array
        | TsType.TypeParameter tsTypeParameter ->
            TsType.TypeParameter (swapTypar tsTypeParameter)
        | TsType.ReadOnly tsType -> swapType tsType |> TsType.ReadOnly
        | TsType.Tuple tsTuple ->
            { tsTuple with
                Types =
                    tsTuple.Types
                    |> List.map (function
                        | TsTupleElement.FixedLabeled (name, typ) ->
                            TsTupleElement.FixedLabeled (name, { typ with Type = swap typ.Type })
                        | Variadic i -> Variadic (swap i)
                        | Fixed tsTupleElementType ->
                            Fixed { tsTupleElementType with Type = swap tsTupleElementType.Type }
                        ) }
            |> TsType.Tuple
        | TsType.Index tsIndex -> TsType.Index { Type = swap tsIndex.Type }
        | TsType.Predicate tsTypePredicate -> TsType.Predicate { tsTypePredicate with Type = swap tsTypePredicate.Type }
        | TsType.TypeLiteral tsTypeLiteral ->
            TsType.TypeLiteral { Members = tsTypeLiteral.Members |> List.map swapMember }
        | TsType.TemplateLiteral tsTemplateLiteralType ->
            {
                tsTemplateLiteralType with
                    Types =
                        tsTemplateLiteralType.Types
                        |> List.map swap
            }
            |> TsType.TemplateLiteral
        | TsType.Optional tsTypeReference ->
            {
                Type = swap tsTypeReference.Type
                TypeArguments = tsTypeReference.TypeArguments |> List.map swap
                ResolvedType = tsTypeReference.ResolvedType |> Option.map swap
            }
            |> TsType.Optional
        | TsType.Substitution tsSubstitutionType ->
            TsType.Substitution {
                Base = swap tsSubstitutionType.Base
                Constraint = swap tsSubstitutionType.Constraint
            }

    types
    |> Map.toArray
    |> Array.Parallel.filter (fun (key, _) -> compressions.ContainsValue(key))
    |> Array.Parallel.map (fun (key, value) -> key, swapType value)
    |> Map.ofArray
    , compressions

let private compressResult (types: Map<TypeKey, TsType>) (protectedKeys: TypeKey list) =
    // Collect duplicate type shapes over multiple type keys
    let dupeCollections = Dictionary<TsType, TypeKey ResizeArray>()
    let addTypeToDupes key typ =
        match dupeCollections.TryGetValue(typ) with
        | true, dupes -> dupes.Add(key)
        | _ -> dupeCollections.Add(typ, ResizeArray([ key ]))
    let primitives = Map [
        TypeKindPrimitive.Any.TypeKey, TsType.Primitive TypeKindPrimitive.Any
        TypeKindPrimitive.Any.TypeKey, TsType.Primitive TypeKindPrimitive.Any
        TypeKindPrimitive.Unknown.TypeKey, TsType.Primitive TypeKindPrimitive.Unknown
        TypeKindPrimitive.Never.TypeKey, TsType.Primitive TypeKindPrimitive.Never
        TypeKindPrimitive.Void.TypeKey, TsType.Primitive TypeKindPrimitive.Void
        TypeKindPrimitive.Undefined.TypeKey, TsType.Primitive TypeKindPrimitive.Undefined
        TypeKindPrimitive.Null.TypeKey, TsType.Primitive TypeKindPrimitive.Null
        TypeKindPrimitive.String.TypeKey, TsType.Primitive TypeKindPrimitive.String
        TypeKindPrimitive.Integer.TypeKey, TsType.Primitive TypeKindPrimitive.Integer
        TypeKindPrimitive.Number.TypeKey, TsType.Primitive TypeKindPrimitive.Number
        TypeKindPrimitive.Boolean.TypeKey, TsType.Primitive TypeKindPrimitive.Boolean
        TypeKindPrimitive.BigInt.TypeKey, TsType.Primitive TypeKindPrimitive.BigInt
        TypeKindPrimitive.ESSymbol.TypeKey, TsType.Primitive TypeKindPrimitive.ESSymbol
        TypeKindPrimitive.NonPrimitive.TypeKey, TsType.Primitive TypeKindPrimitive.NonPrimitive
    ]
    primitives
    |> Map.iter addTypeToDupes
    types
    |> Map.iter addTypeToDupes
    dupeCollections
    |> Seq.collect (fun (KeyValue(key, dupes)) ->
        let key =
            if primitives.Values.Contains(key) then
                primitives
                |> Seq.find (fun (KeyValue(_, value)) -> key = value)
                |> _.Key
            else dupes |> Seq.max
        dupes
        |> Seq.map (fun dupe -> KeyValuePair(dupe, key))
        )
    |> Seq.distinctBy _.Key
    |> Dictionary
    |> fun remap ->
        for protectedKey in protectedKeys do
            if remap.ContainsKey(protectedKey) then
                remap[protectedKey] <- protectedKey
        remap
    |> compressWithMap types


let private compressExports (exports: Map<TypeKey, TsExportDeclaration>) (compressions: Dictionary<TypeKey, TypeKey>) =
    let inline swap key = compressions[key]
    let swapParameter (parameter: TsParameter) = { parameter with Type = swap parameter.Type }
    let swapMember = function
        | TsMember.Method glueMethod ->
            glueMethod.Values
            |> Array.map (fun glueMethod -> {
                glueMethod with
                    Parameters = glueMethod.Parameters |> List.map swapParameter
                    Type = swap glueMethod.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.Method
        | TsMember.Property tsProperty -> TsMember.Property { tsProperty with Type = swap tsProperty.Type }
        | TsMember.GetAccessor tsGetAccessor -> TsMember.GetAccessor { tsGetAccessor with Type = swap tsGetAccessor.Type }
        | TsMember.SetAccessor tsSetAccessor -> TsMember.SetAccessor { tsSetAccessor with ArgumentType = swap tsSetAccessor.ArgumentType }
        | TsMember.CallSignature tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun callSignature -> {
                callSignature with
                    Parameters = callSignature.Parameters |> List.map swapParameter
                    Type = swap callSignature.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.CallSignature
        | TsMember.IndexSignature tsIndexSignature ->
            TsMember.IndexSignature {
                tsIndexSignature with
                    Parameters = tsIndexSignature.Parameters |> List.map swapParameter
                    Type = swap tsIndexSignature.Type
            }
        | TsMember.ConstructSignature tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun constructSignature -> {
                constructSignature with
                    Parameters = constructSignature.Parameters |> List.map swapParameter
                    Type = swap constructSignature.Type
            })
            |> TsOverloadableConstruct.Create
            |> TsMember.ConstructSignature
    let swapTypar (typar: TsTypeParameter) = {
        typar with
            Default = typar.Default |> Option.map swap
            Constraint = typar.Constraint |> Option.map swap
    }
    let rec swapExport = function
        | TsExportDeclaration.Variable glueVariable -> TsExportDeclaration.Variable { glueVariable with Type = swap glueVariable.Type }
        | TsExportDeclaration.Interface tsInterface -> TsExportDeclaration.Interface {
            tsInterface with
                Members = tsInterface.Members |> List.map swapMember
                TypeParameters = tsInterface.TypeParameters |> List.map (fun (key, typeParameter) -> swap key, swapTypar typeParameter)
                Heritage.Extends = tsInterface.Heritage.Extends |> List.map (fun heritage -> {
                    Type = swap heritage.Type
                    TypeArguments = heritage.TypeArguments |> List.map swap
                    ResolvedType = heritage.ResolvedType |> Option.map swap
                })
            }
        | TsExportDeclaration.TypeAlias tsTypeAlias -> TsExportDeclaration.TypeAlias {
            tsTypeAlias with
                TypeParameters = tsTypeAlias.TypeParameters |> List.map (fun (key, typeParameter) -> swap key, swapTypar typeParameter)
                Type = swap tsTypeAlias.Type
            }
        | TsExportDeclaration.Class tsInterface -> 
            TsExportDeclaration.Class {
                tsInterface with
                    Members = tsInterface.Members |> List.map swapMember
                    TypeParameters =
                        tsInterface.TypeParameters
                        |> List.map (fun (key, typeParameter) ->
                            swap key,
                            swapTypar typeParameter
                            )
                    Constructors = tsInterface.Constructors |> List.map (fun constructor -> {
                        constructor with
                            Parameters = constructor.Parameters |> List.map swapParameter
                    })
                    Heritage.Extends = tsInterface.Heritage.Extends |> List.map (fun heritage -> {
                        Type = swap heritage.Type
                        TypeArguments = heritage.TypeArguments |> List.map swap
                        ResolvedType = heritage.ResolvedType |> Option.map swap
                    })
                    Heritage.Implements = tsInterface.Heritage.Implements |> Option.map (fun heritage -> {
                        Type = swap heritage.Type
                        TypeArguments = heritage.TypeArguments |> List.map swap
                        ResolvedType = heritage.ResolvedType |> Option.map swap
                    })
            }
        | TsExportDeclaration.Enum _ as export -> export
        | TsExportDeclaration.Module tsModule ->
            TsExportDeclaration.Module {
                tsModule with
                    Exports = tsModule.Exports |> List.map swapExport
            }
        | TsExportDeclaration.Function tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.Parallel.map (fun tsOverloadableConstruct -> {
                tsOverloadableConstruct with
                    Parameters = tsOverloadableConstruct.Parameters |> List.map swapParameter
                    Type = swap tsOverloadableConstruct.Type
                    TypeParameters = tsOverloadableConstruct.TypeParameters |> List.map (fun (key, typeParameter) -> swap key, swapTypar typeParameter)
                    SignatureKey = swap tsOverloadableConstruct.SignatureKey
            })
            |> TsOverloadableConstruct.Create
            |> TsExportDeclaration.Function
    exports
    |> Map.toArray
    |> Array.Parallel.filter (fun (key, _) ->
        let conservative =
            (compressions.ContainsKey(key) && compressions.ContainsValue(key))
            || not(compressions.ContainsKey(key) || compressions.ContainsValue(key))
        conservative)
    |> Array.Parallel.map (fun (key, value) -> key, swapExport value)
    |> Map.ofArray

/// <summary>
/// <para>Compresses a <c>Schema.EncodedResult</c> to a smaller typemap.</para>
/// <para>Collects duplicate type shapes, and remaps them to a common typekey, which
/// is then propagated through all types and exports.</para>
/// <para>This is a recursive process, so it will continue to compress until no more
/// changes are made.</para>
/// <para>This is a deterministic process, and will always produce the same output.</para>
/// <para>This will not remove/change keys which are export keys.</para>
/// </summary>
let compress (result: Schema.EncodedResult) =
    printfn "Compressing..."
    // for diagnostics
    let initialCount = result.Types.Count
    let mutable cycle = 0
    // condition check to prevent infinite loops
    let mutable lastCount = 0
    // interim store of results
    let mutable types = result.Types
    let mutable exports = result.ExportedDeclarations
    // top level keys to protect
    let topLevelKeys = result.TopLevelExports
    while lastCount <> types.Count do
        lastCount <- types.Count
        let newTypes, compressions = compressResult types (topLevelKeys |> List.append (result.ExportedDeclarations.Keys |> Seq.toList))
        types <- newTypes
        exports <- compressExports exports compressions
        cycle <- cycle + 1
    printfn $"Compressed typemap from %i{initialCount} to %i{types.Count} types over %i{cycle} cycles."
    { result with Types = types; ExportedDeclarations = exports }

module private Sanitize =
    let santizeTypes (result: Schema.EncodedResult) =
        printfn "Sanitizing types..."
        let mutable count = 0
        let inline fixKey key input =
            if input = key then
                count <- count + 1
                TypeKindPrimitive.NonPrimitive.TypeKey
            else
                input
        let sanitizedTypes =
            result.Types
            |> Map.map (fun key typ ->
            let inline fixKey input = fixKey key input
            match typ with
            | TsType.TypeReference value when value.Type = key || List.contains key value.TypeArguments || Option.contains key value.ResolvedType ->
                TsType.TypeReference {
                    Type = fixKey value.Type
                    TypeArguments = value.TypeArguments |> List.map fixKey
                    ResolvedType = value.ResolvedType |> Option.map fixKey
                }
            | TsType.Conditional tsConditionalType when List.contains key [
                    tsConditionalType.Extends
                    tsConditionalType.Check
                    tsConditionalType.False
                    tsConditionalType.True
                ] ->
                TsType.Conditional {
                    Extends = fixKey tsConditionalType.Extends
                    Check = fixKey tsConditionalType.Check
                    True = fixKey tsConditionalType.True
                    False = fixKey tsConditionalType.False
                }
            | TsType.Union tsUnion when tsUnion.Types |> List.contains key ->
                TsType.Union (TsTypeUnion (tsUnion.Types |> List.map fixKey))
            | TsType.Intersection tsTypeIntersection when tsTypeIntersection.Types |> List.contains key ->
                TsType.Intersection (TsTypeIntersection (tsTypeIntersection.Types |> List.map fixKey))
            | TsType.IndexedAccess tsIndexAccessType when tsIndexAccessType.Index = key || tsIndexAccessType.Object = key ->
                TsType.IndexedAccess {
                    Index = fixKey tsIndexAccessType.Index
                    Object = fixKey tsIndexAccessType.Object
                }
            | TsType.TypeParameter tsTypeParameter when Option.contains key tsTypeParameter.Constraint || Option.contains key tsTypeParameter.Default ->
                TsType.TypeParameter {
                    tsTypeParameter with
                        Constraint = tsTypeParameter.Constraint |> Option.map fixKey
                        Default = tsTypeParameter.Default |> Option.map fixKey
                }
            | TsType.Tuple tsTuple when tsTuple.Types |> List.exists (function
                | TsTupleElement.FixedLabeled (_, { Type = innerKey })
                | TsTupleElement.Variadic innerKey
                | TsTupleElement.Fixed { Type = innerKey } -> innerKey = key
                ) ->
                TsType.Tuple {
                    tsTuple with
                        Types =
                            tsTuple.Types
                            |> List.map (function
                                | TsTupleElement.FixedLabeled (name, value) ->
                                    TsTupleElement.FixedLabeled (name, { value with Type = fixKey value.Type })
                                | TsTupleElement.Variadic innerKey -> Variadic (fixKey innerKey)
                                | Fixed tsTupleElementType -> Fixed { tsTupleElementType with Type = fixKey tsTupleElementType.Type }
                                )
                }
            | TsType.Index tsIndex when tsIndex.Type = key -> TsType.Index { Type = fixKey tsIndex.Type }
            | TsType.TemplateLiteral tsTemplateLiteralType when tsTemplateLiteralType.Types |> List.contains key ->
                { tsTemplateLiteralType with Types = tsTemplateLiteralType.Types |> List.map fixKey }
                |> TsType.TemplateLiteral
            | TsType.Optional tsTypeReference when tsTypeReference.Type = key || List.contains key tsTypeReference.TypeArguments || Option.contains key tsTypeReference.ResolvedType ->
                TsType.Optional {
                    Type = fixKey tsTypeReference.Type
                    TypeArguments = tsTypeReference.TypeArguments |> List.map fixKey
                    ResolvedType = tsTypeReference.ResolvedType |> Option.map fixKey
                }
            | TsType.Substitution tsSubstitutionType when tsSubstitutionType.Constraint = key || tsSubstitutionType.Constraint = key ->
                TsType.Substitution {
                    Base = fixKey tsSubstitutionType.Base
                    Constraint = fixKey tsSubstitutionType.Constraint
                }
            | value -> value
            )
        if count > 0 then
            printfn $"Sanitized %i{count} types."
        { result with Types = sanitizedTypes }

let sanitize (result: Schema.EncodedResult) =
    Sanitize.santizeTypes result