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

open System.Collections.Frozen
open System.Collections.Generic
open Xantham
open Xantham.Decoder


type ResolvedType =
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

and ResolvedExport =
    | Variable of Variable
    | Interface of Interface
    | TypeAlias of TypeAlias
    | Class of Class
    | Enum of EnumType
    | Function of Function list
    | Module of Module

and LazyResolvedType = Lazy<ResolvedType>
and LazyResolvedExport = Lazy<ResolvedExport>

and [<ReferenceEquality>] Module = {
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
    Name: Name<Case.pascal>
    IsNamespace: bool
    IsRecursive: bool
    Exports: ResolvedExport list
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
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
    Name: Name<Case.pascal>
    Members: Lazy<EnumCase> list
    Documentation: TsComment list
}
and [<ReferenceEquality>] EnumCase = {
    Parent: Lazy<EnumType>
    Source: string option
    FullyQualifiedName: string list
    Name: Name<Case.pascal>
    Value: TsLiteral
    Documentation: TsComment list
}
and [<ReferenceEquality>] Variable = {
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
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
    Documentation: TsComment list
    IsOptional: bool
    IsStatic: bool
} with interface IOverloadable

and [<ReferenceEquality>] CallSignature = {
    Documentation: TsComment list
    Parameters: Parameter list
    Type: LazyResolvedType
} with interface IOverloadable

and [<ReferenceEquality>] ConstructSignature = {
    Type: LazyResolvedType
    Parameters: Parameter list
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
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
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

and Member =
    | Method of Method list
    | Property of Property
    | GetAccessor of GetAccessor
    | SetAccessor of SetAccessor
    | CallSignature of CallSignature list
    | IndexSignature of IndexSignature
    | ConstructSignature of ConstructSignature list

and [<ReferenceEquality>] TypeReference = {
    Type: Lazy<ResolvedType>
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
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
    Name: Name<Case.pascal>
    Members: Member list
    TypeParameters: Lazy<TypeParameter> list
    Documentation: TsComment list
    Heritage: InterfaceHeritage
}
and [<ReferenceEquality>] Class = {
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
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
    IsLibEs: bool
    Source: string option
    FullyQualifiedName: string list
    Name: Name<Case.pascal>
    Type: LazyResolvedType
    TypeParameters: Lazy<TypeParameter> list
    Documentation: TsComment list
}
and [<ReferenceEquality>] SubstitutionType = {
    Base: LazyResolvedType
    Constraint: LazyResolvedType
}

type ArenaInterner = {
    ResolveType: TypeKey -> ResolvedType
    ResolveExport: TypeKey -> Result<ResolvedExport, ResolvedType>
    ExportMap: Map<string, ResolvedExport list>
}

module ArenaInterner =
    let create (decodedResult: DecodedResult) =
        let typeMap = decodedResult.TypeMap
        let exportMap = decodedResult.ExportMap
        let typeExportMap = decodedResult.ExportTypeMap
        let resolved = Dictionary<TypeKey, ResolvedType>()
        let resolvedExports = Dictionary<TypeKey, ResolvedExport>()
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
                    let resolvedValue = buildFrom (isLibEs typeKey) typeMap[typeKey]
                    resolved[typeKey] <- resolvedValue
                    resolvedValue
        and resolveExport (typeKey: TypeKey) =
            match resolvedExports.TryGetValue(typeKey) with
            | true, value -> Ok value
            | _ when typeExportMap.ContainsKey(typeKey) ->
                let resolvedValue = buildFromExport (isLibEs typeKey) typeExportMap[typeKey]
                resolvedExports[typeKey] <- resolvedValue
                resolvedValue
                |> Ok
            | _ ->
                resolve typeKey
                |> Error
        and buildFromExport (isLibEs: bool) (export: TsExportDeclaration): ResolvedExport =
            let inline lazyResolve typ = lazy (resolve typ)
            match export with
            | TsExportDeclaration.Variable tsVariable -> ResolvedExport.Variable {
                    IsLibEs = isLibEs
                    Source = tsVariable.Source
                    FullyQualifiedName = tsVariable.FullyQualifiedName
                    Variable.Name = Name.Camel.create tsVariable.Name
                    Type = lazyResolve tsVariable.Type
                    Documentation = tsVariable.Documentation
                }
            | TsExportDeclaration.Interface tsInterface -> ResolvedExport.Interface {
                    IsLibEs = isLibEs
                    Source = tsInterface.Source
                    FullyQualifiedName = tsInterface.FullyQualifiedName
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
                    Source = tsClass.Source
                    FullyQualifiedName = tsClass.FullyQualifiedName
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
                    Source = tsTypeAlias.Source
                    FullyQualifiedName = tsTypeAlias.FullyQualifiedName
                    Name = Name.Pascal.create tsTypeAlias.Name
                    Type = tsTypeAlias.Type |> lazyResolve
                    TypeParameters =
                        tsTypeAlias.TypeParameters
                        |> List.map buildFromTypeParameter
                    Documentation = tsTypeAlias.Documentation
                }
            | TsExportDeclaration.Enum tsEnumType -> ResolvedExport.Enum {
                    IsLibEs = isLibEs
                    Source = tsEnumType.Source
                    FullyQualifiedName = tsEnumType.FullyQualifiedName
                    Name = Name.Pascal.create tsEnumType.Name
                    Members = tsEnumType.Members |> List.map (fun value ->
                        lazy {
                            Parent =
                                lazy
                                    match resolve value.Parent with
                                    | ResolvedType.Enum enum -> enum
                                    | _ -> failwith "Inlining an enum returned a non enum type."
                            EnumCase.Source = value.Source
                            FullyQualifiedName = value.FullyQualifiedName
                            Name = Name.Pascal.create value.Name
                            Value = value.Value
                            Documentation = value.Documentation
                        })
                    Documentation = tsEnumType.Documentation
                }
            | TsExportDeclaration.Module tsModule ->
                ResolvedExport.Module {
                    IsLibEs = isLibEs
                    Source = tsModule.Source
                    FullyQualifiedName = tsModule.FullyQualifiedName
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
                        Function.Source = func.Source
                        FullyQualifiedName = func.FullyQualifiedName
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
            let inline lazyResolve typ = lazy (resolve typ)
            let inline lazyResolveOption typ = typ |> Option.map lazyResolve
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
                    Source = tsInterface.Source
                    FullyQualifiedName = tsInterface.FullyQualifiedName
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
                    Source = tsClass.Source
                    FullyQualifiedName = tsClass.FullyQualifiedName
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
                    Source = tsEnumType.Source
                    FullyQualifiedName = tsEnumType.FullyQualifiedName
                    Name = Name.Pascal.create tsEnumType.Name
                    Members = tsEnumType.Members |> List.map (fun value ->
                        lazy {
                            Parent =
                                lazy
                                    match resolve value.Parent with
                                    | ResolvedType.Enum enum -> enum
                                    | _ -> failwith "Inlining an enum returned a non enum type."
                            EnumCase.Source = value.Source
                            FullyQualifiedName = value.FullyQualifiedName
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
                    Source = tsEnumCase.Source
                    FullyQualifiedName = tsEnumCase.FullyQualifiedName
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
        and buildFromTypeParameter (key: TypeKey, _) =
            lazy
            match resolve key with
            | ResolvedType.TypeParameter typ -> typ
            | _ -> failwith "Inlining a type parameter returned a non type parameter."
        and buildFromTypeReference (typ: TsTypeReference) =
            {
                TypeReference.Type = lazy resolve typ.Type
                TypeArguments = typ.TypeArguments |> List.map (fun arg -> lazy resolve arg)
                ResolvedType = typ.ResolvedType |> Option.map (fun arg -> lazy resolve arg)
            }
        and buildFromMember = function
            | TsMember.Method tsMethod ->
                tsMethod.ToList()
                |> List.map (fun method -> {
                    Method.Name = Name.Camel.create method.Name
                    Parameters = method.Parameters |> List.map buildFromParameter
                    Type = lazy resolve method.Type
                    Documentation = method.Documentation
                    IsOptional = method.IsOptional
                    IsStatic = method.IsStatic
                })
                |> Member.Method
            | TsMember.Property tsProperty -> Member.Property {
                Property.Name = Name.Camel.create tsProperty.Name
                Type = lazy resolve tsProperty.Type
                Documentation = tsProperty.Documentation
                IsOptional = tsProperty.IsOptional
                IsStatic = tsProperty.IsStatic
                IsPrivate = tsProperty.IsPrivate
                Accessor = tsProperty.Accessor
                }
            | TsMember.GetAccessor tsGetAccessor -> Member.GetAccessor {
                    GetAccessor.Name = Name.Camel.create tsGetAccessor.Name
                    Type = lazy resolve tsGetAccessor.Type
                    IsStatic = tsGetAccessor.IsStatic
                    IsPrivate = tsGetAccessor.IsPrivate
                }
            | TsMember.SetAccessor tsSetAccessor -> Member.SetAccessor {
                SetAccessor.Name = Name.Camel.create tsSetAccessor.Name
                ArgumentType = lazy resolve tsSetAccessor.ArgumentType
                IsStatic = tsSetAccessor.IsStatic
                IsPrivate = tsSetAccessor.IsPrivate
                Documentation = tsSetAccessor.Documentation
                }
            | TsMember.CallSignature tsOverloadableConstruct ->
                tsOverloadableConstruct.ToList()
                |> List.map (fun value -> {
                    CallSignature.Documentation = value.Documentation
                    Parameters = value.Parameters |> List.map buildFromParameter
                    Type = lazy resolve value.Type
                })
                |> Member.CallSignature
            | TsMember.IndexSignature tsIndexSignature ->
                Member.IndexSignature {
                    IndexSignature.Parameters = tsIndexSignature.Parameters |> List.map buildFromParameter
                    Type = lazy resolve tsIndexSignature.Type
                    IsReadOnly = tsIndexSignature.IsReadOnly
                }
            | TsMember.ConstructSignature tsOverloadableConstruct ->
                tsOverloadableConstruct.ToList()
                |> List.map (fun value -> {
                    ConstructSignature.Type = lazy resolve value.Type
                    Parameters = value.Parameters |> List.map buildFromParameter
                })
                |> Member.ConstructSignature

        and buildFromParameter (para: TsParameter) = {
            Parameter.Name = Name.Camel.create para.Name
            IsOptional = para.IsOptional
            IsSpread = para.IsSpread
            Type = lazy resolve para.Type
            Documentation = para.Documentation
        }
        
        {
            ResolveType = resolve
            ResolveExport = resolveExport
            ExportMap =
                exportMap
                |> Map.map (fun _ value ->
                    value
                    |> Set.toList
                    |> List.choose (resolveExport >> Result.toOption)
                    )
        }

let inline (|Resolve|) (value: Lazy<'T>) = value.Value