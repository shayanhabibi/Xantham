module Xantham.SimpleGenerator.Generator.TypeAliasRender

open Fabulous.AST
open Xantham
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Generator.EnumRender
open Xantham.SimpleGenerator.Patterns
open Xantham.Decoder

let getTypeAliasPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeAlias>) = fun typeAliasParentPath ->
    genCache.pathResolver.Prerenderer typeAliasParentPath (TypeAlias.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeAlias>) =
    let underlyingTypeFn = 
        match TypeAlias.type' key with
        | MasterKey.KeyType.TypeLiteral (TypeLiteral.IsCallMethod callMethod) ->
            CallSignatureRender.prerender genCache callMethod 
            >> TypeAliasUnderlyingType.CallSignature
        | MasterKey.KeyType.TypeLiteral typeLiteral ->
            TypeLiteralRender.prerender genCache typeLiteral 
            >> TypeAliasUnderlyingType.TypeLiteral
        | MasterKey.KeyType.Class classKey ->
            ClassRender.prerender genCache classKey 
            >> TypeAliasUnderlyingType.Class
        | MasterKey.KeyType.Interface interfaceKey ->
            InterfaceRender.prerender genCache interfaceKey 
            >> TypeAliasUnderlyingType.Interface
        | MasterKey.KeyType.Union unionKey ->
            EnumRender.prerenderFromUnion unionKey
            >> function
                | Ok enum ->
                    TypeAliasUnderlyingType.Union enum
                | Error err ->
                    match err with
                    | EnumRender.EnumFromUnionError.IsBoolean ->
                        TypeAliasUnderlyingType.TypeReference { Type = TypeRefRender.create false Types.bool; TypeArguments = [||] }
                    | EnumRender.EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedEnumCasesAndNullCases(literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedEnumCases(literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedNulls (literalsEnum = enum) 
                    | EnumRender.EnumFromUnionError.ContainedNonLiteralCases (literalsEnum = enum) -> 
                        TypeAliasUnderlyingType.Union enum
                    | EnumRender.EnumFromUnionError.NoLiteralCases key -> 
                        TypeAliasUnderlyingType.TypeReference {
                            Type = // TODO - placeholder
                                TypeRefRender.create false Types.string
                            TypeArguments = [||]
                        }
        | MasterKey.KeyType.Intersection intersectionKey ->
            InterfaceRender.prerenderFromIntersection genCache intersectionKey
            >> TypeAliasUnderlyingType.Interface
        | MasterKey.KeyType.TypeReference typeRef ->
            TypeReferenceRender.prerender genCache typeRef
            >> TypeAliasUnderlyingType.TypeReference
        | MasterKey.MasterBuilder (Value builder) ->
            failwithf "%A" builder

    let name = TypeAlias.name key |> PatternContext.value |> Name.Pascal.create
    let typeParameterFns =
        TypeAlias.typeParameters key
        |> ValueOption.map (
            PatternContext.Array.cmap (TypeParameterRender.prerender genCache)
            >> PatternContext.value)
        |> ValueOption.defaultValue [||]
    let typeAliasMasterKey = TypeAlias.toMasterKey key
    fun typeAliasParentPath ->
        let typeAliasPath = genCache.pathResolver.Prerenderer typeAliasParentPath typeAliasMasterKey
        {
            TypeAliasRender.Name = name
            TypeParameters = Array.mapApply typeAliasPath typeParameterFns 
            UnderlyingType = underlyingTypeFn typeAliasPath
        }
        
        
// We can safely reference types so long as we don't render the members of the types outside of the
// full render (ie the short circuit must be available)
let createRender (genCache: GeneratorContext) isNullable (typeAlias: PatternContextHolder<KeyTypeAlias>) =
    /// The path to the type alias. Concrete. Constant.
    let resultPath =
        KeyPath.initConcreteTypeWithMasterKey
            (TypeAlias.toMasterKey typeAlias).Value
            typeAlias
        |> KeyPathKind.ConcreteType
        
    /// Pascal cased name of the type alias.
    let name =
        TypeAlias.name typeAlias
        |> PatternContext.value
        |> Name.Pascal.create
        
    /// Type parameters of the type alias. Prerendered since the parent path
    /// is already known.
    let typeParameters =
        TypeAlias.typeParameters typeAlias
        |> ValueOption.map (
            PatternContext.Array.cmap (TypeParameterRender.prerender genCache)
            >> PatternContext.value
            )
        |> ValueOption.defaultValue [||]
        |> Array.mapApply resultPath
    let typeAliasMasterKey = TypeAlias.toMasterKey typeAlias
    /// Create the render output given an underlying type alias kind.
    let makeMaybePathedRender = fun underlyingRender ->
        TypeMaybePathedRender.Pathed {
            Path = resultPath
            Render = fun _ ->
                {
                    TypeAliasRender.Name = name
                    TypeParameters = typeParameters
                    UnderlyingType = underlyingRender
                }
                |> TypeRender.TypeAlias
        }
    /// The default short circuit if rendering the path to the type
    let pathedShortCircuit = TypeRefRender.create isNullable resultPath
    /// The default short circuit, with a switch for nullability
    let pathedShortCircuitWithNullable = fun isNullable -> TypeRefRender.create isNullable resultPath
    match TypeAlias.type' typeAlias with
    | MasterKey.KeyType.TypeLiteral (TypeLiteral.IsCallMethod callMethod) ->
        let typeRender =
            CallSignatureRender.prerender genCache callMethod resultPath
            |> TypeAliasUnderlyingType.CallSignature
            |> makeMaybePathedRender
        match callMethod with
        | CallSignature.Parameters (Array.Length l & parameters) when l < 3 ->
            // we'll make a definition, but we'll also just render
            // the signature immediately.
            let parameters =
                parameters
                |> ParameterRender.prerender genCache
            let returnType =
                CallSignature.type' callMethod
                |> PatternContext.value
                |> GeneratorContext.getTypeRef genCache
            let shortCircuit = fun path ->
                let parameters =
                    parameters path
                    |> ParameterRenderArray.prerenderNamelessTypes genCache path
                    |> _.Default
                let returnType =
                    returnType path
                    |> TypeRefRender.toWidget genCache path
                Ast.Funs(parameters, returnType)
                |> TypeRefRender.create isNullable
                
            {
                ShortCircuit = shortCircuit
                Full = ValueSome(lazy typeRender)
            }
            // |> Default
        | _ ->
            {
                ShortCircuit = fun _ -> pathedShortCircuit
                Full = ValueSome(lazy typeRender)
            }
            // |> Default
    | MasterKey.KeyType.Interface interfaceKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome(lazy
                InterfaceRender.prerender genCache interfaceKey resultPath
                |> TypeAliasUnderlyingType.Interface
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Class classKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome(lazy
                ClassRender.prerender genCache classKey resultPath
                |> TypeAliasUnderlyingType.Class
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.TypeLiteral typeLiteralKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome(lazy
                TypeLiteralRender.prerender genCache typeLiteralKey resultPath
                |> TypeAliasUnderlyingType.TypeLiteral
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Union unionKey ->
        let createDefaultRender = fun nullable enumRender ->
            {
                ShortCircuit = fun _ -> pathedShortCircuitWithNullable nullable
                Full = ValueSome (lazy
                    TypeAliasUnderlyingType.Union enumRender
                    |> makeMaybePathedRender
                )
            }
            
        let createRenderForUnionErrors = function
            | IsBoolean -> Render.createShortOnly isNullable Types.bool //|> Default
            | ContainedNulls(_, literalsEnum) -> createDefaultRender true literalsEnum //|> Default
            | NoLiteralCases patternContextHolder ->
                {
                    ShortCircuit = fun _ -> pathedShortCircuit
                    Full = ValueSome (lazy
                        {
                            TypeReferenceRender.Type =
                                ErasedUnionRender.prerenderFromUnion genCache patternContextHolder resultPath
                                |> ErasedUnionRender.toWidget genCache
                                |> funApply resultPath
                                |> TypeRefRender.create isNullable
                            TypeArguments = [||]
                        }
                        |> TypeAliasUnderlyingType.TypeReference
                        |> makeMaybePathedRender
                        )
                }
                // |> Default
            // none of these will need the literal to be rendered
            // TODO - if we decide to start rendering the enum cases inlined, then we will need to change the above
            | ContainedNonLiteralCases(_, literalEnums) 
            | ContainedNonLiteralCasesAndNullCases(_, _, literalEnums) 
            | ContainedEnumCases(_, _, literalEnums) 
            | ContainedEnumCasesAndNullCases(_, _, _, literalEnums) 
            | ContainedEnumCasesAndNonLiteralCases(_, _, _, literalEnums) 
            | ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, _, _, literalEnums) as case ->
                let needsLiteralTypeDefn = Array.isEmpty literalEnums.Cases |> not
                let isNullable =
                    isNullable
                    || case.IsContainedNonLiteralCasesAndNullCases
                    || case.IsContainedEnumCasesAndNonLiteralCasesAndNullCases
                    || case.IsContainedEnumCasesAndNullCases
                let nonLiterals =
                    match case with
                    | ContainedNonLiteralCases(nonLiterals, _) 
                    | ContainedNonLiteralCasesAndNullCases(nonLiterals, _, _) 
                    | ContainedEnumCasesAndNonLiteralCases(_, _, nonLiterals, _) 
                    | ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, nonLiterals, _, _) -> nonLiterals
                    | _ -> [||]
                let enumCases, enumKeys =
                    match case with
                    | ContainedEnumCases(enumCases, enumKeys, _) 
                    | ContainedEnumCasesAndNullCases(enumCases, enumKeys, _, _) 
                    | ContainedEnumCasesAndNonLiteralCases(enumCases, enumKeys, _, _) 
                    | ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enumKeys, _, _, _) -> enumCases, enumKeys
                    | _ -> [||], [||]
                let enumKeys =
                    PatternContext.prepare genCache.ctx enumCases
                    |> PatternContext.Array.cbind EnumCaseKey.parentEnumKey
                    |> PatternContext.map (Array.append enumKeys)
                    |> PatternContext.Array.cbind EnumKey.toMasterKey
                    |> PatternContext.map Array.distinct
                let typeRenderLiterals =
                    if not needsLiteralTypeDefn then ValueNone else
                    let name = "Literals"
                    let nameKey = genCache.ctx.createNameKey name
                    let typePath =
                        resultPath.Value
                        |> KeyPath.appendQualifierKey nameKey
                        |> KeyPath.addMeasure
                        |> KeyPathKind.ConcreteType
                    let literalUnion = TypeRender.LiteralUnion {
                        literalEnums with Name = Name.Pascal.create name
                    }
                    let shortCircuit = TypeRefRender.create false typePath
                    {
                        ShortCircuit = fun _ -> shortCircuit
                        Full = ValueSome (lazy
                            TypeMaybePathedRender.Pathed {
                                Path = typePath
                                Render = fun _ -> literalUnion
                            }
                            )
                    }
                    |> ValueSome
                let prerenderErasedUnion =
                    enumKeys
                    |> PatternContext.map (Array.append nonLiterals)
                    |> ErasedUnionRender.prerenderFromTypeArray genCache
                let typeCountOfErasedUnion =
                    enumKeys.Value.Length + nonLiterals.Length
                    + (if needsLiteralTypeDefn then 1 else 0)
                let shortCircuitErasedUnion = fun parentPath ->
                    prerenderErasedUnion parentPath
                    |> function
                        | value when needsLiteralTypeDefn ->
                            { value with
                                Types = Array.appendOne (
                                    typeRenderLiterals.Value.ShortCircuit parentPath
                                ) value.Types }
                        | value -> value
                    |> ErasedUnionRender.toWidget genCache
                    |> funApply parentPath
                    |> TypeRefRender.create isNullable
                {
                    ShortCircuit = fun parentPath ->
                        if typeCountOfErasedUnion < 4
                        then shortCircuitErasedUnion parentPath
                        else TypeRefRender.create isNullable resultPath
                    Full = ValueSome (lazy
                        {
                            TypeReferenceRender.Type =
                                shortCircuitErasedUnion resultPath
                            TypeArguments = [||]
                        }
                        |> TypeAliasUnderlyingType.TypeReference
                        |> makeMaybePathedRender
                    )
                }
                // |> fun defrender ->
                //     match typeRenderLiterals with
                //     | ValueSome aux -> Auxilliary(defrender, aux)
                //     | ValueNone -> Default defrender
        match EnumRender.prerenderFromUnion unionKey resultPath with
        | Ok enumRender -> createDefaultRender isNullable enumRender //|>  Default
        | Error err -> createRenderForUnionErrors err
    | MasterKey.KeyType.Intersection intersectionKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                InterfaceRender.prerenderFromIntersection genCache intersectionKey resultPath
                |> TypeAliasUnderlyingType.Interface
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.TypeReference typeRef ->
        // TODO - should we just render the type reference if there are no type arguments?
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                TypeReferenceRender.prerender genCache typeRef resultPath
                |> TypeAliasUnderlyingType.TypeReference
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Enum enumKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                EnumRender.prerender enumKey
                |> TypeAliasUnderlyingType.Union
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.TypeParameter typeParameter ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                {
                    TypeReferenceRender.Type =
                        TypeParameterRender.prerender genCache typeParameter resultPath
                        |> TypeParameterRender.toWidget
                        |> TypeRefRender.create false
                    TypeArguments = [||]
                }
                |> TypeAliasUnderlyingType.TypeReference
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Tuple tupleKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                {
                    TypeReferenceRender.Type =
                        TupleRender.prerender genCache tupleKey resultPath
                        |> TupleRender.toTupleRender
                        |> TupleRender.toTypes genCache
                        |> funApply resultPath
                        |> Ast.Tuple
                        |> TypeRefRender.create false
                    TypeArguments = [||]
                }
                |> TypeAliasUnderlyingType.TypeReference
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Conditional conditionalKey ->
        let prerenderedUnion = ErasedUnionRender.prerenderFromConditional genCache conditionalKey
        {
            ShortCircuit = fun parentPath ->
                prerenderedUnion parentPath
                |> ErasedUnionRender.toWidget genCache
                |> funApply parentPath
                |> TypeRefRender.create isNullable
            Full = ValueSome (lazy
                {
                    TypeReferenceRender.Type =
                        prerenderedUnion resultPath
                        |> ErasedUnionRender.toWidget genCache
                        |> funApply resultPath
                        |> TypeRefRender.create isNullable
                    TypeArguments = [||]
                }
                |> TypeAliasUnderlyingType.TypeReference
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.Literal literalKey ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                // TypeMaybePathedRender.Pathed {
                //     Path = resultPath
                //     Render = fun _ ->
                //         { UnionRender.Name = name
                //           Cases = Array.singleton(EnumCaseRender.prerenderFromLiteral literalKey) }
                //         |> EnumRender.toTypeRender
                // }
                { UnionRender.Name = name
                  Cases = Array.singleton(EnumCaseRender.prerenderFromLiteral literalKey) }
                |> TypeAliasUnderlyingType.Union
                |> makeMaybePathedRender
                )
        }
        // |> Default
    | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (EnumCaseKey.ParentEnumMasterKey enumKey)) ->
        {
            ShortCircuit = fun _ -> pathedShortCircuit
            Full = ValueSome (lazy
                {
                    TypeReferenceRender.Type =
                        GeneratorContext.getTypeRef genCache enumKey.Value resultPath
                    TypeArguments = [||]
                }
                |> TypeAliasUnderlyingType.TypeReference
                |> makeMaybePathedRender
                )
        }
    | MasterKey.KeyType.MemberKey memberKey ->
        {
            ShortCircuit = fun parentPath ->
                memberKey
                |> MemberRender.prerender genCache
                |> funApply parentPath
                |> MemberRender.toTypeRef genCache
                |> funApply parentPath
            Full = ValueNone
        }
    | MasterKey.MasterBuilder value ->
        value.Value
        |> sprintf "%A"
        |> failwith
        // |> Default
            
let toTypeDefn (genCache: GeneratorContext) (typeAlias: TypeAliasRender) = 
    let typeParameterFns =
        typeAlias.TypeParameters
        |> Array.map (TypeParameterRender.toTyparDecl genCache)
        |> fun arrFns typeAliasPath ->
            arrFns
            |> Array.mapApply typeAliasPath
            |> Array.toList
    let renderName =
        typeAlias.Name
        |> Name.Case.valueOrModified
    match typeAlias.UnderlyingType with
    // render alias ===========
    | TypeAliasUnderlyingType.Interface interfaceRender ->
        InterfaceRender.renderInterfaceTypeDefn genCache interfaceRender
    | TypeAliasUnderlyingType.Class classRender ->
        failwith "todo"
    | TypeAliasUnderlyingType.TypeReference typeReferenceRender ->
        fun typeAliasPath ->
            Ast.Abbrev(
                renderName,
                TypeReferenceRender.toWidget
                    genCache
                    typeReferenceRender
                    typeAliasPath
                )
            |> Utils.TypeDefn.withTyparsIfNotEmpty ( typeParameterFns typeAliasPath )
    // render enum
    | TypeAliasUnderlyingType.Union unionRender when unionRender.Cases |> Array.forall _.IsEnum ->
        fun typeAliasPath ->
            Ast.Enum(renderName) {
                for case in unionRender.Cases do
                    EnumCaseRender.renderToEnum case
            }
            |> Utils.TypeDefn.withTyparsIfNotEmpty ( typeParameterFns typeAliasPath )
    // render string lit
    | TypeAliasUnderlyingType.Union unionRender ->
        fun typeAliasPath ->
        Ast.Union(renderName) {
            for case in unionRender.Cases do
                EnumCaseRender.renderToUnion case
        }
        |> Utils.TypeDefn.attributesIfNotEmpty (attributes {
            Attributes.requireQualifiedAccess
            Attributes.stringEnum
        })
        |> Utils.TypeDefn.withTyparsIfNotEmpty ( typeParameterFns typeAliasPath )
    //=========================
    // render interface
    | TypeAliasUnderlyingType.TypeLiteral { Members = members } ->
        let memberFns =
            members
            |> Array.map (MemberRender.renderAbstract genCache)
        fun typeAliasPath ->
            Ast.TypeDefn(Name.Case.valueOrModified typeAlias.Name) {
                yield!
                    memberFns
                    |> Array.mapApply typeAliasPath
            }
            |> Utils.TypeDefn.attributesIfNotEmpty (attributes {
                Attributes.allowNullLiteral
                Attributes.``interface``
            })
            |> Utils.TypeDefn.withTyparsIfNotEmpty ( typeParameterFns typeAliasPath )
    // render delegate?
    // render interface with invoke?
    | TypeAliasUnderlyingType.CallSignature callSignatureRender ->
        fun typeAliasPath ->
            Ast.TypeDefn(renderName) {
                CallSignatureRender.renderAbstract genCache callSignatureRender typeAliasPath
            }
            |> Utils.TypeDefn.attributesIfNotEmpty (attributes {
                Attributes.allowNullLiteral
                Attributes.``interface``
            })
            |> Utils.TypeDefn.withTyparsIfNotEmpty ( typeParameterFns typeAliasPath )

    