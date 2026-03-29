module Xantham.SimpleGenerator.Generator.UnionRender

open Fabulous.AST
open Xantham.SimpleGenerator
open Xantham.Decoder
open Xantham.SimpleGenerator.Generator
open Xantham.SimpleGenerator.Patterns

let prerender (key: PatternContextHolder<KeyUnion>) =
    EnumRender.prerenderFromUnion key

let toRender (genCache: GeneratorContext) (isNullable: bool) (prerender: Result<UnionRender, EnumRender.EnumFromUnionError>) = fun (unionPath: KeyPathKind) ->
    match prerender with
    | Ok enumRender ->
        let enumRender = {
            enumRender with
                Name =
                    unionPath.Value
                    |> KeyPath.popQualifier
                    |> snd
                    |> ValueOption.map (KeyResolution.getNamePascalCase genCache.ctx)
                    |> ValueOption.defaultValue enumRender.Name
        }
        let render =
            if EnumRender.isRenderedAsEnum enumRender then
                TypeRender.Enum enumRender
            else TypeRender.LiteralUnion enumRender
        Render.create genCache isNullable unionPath <| fun _ ->
            unionPath,
            fun _ -> render
    | Error err ->
        match err with
        | EnumRender.IsBoolean ->
            Render.createShortOnly isNullable Types.bool
        | EnumRender.ContainedNulls (_, { Cases = [| case1; case2 |] }) when case1.LiteralValue.IsBool && case2.LiteralValue.IsBool ->
            Render.createShortOnly true Types.bool
        | EnumRender.ContainedNulls(_, literals) when EnumRender.isRenderedAsEnum literals ->
            Render.create genCache true unionPath <| fun _ ->
                unionPath, fun _ -> TypeRender.Enum literals
        | EnumRender.ContainedNulls(_, literals) ->
            Render.create genCache true unionPath <| fun _ ->
                unionPath, fun _ -> TypeRender.LiteralUnion literals
        | EnumRender.NoLiteralCases patternContextHolder ->
            ErasedUnionRender.prerenderFromUnion genCache patternContextHolder unionPath
            |> ErasedUnionRender.toWidget genCache
            |> funApply unionPath
            |> Render.createShortOnly isNullable
        | EnumRender.ContainedNonLiteralCases(_, literalEnums)
        | EnumRender.ContainedNonLiteralCasesAndNullCases(_, _, literalEnums) 
        | EnumRender.ContainedEnumCases(_, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNullCases(_, _, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNonLiteralCases(_, _, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, _, _, literalEnums) as case ->
            let needsLiteralTypeDefn =
                if Array.isEmpty literalEnums.Cases |> not then
                    if EnumRender.isRenderedAsEnum literalEnums then
                        TypeRender.Enum literalEnums
                    else TypeRender.LiteralUnion literalEnums
                    |> ValueSome
                else ValueNone
            let isNullable =
                isNullable
                || case.IsContainedNonLiteralCasesAndNullCases
                || case.IsContainedEnumCasesAndNonLiteralCasesAndNullCases
                || case.IsContainedEnumCasesAndNullCases
            let nonLiterals =
                match case with
                | EnumRender.ContainedNonLiteralCases(nonLiterals, _) 
                | EnumRender.ContainedNonLiteralCasesAndNullCases(nonLiterals, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCases(_, _, nonLiterals, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, nonLiterals, _, _) -> nonLiterals
                | _ -> [||]
            let enumCases, enumKeys =
                match case with
                | EnumRender.ContainedEnumCases(enumCases, enumKeys, _) 
                | EnumRender.ContainedEnumCasesAndNullCases(enumCases, enumKeys, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCases(enumCases, enumKeys, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enumKeys, _, _, _) -> enumCases, enumKeys
                | _ -> [||], [||]
            let groupedEnumCases =
                enumCases
                |> Array.groupBy (
                    PatternContext.prepare genCache.ctx
                    >> function EnumCaseKey.ParentEnum (Value enumKey) -> enumKey
                    )
            let enumKeyCaseSets =
                groupedEnumCases
                |> Array.fold (fun acc (keyEnum, cases) ->
                    let enumKey = KeyNodeHashing.EnumKey.create keyEnum
                    if keyEnum.Members |> Array.except cases |> Array.isEmpty then
                        (enumKey, [||]) :: acc
                    else (enumKey, cases) :: acc
                    ) []
                |> List.append (enumKeys |> Seq.map (fun enumKey -> (enumKey, [||])) |> Seq.toList)
            let actualEnumKeys =
                enumKeyCaseSets
                |> Seq.map (
                    fst
                    >> PatternContext.prepare genCache.ctx
                    >> PatternContext.cbind EnumKey.toMasterKey
                    >> PatternContext.value)
                |> Seq.distinct
                |> Seq.toList
            let typeRenderLiterals =
                if needsLiteralTypeDefn.IsSome then
                    let name = "Literals"
                    let nameKey = genCache.ctx.createNameKey name
                    let typePath =
                        unionPath.Value
                        |> KeyPath.appendQualifierKey nameKey
                        |> KeyPath.addMeasure
                        |> KeyPathKind.TransientType
                    let literalUnion =
                        TypeRender.LiteralUnion {
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
                else ValueNone
            let prerenderErasedUnion =
                List.toArray actualEnumKeys
                |> Array.append nonLiterals
                |> PatternContext.prepare genCache.ctx
                |> ErasedUnionRender.prerenderFromTypeArray genCache
            let typeCountOfErasedUnion =
                actualEnumKeys.Length + nonLiterals.Length
                + (if needsLiteralTypeDefn.IsSome then 1 else 0)
            let shortCircuitErasedUnion = fun parentPath ->
                prerenderErasedUnion parentPath
                |> function
                    | value when needsLiteralTypeDefn.IsSome ->
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
                    else TypeRefRender.create isNullable parentPath
                Full =
                    match needsLiteralTypeDefn with
                    | ValueNone -> ValueNone
                    | ValueSome value -> ValueSome (lazy
                        Pathed {
                            Path = unionPath
                            Render = fun _ -> value
                        }
                    )
            }

let toTypeRef (genCache: GeneratorContext) (prerender: Result<UnionRender, EnumRender.EnumFromUnionError>) = fun (unionPath: KeyPathKind) ->
    match prerender with
    | Ok _ ->
        // will be rendering a literal union; render the path
        TypeRefRender.createPath false unionPath
    | Error err ->
        match err with
        | EnumRender.IsBoolean ->  Prelude.TypeRefRenders.bool
        | EnumRender.ContainedNulls (_, { Cases = [| case1; case2 |] })  when case1.LiteralValue.IsBool && case2.LiteralValue.IsBool ->
            TypeRefRender.create true Types.bool
            // will be rendering a literal union, render path with nullability
        | EnumRender.ContainedNulls _ ->
            TypeRefRender.createPath true unionPath
        | EnumRender.NoLiteralCases patternContextHolder ->
            ErasedUnionRender.prerenderFromUnion genCache patternContextHolder unionPath
            |> ErasedUnionRender.toWidget genCache
            |> funApply unionPath
            |> TypeRefRender.create false
        | EnumRender.ContainedNonLiteralCases(_, literalEnums)
        | EnumRender.ContainedNonLiteralCasesAndNullCases(_, _, literalEnums) 
        | EnumRender.ContainedEnumCases(_, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNullCases(_, _, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNonLiteralCases(_, _, _, literalEnums) 
        | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, _, _, literalEnums) as case ->
            let needsLiteralTypeDefn = Array.isEmpty literalEnums.Cases |> not
            let isNullable =
                case.IsContainedNonLiteralCasesAndNullCases
                || case.IsContainedEnumCasesAndNonLiteralCasesAndNullCases
                || case.IsContainedEnumCasesAndNullCases
            let nonLiterals =
                match case with
                | EnumRender.ContainedNonLiteralCases(nonLiterals, _) 
                | EnumRender.ContainedNonLiteralCasesAndNullCases(nonLiterals, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCases(_, _, nonLiterals, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(_, _, nonLiterals, _, _) -> nonLiterals
                | _ -> [||]
            let enumCases, enumKeys =
                match case with
                | EnumRender.ContainedEnumCases(enumCases, enumKeys, _) 
                | EnumRender.ContainedEnumCasesAndNullCases(enumCases, enumKeys, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCases(enumCases, enumKeys, _, _) 
                | EnumRender.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enumKeys, _, _, _) -> enumCases, enumKeys
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
                    unionPath.Value
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
            if typeCountOfErasedUnion < 4
            then
                shortCircuitErasedUnion unionPath
            else TypeRefRender.create isNullable unionPath
        
        
    