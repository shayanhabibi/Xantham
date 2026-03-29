module Xantham.SimpleGenerator.Generator.EnumRender

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getEnumPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyEnum>) = fun enumParentPath ->
    genCache.pathResolver.Prerenderer enumParentPath (Enum.toMasterKey key)

let prerender (key: PatternContextHolder<KeyEnum>) =
    {
        Name =
            Enum.name key |> PatternContext.value |> Name.Pascal.create
        Cases =
            Enum.members key
            |> PatternContext.Array.cmap EnumCaseRender.prerender
            |> PatternContext.value
    }

type EnumFromUnionError =
    | ContainedNonLiteralCases of nonLiterals: MasterKey array * literalsEnum: UnionRender
    | NoLiteralCases of PatternContextHolder<KeyUnion>
    | IsBoolean
    | ContainedNulls of nullLikes: MasterKey array * literalsEnum: UnionRender
    | ContainedNonLiteralCasesAndNullCases of nonLiterals: MasterKey array * nullCases: MasterKey array * literalsEnum: UnionRender
    | ContainedEnumCases of enumCases: EnumCaseKey array * enums: EnumKey array * literalsEnum: UnionRender
    | ContainedEnumCasesAndNonLiteralCases of enumCases: EnumCaseKey array * enums: EnumKey array * nonLiterals: MasterKey array * literalsEnum: UnionRender
    | ContainedEnumCasesAndNullCases of enumCases: EnumCaseKey array * enums: EnumKey array * nullCases: MasterKey array * literalsEnum: UnionRender
    | ContainedEnumCasesAndNonLiteralCasesAndNullCases of enumCases: EnumCaseKey array * enums: EnumKey array * nonLiterals: MasterKey array * nullCases: MasterKey array * literalsEnum: UnionRender

let prerenderFromUnion (key: PatternContextHolder<KeyUnion>) =
    if key |> Union.isBoolean then fun _ -> Error IsBoolean
    elif
        key
        |> Union.types
        |> PatternContext.Array.cforall (function MasterKey.VisitationFlags.YieldsLiterals -> false | _ -> true)
    then
        let value =
            NoLiteralCases(key)
            |> Error
        fun _ -> value
    else
    let addNonLiteralKey (acc: Result<UnionRender, EnumFromUnionError>) (key: MasterKey) =
        match acc with
        | Ok unionRender ->
            EnumFromUnionError.ContainedNonLiteralCases([| key |], unionRender)
            |> Error
        | Error err ->
            match err with
            | EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, unionRender) ->
                EnumFromUnionError.ContainedNonLiteralCases(nonLiterals |> Array.appendOne key, unionRender)
                |> Error
            | EnumFromUnionError.ContainedNulls(nullLikes, unionRender) ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases([| key |], nullLikes, unionRender)
                |> Error
            | EnumFromUnionError.IsBoolean ->
                EnumFromUnionError.ContainedNonLiteralCases([| Prelude.Primitive.Master.booleanKey; key |], { Name = Name.Pascal.create ""; Cases = [||] })
                |> Error
            | EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, unionRender) ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals |> Array.appendOne key, nullCases, unionRender)
                |> Error
            | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, [| key |], literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals |> Array.appendOne key, literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, [| key |], nullCases, literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals |> Array.appendOne key, nullCases, literalsEnum)
                |> Error
            | EnumFromUnionError.NoLiteralCases _ -> failwith "Unreachable"
    let addNullLikeKey (acc: Result<UnionRender, EnumFromUnionError>) (key: MasterKey) =
        match acc with
        | Ok unionRender ->
            EnumFromUnionError.ContainedNulls([| key |], unionRender)
            |> Error
        | Error err ->
            match err with
            | EnumFromUnionError.ContainedNulls(nullLikes, unionRender) ->
                EnumFromUnionError.ContainedNulls(nullLikes |> Array.appendOne key, unionRender)
                |> Error
            | EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, unionRender) ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, [| key |], unionRender)
                |> Error
            | EnumFromUnionError.IsBoolean ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases([| Prelude.Primitive.Master.booleanKey |], [| key |], { Name = Name.Pascal.create ""; Cases = [||] })
                |> Error
            | EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, unionRender) ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases |> Array.appendOne key, unionRender)
                |> Error
            | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, [| key |], literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, [|key|], literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases |> Array.appendOne key, literalsEnum)
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) -> 
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases |> Array.appendOne key, literalsEnum)
                |> Error
            | EnumFromUnionError.NoLiteralCases _ -> failwith "Unreachable"
    let addEnumCaseKey (acc: Result<UnionRender, EnumFromUnionError>) (key: EnumCaseKey) =
        match acc with
        | Ok render ->
            EnumFromUnionError.ContainedEnumCases([| key |], [||], render)
            |> Error
        | Error err ->
            match err with
            | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCases(enumCases |> Array.appendOne key, enums, literalsEnum)
                |> Error
            | ContainedNonLiteralCases(nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases([| key |], [||], nonLiterals, literalsEnum)
                |> Error
            | IsBoolean ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases([| key |], [||], [| Prelude.Primitive.Master.booleanKey |], { Name = Name.Pascal.create ""; Cases = [||] })
                |> Error
            | ContainedNulls(nullLikes, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases([| key |], [||], nullLikes, literalsEnum)
                |> Error
            | ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases([| key |], [||], nonLiterals, nullCases, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases |> Array.appendOne key, enums, nonLiterals, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases |> Array.appendOne key, enums, nullCases, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases |> Array.appendOne key, enums, nonLiterals, nullCases, literalsEnum)
                |> Error
            | NoLiteralCases _ -> failwith "unreachable"
    let addEnumKey (acc: Result<UnionRender, EnumFromUnionError>) (key: EnumKey) =
        match acc with
        | Ok render ->
            EnumFromUnionError.ContainedEnumCases([||], [| key |], render)
            |> Error
        | Error err ->
            match err with
            | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCases(enumCases, enums |> Array.appendOne key, literalsEnum)
                |> Error
            | ContainedNonLiteralCases(nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases([||], [| key |], nonLiterals, literalsEnum)
                |> Error
            | IsBoolean ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases([||], [| key |], [| Prelude.Primitive.Master.booleanKey |], { Name = Name.Pascal.create ""; Cases = [||] })
                |> Error
            | ContainedNulls(nullLikes, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases([||], [| key |], nullLikes, literalsEnum)
                |> Error
            | ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases([||], [| key |], nonLiterals, nullCases, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums |> Array.appendOne key, nonLiterals, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums |> Array.appendOne key, nullCases, literalsEnum)
                |> Error
            | ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums |> Array.appendOne key, nonLiterals, nullCases, literalsEnum)
                |> Error
            | NoLiteralCases _ -> failwith "unreachable"
    let addRenderedCases (acc: Result<UnionRender, EnumFromUnionError>) (cases: UnionCaseRender array) =
        match acc with
        | Ok unionRender -> Ok { unionRender with Cases = unionRender.Cases |> Array.append cases }
        | Error err ->
            match err with
            | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCases(enumCases, enums, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, literalsEnum) ->
                EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.ContainedNulls(nullLikes, literalsEnum) ->
                EnumFromUnionError.ContainedNulls(nullLikes, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.IsBoolean ->
                EnumFromUnionError.ContainedNonLiteralCases([| Prelude.Primitive.Master.booleanKey |], { Name = Name.Pascal.create ""; Cases = cases })
                |> Error
            | EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, literalsEnum) ->
                EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, { literalsEnum with Cases = literalsEnum.Cases |> Array.append cases })
                |> Error
            | EnumFromUnionError.NoLiteralCases _ -> failwith "unreachable"
    key
    |> Union.types
    |> PatternContext.Array.cfold (fun (acc: Result<UnionRender, EnumFromUnionError>) -> function
        | MasterKey.VisitationFlags.HasMembers & Value masterKey ->
            addNonLiteralKey acc masterKey
        | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (Value caseKey)) ->
            addEnumCaseKey acc caseKey
        | MasterKey.KeyType.Enum (Enum.ToEnumKey (Value enumKey)) ->
            addEnumKey acc enumKey
        | MasterKey.IsNullish & Value nullishKey ->
            addNullLikeKey acc nullishKey
        | MasterKey.KeyType.Name (Name.ToLiteral literal)
        | MasterKey.KeyType.Literal literal ->
            literal
            |> EnumCaseRender.prerenderFromLiteral
            |> Array.singleton
            |> addRenderedCases acc
        | MasterKey.AssociatedLiterals literals ->
            literals
            |> PatternContext.Array.cmap EnumCaseRender.prerenderFromLiteralKey
            |> PatternContext.value
            |> addRenderedCases acc
        | Value masterKey ->
            addNonLiteralKey acc masterKey
        ) (Ok { UnionRender.Name = Name.Pascal.create ""; Cases = [||] })
    |> PatternContext.value
    |> fun unionRender (unionParentPath: KeyPathKind) ->
            let name =
                unionParentPath.Value.Qualifiers
                |> Array.tryLast
                |> Option.map (
                    PatternContext.prepare key.Context
                    >> NameKey.(|ToName|)
                    >> PatternContext.value
                    >> Name.Pascal.create
                )
                |> Option.defaultWith (fun () -> Name.Pascal.create "Literals")
            match unionRender with
            | Ok unionRender -> Ok { unionRender with Name = name }
            | Error err ->
                match err with
                | EnumFromUnionError.ContainedEnumCases(enumCases, enums, literalsEnum) ->
                    EnumFromUnionError.ContainedEnumCases(enumCases, enums, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, literalsEnum) ->
                    EnumFromUnionError.ContainedEnumCasesAndNonLiteralCases(enumCases, enums, nonLiterals, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, literalsEnum) ->
                    EnumFromUnionError.ContainedEnumCasesAndNullCases(enumCases, enums, nullCases, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, literalsEnum) ->
                    EnumFromUnionError.ContainedEnumCasesAndNonLiteralCasesAndNullCases(enumCases, enums, nonLiterals, nullCases, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, literalsEnum) ->
                    EnumFromUnionError.ContainedNonLiteralCases(nonLiterals, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.ContainedNulls(nullLikes, literalsEnum) ->
                    EnumFromUnionError.ContainedNulls(nullLikes, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.IsBoolean -> Error EnumFromUnionError.IsBoolean
                | EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, literalsEnum) ->
                    EnumFromUnionError.ContainedNonLiteralCasesAndNullCases(nonLiterals, nullCases, { literalsEnum with Name = name })
                    |> Error
                | EnumFromUnionError.NoLiteralCases _ -> failwith "unreachable"

let toTypeRender (prerender: UnionRender) =
    prerender
    |> if
        prerender.Cases
        |> Array.forall _.IsEnum
       then TypeRender.Enum
       else TypeRender.LiteralUnion
let isRenderedAsEnum (prerender: UnionRender) =
    prerender.Cases |> Array.forall _.IsEnum
let isRenderedAsLiteralUnion (prerender: UnionRender) =
    prerender.Cases |> Array.exists (_.IsEnum >> not)

let toTypeDefn (prerender: UnionRender) =
    let isEnum = isRenderedAsEnum prerender
    if isEnum then
        Ast.Enum(Name.Case.valueOrModified prerender.Name) {
            for case in prerender.Cases do
                Ast.EnumCase(
                    Name.Case.valueOrModified case.Name,
                    match case.LiteralValue with
                    | TsLiteral.String value -> Ast.String value
                    | TsLiteral.Int value -> Ast.Int value
                    | TsLiteral.Float value -> Ast.Float value
                    | TsLiteral.Bool value -> Ast.Bool value
                    | TsLiteral.BigInt value -> Ast.Constant(value.ToString())
                    | TsLiteral.Null -> Ast.Constant "null"
                )
        }
    else
        Ast.Union(Name.Case.valueOrModified prerender.Name) {
            for case in prerender.Cases do
                EnumCaseRender.renderToUnion case
        }
        |> _.attributes([
            Attributes.requireQualifiedAccess
            Attributes.stringEnum
        ])