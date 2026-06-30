[<AutoOpen>]
module Xantham.Generator.Generator.Render_Transient

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module Union =
    let private renderUnionLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase enumCase ->
            let path =
                Case.unboxMeasure enumCase.Name
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                LiteralCaseRender.Metadata = {
                    Path = path
                    Original = path
                    Source = enumCase.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome enumCase.FullyQualifiedName
                }
                Name = enumCase.Name |> ValueSome
                Value = enumCase.Value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal tsLiteral ->
            let path = TransientMemberPath.Anchored |> Path.create
            {
                LiteralCaseRender.Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name =
                    match tsLiteral with
                    | TsLiteral.String value -> value
                    | TsLiteral.Int value -> string value
                    | TsLiteral.Float value -> string value
                    | TsLiteral.Bool value -> string value
                    | TsLiteral.BigInt value -> string value
                    | TsLiteral.Null -> "null"
                    |> Name.Pascal.create
                    |> ValueSome
                Value = tsLiteral
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery typeQuery ->
            let value =
                match typeQuery.Type.Value with
                | ResolvedType.Literal tsLiteral -> tsLiteral
                | _ -> failwith "Cannot render union literal case for non literal type"
            let name =
                typeQuery.FullyQualifiedName
                |> List.last
                |> (_.Value >> Name.Pascal.create)
            let path =
                name
                |> Case.unboxMeasure
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueSome typeQuery.FullyQualifiedName
                }
                Name = ValueSome name
                Value = value
                Documentation = []
            }

    let private renderUnionLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = { Path = Path.create TransientTypePath.Anchored
                         Original = Path.create TransientTypePath.Anchored
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Prelude.Transient.LiteralUnionRender.Name = ValueNone
            Cases =
                literals
                |> List.map (renderUnionLiteralCase ctx scopeStore)
            Documentation = []
        }
    let private renderEnumLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
            let path =
                Case.unboxMeasure enumCase.Name
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata = { Path = path
                             Original = path
                             Source = enumCase.Source |> Option.toValueOption
                             FullyQualifiedName = ValueSome enumCase.FullyQualifiedName }
                Name = enumCase.Name |> ValueSome
                Value = value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Int value) ->
            let path = TransientMemberPath.Anchored |> Path.create
            {
                Metadata = { Path = path; Original = path
                             Source = ValueNone; FullyQualifiedName = ValueNone }
                Name = string value |> Name.Pascal.create |> ValueSome
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery { FullyQualifiedName = fqn; Type = Resolve (ResolvedType.Literal (TsLiteral.Int value))  } ->
            let name =
                fqn
                |> List.last
                |> _.Value
                |> Name.Pascal.create
            let path =
                name
                |> Case.unboxMeasure
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata =
                    RenderMetadata.createWithPath path
                    |> RenderMetadata.withFullyQualifiedName fqn
                Name = ValueSome name
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery _
        | ResolvedTypeLiteralLike.EnumCase _ 
        | ResolvedTypeLiteralLike.Literal _ -> failwith "Cannot render enum literal case for non int literal. This should be guarded against"

    let private renderEnumLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        let path = TransientTypePath.Anchored |> Path.create
        {
            Metadata = { Path = path; Original = path
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Name = ValueNone
            Cases =
                literals
                |> List.map (renderEnumLiteralCase ctx scopeStore)
            Documentation = []
        }
    
    let renderLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        if
            literals
            |> List.forall (function
                | ResolvedTypeLiteralLike.EnumCase { Value = TsLiteral.Int _ }
                | ResolvedTypeLiteralLike.TypeQuery { Type = Resolve (ResolvedType.Literal (TsLiteral.Int _)) }
                | ResolvedTypeLiteralLike.Literal (TsLiteral.Int _) -> true
                | _ -> false
                )
        then
            renderEnumLiterals ctx scopeStore literals
            |> Transient.TypeRender.EnumUnion
        else
            renderUnionLiterals ctx scopeStore literals
            |> Transient.TypeRender.StringUnion
module TemplateLiteral =
    // A TS template-literal type (e.g. `` `${number} second` ``) is a compile-time string
    // refinement with no distinct runtime representation in an erased binding — it erases to
    // `string`. Render it as a PATH-DERIVED type abbreviation (`type <name> = string`) with
    // `Name = ValueNone`, so the emitted def takes whatever name the anchor path supplies. This
    // is what lets a template-literal that is a UNION MEMBER of a type-alias body receive a
    // distinct positional `Case{i}` identity (grafted into the alias scope's TypeStore by
    // `caseLiteralRef` in Render.TypeAlias): the abbreviation def is emitted as `type Case{i} =
    // string` matching the union-member reference, instead of the old hard-named `TemplateLiteral`
    // TypeDefn (Value/Create) which — because its name was fixed — collapsed every member onto the
    // owner alias name and produced the illegal self-cyclic `U15<X, X, ...>` (FS0953).
    let render (_ctx: GeneratorContext) (scopeStore: RenderScopeStore) (_templateLiteral: TemplateLiteral) =
            // Nameless `Anchored` metadata path (matching the prelude's nameless template-literal
            // ref) so the def name is supplied entirely by the ANCHOR path. When this template
            // literal is grafted into a type-alias scope as a positional union member
            // (`caseLiteralRef` in Render.TypeAlias sets `TypeStore[member] <- ...Case{i}`), the
            // anchor path is `Case{i}` and the def emits as `type Case{i} = string` matching the
            // union-member reference. The old hard-coded `TemplateLiteral` Moored leaf overrode that
            // anchor, so every member collapsed onto the owner alias name and produced the illegal
            // self-cyclic `U15<X, X, ...>` (FS0953).
            let path = Path.create TransientTypePath.Anchored
            {
                TypeAliasRenderRef.Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name = ValueNone
                TypeParameters = []
                Documentation = []
                // A TS template-literal type (e.g. `` `${number} second` ``) is a compile-time
                // string refinement with no distinct runtime representation in an erased binding —
                // it erases to `string`. Emit the path-derived abbreviation `type <name> = string`.
                Type =
                    RenderScopeStore.TypeRefRender.create
                        scopeStore
                        (ResolvedType.Primitive TypeKindPrimitive.String)
                        false
                        Intrinsic.string
            }
            |> Transient.TypeAliasRender.Alias
            |> TypeAlias
module Members =
    let renderFromMembersAndFunctions (ctx: GeneratorContext) (scopeStore: RenderScopeStore) members functions =
        {
            Transient.TypeLikeRender.Metadata = { Path = Path.create TransientTypePath.Anchored; Original = Path.create TransientTypePath.Anchored
                                                  Source = ValueNone; FullyQualifiedName = ValueNone }
            Name = ValueNone
            TypeParameters = []
            Inheritance = []
            Members = members
            Functions = functions
            Constructors = []
            Documentation = []
        }
        |> Transient.TypeRender.TypeDefn
    let inline render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (members: Member list) =
        Member.partitionRender ctx scopeStore members
        ||> renderFromMembersAndFunctions ctx scopeStore

module Intersection =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (intersection: Intersection) =
        Member.collectAllRecursively (ResolvedType.Intersection intersection)
        |> Members.render ctx scopeStore

module TypeLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (typeLiteral: TypeLiteral) =
        typeLiteral.Members
        |> Members.render ctx scopeStore

module Literal =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: TsLiteral) =
        {
            Metadata =
                TransientTypePath.Anchored
                |> Path.create
                |> RenderMetadata.createWithPath
            LiteralUnionRender.Name = ValueNone
            Cases = [
                {
                    Metadata = 
                        TransientMemberPath.Anchored
                        |> Path.create
                        |> RenderMetadata.createWithPath
                    Name =
                        match literal with
                        | TsLiteral.String value -> value
                        | TsLiteral.Int value -> string value
                        | TsLiteral.Float value -> string value
                        | TsLiteral.Bool value -> string value
                        | TsLiteral.BigInt value -> string value
                        | TsLiteral.Null -> "null"
                        |> Name.Pascal.create
                        |> ValueSome
                    Value = literal
                    Documentation = []
                }
            ]
            Documentation = []
        }
        |> Transient.TypeRender.StringUnion

module EnumCase =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (enumCase: EnumCase) =
        {
            Metadata =
                TransientTypePath.Anchored
                |> Path.create
                |> RenderMetadata.createWithPath
                |> RenderMetadata.withFullyQualifiedName enumCase.FullyQualifiedName
                |> RenderMetadata.withSourceOption enumCase.Source
            Name = ValueSome enumCase.Name
            Cases = [
                let path = (Case.unboxMeasure enumCase.Name) |> TransientMemberPath.AnchoredAndMoored |> Path.create
                {
                    Metadata = { Path = path; Original = path
                                 Source = enumCase.Source |> Option.toValueOption
                                 FullyQualifiedName = ValueSome enumCase.FullyQualifiedName }
                    Name = ValueSome enumCase.Name
                    Value = enumCase.Value
                    Documentation = enumCase.Documentation
                }
            ]
            Documentation = enumCase.Documentation
        }
        |> Transient.TypeRender.StringUnion