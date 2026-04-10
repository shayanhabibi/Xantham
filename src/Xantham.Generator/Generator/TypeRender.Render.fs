[<AutoOpen>]
module Xantham.Generator.Generator.TypeRender_Render

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.TypeRefRender

module LiteralCaseRender =
    let renderEnumCase (ctx: GeneratorContext) (enumCase: LiteralCaseRender<int>) =
        Ast.EnumCase(
            Name.Case.valueOrModified enumCase.Name,
            string enumCase.Value
            )
    let renderUnionCase (ctx: GeneratorContext) (unionCase: LiteralCaseRender<TsLiteral>) =
        let attribute =
            match unionCase.Value with
            | TsLiteral.String value ->
                if value <> Name.Case.valueOrModified unionCase.Name
                then Some (Attributes.compiledName value)
                else None
            | TsLiteral.Int value ->
                Attributes.compiledValue value
                |> Some
            | TsLiteral.Float value ->
                Attributes.compiledValue value
                |> Some
            | TsLiteral.Bool value ->
                Attributes.compiledValue value
                |> Some
            | TsLiteral.BigInt value ->
                Attributes.compiledValue (int value)
                |> Some
            | TsLiteral.Null ->
                Attributes.compiledName null
                |> Some

        Ast.UnionCase(Name.Case.valueOrModified unionCase.Name)
            .attributes(attributes { attribute })

module LiteralUnionRender =
    let renderEnum (ctx: GeneratorContext) (enumType: LiteralUnionRender<int>) =
        Ast.Enum(Name.Case.valueOrModified enumType.Name) {
            for case in enumType.Cases do
                LiteralCaseRender.renderEnumCase ctx case
        }
    let renderUnion (ctx: GeneratorContext) (unionType: LiteralUnionRender<TsLiteral>) =
        Ast.Union(Name.Case.valueOrModified unionType.Name) {
            for case in unionType.Cases do
                LiteralCaseRender.renderUnionCase ctx case
        }
        |> _.attributes(attributes {
            stringEnum
            requireQualifiedAccess
        })

module TypedNameRender =
    // parameters
    let renderAsPattern (ctx: GeneratorContext) (typedName: TypedNameRender) = ()
    // parameters for abstracts/delegatets
    let renderAsNamedType (ctx: GeneratorContext) (typedName: TypedNameRender) = ()
    // when extracting type
    let renderTypeOnly (ctx: GeneratorContext) (typedName: TypedNameRender) = ()
    // rendering properties/members as abstracts
    let renderAbstract (ctx: GeneratorContext) (typedName: TypedNameRender) = ()
    // rendering properties/members as member definitions
    let renderMember (ctx: GeneratorContext) (typedName: TypedNameRender) = ()
    // rendering properties/members/variables as let bindings
    let renderBinding (ctx: GeneratorContext) (typedName: TypedNameRender) = ()

module TypeParameterRender =
    // rendering constraints
    let renderConstraints (ctx: GeneratorContext) (typeParameter: TypeParameterRender) = ()
    // rendering as typar decl node
    let renderTypeParameter (ctx: GeneratorContext) (typeParameter: TypeParameterRender) = ()
    // rendering as typar
    let renderType (ctx: GeneratorContext) (typeParameter: TypeParameterRender) = ()

module FunctionLikeRender =
    // rendering as abstract
    let renderAbstract (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()
    // rendering as member
    let renderMember (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()
    // rendering as function/let binding
    let renderBinding (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()
    // rendering as function signature
    let renderSignature (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()
    // rendering as delegate
    let renderDelegate (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()
    // rendering as an extension (member)
    let renderExtension (ctx: GeneratorContext) (functionLike: FunctionLikeRender) = ()

module TypeLikeRender =
    // renders an interface
    let renderInterface (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders a class
    let renderClass (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders as a tag (type without explicit constructor)
    let renderTag (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders as a record
    let renderRecord (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders as an anonymous record
    let renderAnonymousRecord (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    
    