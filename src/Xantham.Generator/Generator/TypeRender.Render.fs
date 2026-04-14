[<AutoOpen>]
module Xantham.Generator.Generator.TypeRender_Render

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.TypeRenders

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
        
module private Helpers =
    let renderWithPath (anchorPath: AnchorPath) (typeRef: TypeRefRender) =
        TypeRefRender.localiseAndAnchor anchorPath typeRef
        |> TypeRefRender.render

module TypedNameRender =
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// When being used as parameter patterns in members, you can use the `withOptional` parameter to
    /// render the name as `?name` if structure, or the typeref is optional.
    /// </summary>
    /// <param name="withOptional">Whether to remap any nullability in the type or render to the parameter name.</param>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let private renderAsPatternImpl withOptional (ctx: GeneratorContext) (typedName: TypedNameRender) (anchorPath: AnchorPath) =
        let isOptional =
            TypedNameTraits.isOptional typedName.Traits
            || typedName.Type.Nullable
        let typeWidget =
            if withOptional then
                TypeRefRender.nonNullable typedName.Type 
            else typedName.Type |> TypeRefRender.setNullable isOptional
            |> Helpers.renderWithPath anchorPath
        let name =
            Name.Case.valueOrModified typedName.Name
            |> if withOptional && isOptional then
                   sprintf "?%s"
               else id
        Ast.ParameterPat( name, typeWidget )
        |> _.attributes(attributes {
            paramArray (TypedNameTraits.isParamArray typedName.Traits)
        })
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// Will route any nullability in the typed name render or the type ref to the parameter name.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsPatternWithOptionName (ctx: GeneratorContext) (typedName: TypedNameRender) (anchorPath: AnchorPath) =
        renderAsPatternImpl true ctx typedName anchorPath
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// Any optionality in the type ref or the type name render will result in the type being wrapped in option.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsPattern (ctx: GeneratorContext) (typedName: TypedNameRender) (anchorPath: AnchorPath) =
        renderAsPatternImpl false ctx typedName anchorPath
        
    /// <summary>
    /// Renders a typed name as a type signature (ie, a named type parameter in a delegate or abstract member).
    /// You can enable or disable the rendering of the optionality in the type, or in the name instead using
    /// the <c>withOption</c> switch
    /// </summary>
    /// <param name="withOption"></param>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsNamedTypeImpl withOption (ctx: GeneratorContext) (typedName: TypedNameRender) (anchorPath: AnchorPath) =
        let typeWidget =
            if withOption then
                TypeRefRender.nonNullable typedName.Type
            else typedName.Type |> TypeRefRender.orNullable (TypedNameTraits.isOptional typedName.Traits)
            |> Helpers.renderWithPath anchorPath
        let name =
            Name.Case.valueOrModified typedName.Name
            |> if withOption && (TypedNameTraits.isOptional typedName.Traits || typedName.Type.Nullable) then
                   sprintf "?%s"
               else id
        Ast.SignatureParameter(name, typeWidget).attributes(attributes {
            paramArray (TypedNameTraits.isParamArray typedName.Traits)
        })
    /// <summary>
    /// Render a typed name render as a type signature, with the option name rendered instead of wrapping the type
    /// with option.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsNamedTypeWithOptionName (ctx: GeneratorContext) (typedName: TypedNameRender) (anchorPath: AnchorPath) =
        renderAsNamedTypeImpl true ctx typedName anchorPath
    /// <summary>
    /// Render a typed name render as a type signature, with the optionality being rendered on the type.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsNamedType (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        renderAsNamedTypeImpl false ctx typedName anchorPath
    
    // when extracting type
    let renderTypeOnly (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        Helpers.renderWithPath anchorPath typedName.Type
    // rendering properties/members as abstracts
    let renderAbstractImpl withOption (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath = 
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            if withOption then
                (TypedNameTraits.isOptional typedName.Traits, typedName.Type)
                ||> TypeRefRender.orNullable
                |> Helpers.renderWithPath anchorPath
            else
                TypeRefRender.nonNullable typedName.Type
                |> Helpers.renderWithPath anchorPath

        match TypedNameTraits.isReadable typedName.Traits, TypedNameTraits.isWritable typedName.Traits with
        | true, true ->
            Ast.AbstractMember(name, typeWidget, hasGetter = true, hasSetter = true)
        | true, false ->
            Ast.AbstractMember(name, typeWidget, hasGetter = true)
        | false, true ->
            Ast.AbstractMember(name, typeWidget, hasSetter = true)
        | false, false ->
            Ast.AbstractMember(name, typeWidget)
        |> _.attributes(attributes {
            compiledName typedName.Name
        })
        |> if TypedNameTraits.isStatic typedName.Traits then _.toStatic() else id
    let renderAbstract (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        renderAbstractImpl true ctx typedName anchorPath
    let renderAbstractNoOption (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        renderAbstractImpl false ctx typedName anchorPath
                
    // rendering properties/members as member definitions
    let renderMember (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (TypedNameTraits.isOptional typedName.Traits, typedName.Type)
            ||> TypeRefRender.orNullable
            |> Helpers.renderWithPath anchorPath
        Ast.Member(name, Exprs.jsUndefined, typeWidget)
        |> _.attributes(attributes {
            compiledNameOrErase typedName.Name
        })
        |> if TypedNameTraits.isStatic typedName.Traits then _.toStatic() else id
        |> if TypedNameTraits.isWritable typedName.Traits then _.toMutable() else id
    let renderValMember (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (TypedNameTraits.isOptional typedName.Traits, typedName.Type)
            ||> TypeRefRender.orNullable
            |> Helpers.renderWithPath anchorPath
        match TypedNameTraits.isReadable typedName.Traits, TypedNameTraits.isWritable typedName.Traits with
        | true, true ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasGetter = true, hasSetter = true)
        | true, false ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasGetter = true)
        | false, true ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasSetter = true)
        | false, false ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget)
        |> _.attributes(attributes {
            compiledNameOrErase typedName.Name
        })
        |> if TypedNameTraits.isStatic typedName.Traits then _.toStatic() else id
    // rendering properties/members/variables as let bindings
    let renderBinding (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (TypedNameTraits.isOptional typedName.Traits, typedName.Type)
            ||> TypeRefRender.orNullable
            |> Helpers.renderWithPath anchorPath
        Ast.Value(name, Exprs.jsUndefined, typeWidget).attributes(attributes {
            compiledNameOrErase typedName.Name
        })
        |> if TypedNameTraits.isWritable typedName.Traits then _.toMutable() else id
        

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
    
    