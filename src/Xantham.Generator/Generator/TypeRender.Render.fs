[<AutoOpen>]
module Xantham.Generator.Generator.TypeRender_Render

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Anchored
open Fantomas.Core.SyntaxOak

module Attributes =
    let inline renderAttributes<^Modifier, ^T
        when (^T or ^Modifier):(static member attributes: ^T * WidgetBuilder<AttributeNode> list -> ^T)
    > (attributes: WidgetBuilder<AttributeNode> list) widget =
        if List.isEmpty attributes
        then widget
        else ((^T or ^Modifier):(static member attributes: ^T * WidgetBuilder<AttributeNode> list -> ^T) widget, attributes)
    let inline renderAttributesForAbstractMember attributes =
        renderAttributes<MemberDefnModifiers, WidgetBuilder<MemberDefn>> attributes
    let inline renderAttributesForBinding attributes =
        renderAttributes<BindingNodeModifiers, WidgetBuilder<BindingNode>> attributes
    let inline renderAttributesForMember attributes =
        renderAttributes<MemberDefnModifiers, WidgetBuilder<MemberDefn>> attributes
    let inline renderAttributesForTypeDefn attributes =
        renderAttributes<TypeDefnModifiers, WidgetBuilder<TypeDefn>> attributes
    let inline renderAttributesForEnumCase attributes =
        renderAttributes<EnumCaseModifiers, WidgetBuilder<EnumCaseNode>> attributes
    let inline renderAttributesForUnionCase attributes =
        renderAttributes<UnionCaseModifiers, WidgetBuilder<UnionCaseNode>> attributes
    let inline renderAttributesForPattern attributes =
        renderAttributes<PatternModifiers, WidgetBuilder<Pattern>> attributes
    let inline renderAttributesForSignature attributes =
        renderAttributes<SignatureParameterModifiers, WidgetBuilder<Type>> attributes

module Documentation =
    let private sortTsComment = function
        | TsComment.Summary _ -> 0
        | TsComment.Remarks _ -> 1
        | TsComment.Example _ -> 2
        | TsComment.Param _ -> 3
        | TsComment.Deprecated _ -> 4
        | _ -> 5
    let private makeOpeningTagWithAttributesImpl tag (attributes: (string * string) list) =
        attributes
        |> List.fold (fun acc (attribute, value) ->
            acc + $" %s{attribute}=\"%s{value}\""
            ) $"<%s{tag}"
    let private makeOpeningTagWithAttributes tag attributes =
        makeOpeningTagWithAttributesImpl tag attributes
        |> sprintf "%s >"
    let private makeImmediateTagWithAttributes tag attributes =
        makeOpeningTagWithAttributesImpl tag attributes
        |> sprintf "%s />"
    let private makeOpeningTag = sprintf "<%s>"
    let private makeClosingTag = sprintf "</%s>"
    let private makeImmediateTag = sprintf "<%s />"
    let inline private wrapInTag (tag: string) (text: string list) =
        match text with
        | [] -> []
        | _ -> makeOpeningTag tag :: text @ [ makeClosingTag tag ]
    let inline private wrapInOrImmediateTag (tag: string) (text: string list) =
        match text with
        | [] -> [ makeImmediateTag tag ]
        | _ -> wrapInTag tag text
    let inline private wrapInTagWithAttributes tag attributes (text: string voption) =
        match text with
        | ValueNone -> [ makeImmediateTagWithAttributes tag attributes ]
        | ValueSome text ->
            [
                makeOpeningTagWithAttributes tag attributes
                text
                makeClosingTag tag
            ]
    let render (documentation: TsComment list) =
        documentation
        |> List.sortBy sortTsComment
        |> List.collect (function
            | TsComment.Summary summary -> wrapInTag "summary" summary
            | TsComment.Remarks remarks -> wrapInTag "remarks" [ remarks ]
            | TsComment.Example example -> wrapInTag "example" [ example ]
            | TsComment.Param(name, content) ->
                wrapInTagWithAttributes "param" [ "name", name ] (Option.toValueOption content)
            | TsComment.Deprecated (Some (Null | "") | None) -> [ makeImmediateTag "deprecated" ]
            | TsComment.Deprecated (Some value) -> wrapInTag "deprecated" [ value ]
            | _ -> []
            )
        |> function
            | [] -> ValueNone
            | docs ->
                docs
                |> Ast.XmlDocs
                |> ValueSome
    open Fantomas.Core.SyntaxOak
    let inline makeRendererHelper<
        ^WidgetBuilder, ^ModifierType, ^Container when
            ^Container:(member Documentation: TsComment list)
            and (^WidgetBuilder or ^ModifierType):(static member xmlDocs: ^WidgetBuilder * WidgetBuilder<XmlDocNode> -> ^WidgetBuilder)
        >
        (container: ^Container) (widgetBuilder: ^WidgetBuilder)=
        match render container.Documentation with
        | ValueNone -> widgetBuilder
        | ValueSome docs ->
            ((^WidgetBuilder or ^ModifierType):(static member xmlDocs: ^WidgetBuilder * WidgetBuilder<XmlDocNode> -> ^WidgetBuilder) widgetBuilder, docs)
    let renderForEnumCase =
        makeRendererHelper<WidgetBuilder<EnumCaseNode>, EnumCaseModifiers, LiteralCaseRender<int, _>>
    let renderForUnionCase =
        makeRendererHelper<WidgetBuilder<UnionCaseNode>, UnionCaseModifiers, LiteralCaseRender<TsLiteral, _>>
    let inline renderForTypeDefn<^Container when ^Container: (member Documentation: TsComment list)> =
        makeRendererHelper<WidgetBuilder<TypeDefn>, TypeDefnModifiers, ^Container>
    let renderForEnum =
        makeRendererHelper<WidgetBuilder<TypeDefn>, TypeDefnModifiers, LiteralUnionRender<int, _>>
    let renderForUnion =
        makeRendererHelper<WidgetBuilder<TypeDefn>, TypeDefnModifiers, LiteralUnionRender<TsLiteral, _>>
    let inline renderForAbstractMember<^Container when ^Container: (member Documentation: TsComment list)> =
        makeRendererHelper<WidgetBuilder<MemberDefn>, AbstractMemberModifiers, ^Container>
    let inline renderForMember<^Container when ^Container: (member Documentation: TsComment list)> =
        makeRendererHelper<WidgetBuilder<MemberDefn>, MemberDefnModifiers, ^Container>
    let inline renderForBinding<^Container when ^Container: (member Documentation: TsComment list)> =
        makeRendererHelper<WidgetBuilder<BindingNode>, BindingNodeModifiers, ^Container>
            

module LiteralCaseRender =
    let renderEnumCase (ctx: GeneratorContext) (enumCase: LiteralCaseRender<int, _>) =
        Ast.EnumCase(
            Name.Case.valueOrModified enumCase.Name,
            string enumCase.Value
            )
        |> Documentation.renderForEnumCase enumCase
    let renderUnionCase (ctx: GeneratorContext) (unionCase: LiteralCaseRender<TsLiteral, _>) =
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
        |> Attributes.renderAttributesForUnionCase (attributes { attribute })
        |> Documentation.renderForUnionCase unionCase

module LiteralUnionRender =
    let renderEnum (ctx: GeneratorContext) (enumType: LiteralUnionRender<int, _>) =
        Ast.Enum(Name.Case.valueOrModified enumType.Name) {
            for case in enumType.Cases do
                LiteralCaseRender.renderEnumCase ctx case
        }
        |> Documentation.renderForEnum enumType
    let renderUnion (ctx: GeneratorContext) (unionType: LiteralUnionRender<TsLiteral, _>) =
        Ast.Union(Name.Case.valueOrModified unionType.Name) {
            for case in unionType.Cases do
                LiteralCaseRender.renderUnionCase ctx case
        }
        |> Attributes.renderAttributesForTypeDefn (attributes {
            stringEnum
            requireQualifiedAccess
        })
        |> Documentation.renderForUnion unionType
        
    let renderWithPath (anchorPath: AnchorPath) (typeRef: Anchored.TypeRefRender) =
        TypeRefRender.Anchored.render typeRef

module TypedNameRender =
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// When being used as parameter patterns in members, you can use the `withOptional` parameter to
    /// render the name as `?name` if structure, or the typeref is optional.
    /// </summary>
    /// <param name="withOptional">Whether to remap any nullability in the type or render to the parameter name.</param>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    let private renderAsPatternImpl withOptional (ctx: GeneratorContext) (typedName: TypedNameRender) =
        let isOptional = typedName.Traits.Contains(RenderTraits.Optional) || typedName.Type.Nullable
        let typeWidget =
            if withOptional then
                TypeRefRender.nonNullable typedName.Type 
            else typedName.Type |> TypeRefRender.setNullable isOptional
            |> TypeRefRender.render
        let name =
            Name.Case.valueOrModified typedName.Name
            |> if withOptional && isOptional then
                   sprintf "?%s"
               else id
        Ast.ParameterPat( name, typeWidget )
        |> Attributes.renderAttributesForPattern (attributes {
            paramArray (typedName.Traits.Contains(RenderTraits.ParamArray))
        })
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// Will route any nullability in the typed name render or the type ref to the parameter name.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    let renderAsPatternWithOptionName (ctx: GeneratorContext) (typedName: TypedNameRender) =
        renderAsPatternImpl true ctx typedName
    /// <summary>
    /// Renders a typed name as a parameter pattern.
    /// Any optionality in the type ref or the type name render will result in the type being wrapped in option.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    let renderAsPattern (ctx: GeneratorContext) (typedName: TypedNameRender) =
        renderAsPatternImpl false ctx typedName 
    /// <summary>
    /// Renders a typed name as a type signature (ie, a named type parameter in a delegate or abstract member).
    /// You can enable or disable the rendering of the optionality in the type, or in the name instead using
    /// the <c>withOption</c> switch
    /// </summary>
    /// <param name="withOption"></param>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    /// <param name="anchorPath"></param>
    let renderAsNamedTypeImpl withOption (ctx: GeneratorContext) (typedName: TypedNameRender) =
        let typeWidget =
            if withOption then
                TypeRefRender.nonNullable typedName.Type
            else
                typedName.Type
                |> TypeRefRender.orNullable (typedName.Traits.Contains(RenderTraits.Optional) || typedName.Type.Nullable)
            |> TypeRefRender.render 
        let name =
            Name.Case.valueOrModified typedName.Name
            |> if withOption && (typedName.Traits.Contains(RenderTraits.Optional) || typedName.Type.Nullable) then
                   sprintf "?%s"
               else id
        Ast.SignatureParameter(name, typeWidget)
        |> Attributes.renderAttributesForSignature (attributes {
            paramArray (typedName.Traits.Contains(RenderTraits.ParamArray))
        })
    /// <summary>
    /// Render a typed name render as a type signature, with the option name rendered instead of wrapping the type
    /// with option.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    let renderAsNamedTypeWithOptionName (ctx: GeneratorContext) (typedName: TypedNameRender)  =
        renderAsNamedTypeImpl true ctx typedName 
    /// <summary>
    /// Render a typed name render as a type signature, with the optionality being rendered on the type.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="typedName"></param>
    let renderAsNamedType (ctx: GeneratorContext) (typedName: TypedNameRender) =
        renderAsNamedTypeImpl false ctx typedName 
    // when extracting type
    let renderTypeOnly (ctx: GeneratorContext) (typedName: TypedNameRender) = TypeRefRender.render typedName.Type
    // rendering properties/members as abstracts
    let renderAbstractImpl withOption (ctx: GeneratorContext) (typedName: TypedNameRender) = 
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            if withOption then
                (typedName.Traits.Contains(RenderTraits.Optional), typedName.Type)
                ||> TypeRefRender.orNullable
                |> TypeRefRender.render
            else
                TypeRefRender.nonNullable typedName.Type
                |> TypeRefRender.render

        match typedName.Traits.Contains(RenderTraits.Readable), typedName.Traits.Contains(RenderTraits.Writable) with
        | true, true ->
            Ast.AbstractMember(name, typeWidget, hasGetter = true, hasSetter = true)
        | true, false ->
            Ast.AbstractMember(name, typeWidget, hasGetter = true)
        | false, true ->
            Ast.AbstractMember(name, typeWidget, hasSetter = true)
        | false, false ->
            Ast.AbstractMember(name, typeWidget)
        |> Attributes.renderAttributesForAbstractMember (attributes {
            compiledName typedName.Name
        })
        |> if typedName.Traits.Contains(RenderTraits.Static) then _.toStatic() else id
        |> Documentation.renderForAbstractMember typedName
    let renderAbstract (ctx: GeneratorContext) (typedName: TypedNameRender) =
        renderAbstractImpl true ctx typedName 
    let renderAbstractNoOption (ctx: GeneratorContext) (typedName: TypedNameRender) =
        renderAbstractImpl false ctx typedName 
    // rendering properties/members as member definitions
    let renderMember (ctx: GeneratorContext) (typedName: TypedNameRender) =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (typedName.Traits.Contains(RenderTraits.Optional), typedName.Type)
            ||> TypeRefRender.orNullable
            |> TypeRefRender.render
        Ast.Member(name, Exprs.jsUndefined, typeWidget)
        |> Attributes.renderAttributesForMember (attributes {
            compiledNameOrErase typedName.Name
        })
        |> if typedName.Traits.Contains(RenderTraits.Static) then _.toStatic() else id
        |> if typedName.Traits.Contains(RenderTraits.Writable) then _.toMutable() else id
        |> Documentation.renderForMember typedName
    let renderValMember (ctx: GeneratorContext) (typedName: TypedNameRender) anchorPath =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (typedName.Traits.Contains(RenderTraits.Optional), typedName.Type)
            ||> TypeRefRender.orNullable
            |> TypeRefRender.render
        match typedName.Traits.Contains(RenderTraits.Readable), typedName.Traits.Contains(RenderTraits.Writable) with
        | true, true ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasGetter = true, hasSetter = true)
        | true, false ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasGetter = true)
        | false, true ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget, hasSetter = true)
        | false, false ->
            Ast.MemberVal(name, Exprs.jsUndefined, typeWidget)
        |> Attributes.renderAttributesForMember (attributes {
            compiledNameOrErase typedName.Name
        })
        |> Documentation.renderForMember typedName
        |> if typedName.Traits.Contains(RenderTraits.Static) then _.toStatic() else id
    // rendering properties/members/variables as let bindings
    let renderBinding (ctx: GeneratorContext) (typedName: TypedNameRender) =
        let name = Name.Case.valueOrModified typedName.Name
        let typeWidget =
            (typedName.Traits.Contains(RenderTraits.Optional), typedName.Type)
            ||> TypeRefRender.orNullable
            |> TypeRefRender.render
        Ast.Value(name, Exprs.jsUndefined, typeWidget)
        |> Attributes.renderAttributesForBinding (attributes {
            compiledNameOrErase typedName.Name
        })
        |> if typedName.Traits.Contains(RenderTraits.Writable) then _.toMutable() else id
        |> Documentation.renderForBinding typedName

module TypeParameterRender =
    // rendering constraints
    let renderConstraints (ctx: GeneratorContext) (typeParameter: TypeParameterRender) =
        match typeParameter with
        | { Constraint = ValueNone } -> ValueNone
        | { Constraint = ValueSome constrain } ->
            constrain
            |> TypeRefRender.nonNullable
            |> TypeRefRender.render
            |> ValueSome
    
    // rendering as typar decl node
    let renderTypeParameter (ctx: GeneratorContext) (typeParameter: TypeParameterRender) =
        let name = Name.Case.valueOrModified typeParameter.Name
        match renderConstraints ctx typeParameter with
        | ValueSome constrain ->
            Ast.TyparDecl(name, constrain)
        | ValueNone ->
            Ast.TyparDecl(name)
        
    // rendering as typar
    let renderType (ctx: GeneratorContext) (typeParameter: TypeParameterRender) =
        Name.Case.valueOrModified typeParameter.Name
        |> Ast.LongIdent

module FunctionLikeSignature =
    let renderAbstractWithName (ctx: GeneratorContext) (name: Name<_>) (functionLike: FunctionLikeSignature) =
        let renderName = Name.Case.valueOrModified name
        let parameters =
            functionLike.Parameters
            |> List.map (TypedNameRender.renderAsNamedTypeWithOptionName ctx)
            |> Ast.Tuple
        let returnType = functionLike.ReturnType |> TypeRefRender.render
        Ast.AbstractMember(renderName, [ parameters ], returnType)
        |> if functionLike.Traits.Contains(RenderTraits.Static) then _.toStatic() else id
        |> Documentation.renderForAbstractMember functionLike

    let renderMember (ctx: GeneratorContext) (name: Name<_>) (functionLike: FunctionLikeSignature) =
        let renderName = Name.Case.valueOrModified name
        let parameters =
            functionLike.Parameters
            |> List.map (TypedNameRender.renderAsPatternWithOptionName ctx)
            |> Ast.TuplePat
            |> Ast.ParenPat
        let returnType = functionLike.ReturnType |> TypeRefRender.render
        Ast.Member(renderName, parameters, Exprs.jsUndefined, returnType)
        |> if functionLike.Traits.Contains(RenderTraits.Static) then _.toStatic() else id
        |> Documentation.renderForMember functionLike
    
    let renderBinding (ctx: GeneratorContext) (name: Name<_>) (functionLike: FunctionLikeSignature) =
        let renderName = Name.Case.valueOrModified name
        let parameters =
            functionLike.Parameters
            |> List.map (TypedNameRender.renderAsPattern ctx >> Ast.ParenPat)
        Ast.Function(renderName, parameters, Exprs.jsUndefined, functionLike.ReturnType |> TypeRefRender.render)
        |> Documentation.renderForBinding functionLike
    
    let renderSignature (ctx: GeneratorContext) (functionLike: FunctionLikeSignature) =
        let parameters =
            functionLike.Parameters
            |> List.map (TypedNameRender.renderTypeOnly ctx)
        let returnType = functionLike.ReturnType |> TypeRefRender.render
        Ast.Funs(parameters, returnType)
    
    let renderDelegate (ctx: GeneratorContext) (name: Name<_>) (functionLike: FunctionLikeSignature) =
        let parameters =
            functionLike.Parameters
            |> List.map (TypedNameRender.renderAsNamedTypeWithOptionName ctx)
            |> Ast.Tuple
        let returnType = functionLike.ReturnType |> TypeRefRender.render
        let renderName = Name.Case.valueOrModified name
        Ast.Delegate(renderName, parameters, returnType)
        |> Documentation.renderForTypeDefn functionLike

module FunctionLikeRender =
    // rendering as abstract
    let renderAbstract (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        functionLike.Signatures
        |> List.map (FunctionLikeSignature.renderAbstractWithName ctx functionLike.Name)
        
    // rendering as member
    let renderMember (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        functionLike.Signatures
        |> List.map (FunctionLikeSignature.renderMember ctx functionLike.Name)
        
    // rendering as function/let binding
    let renderBinding (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        let signature =
            functionLike.Signatures
            |> List.head
        FunctionLikeSignature.renderBinding ctx functionLike.Name signature
        
    // rendering as function signature
    let renderSignature (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        functionLike.Signatures
        |> List.head
        |> FunctionLikeSignature.renderSignature ctx
    // rendering as delegate
    let renderDelegate (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        functionLike.Signatures
        |> List.head
        |> FunctionLikeSignature.renderDelegate ctx functionLike.Name
    // rendering as an extension (member)
    let renderExtension (ctx: GeneratorContext) (functionLike: FunctionLikeRender) =
        renderMember ctx functionLike

module TypeLikeRender =
    let renderAbstractConstructors (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
        let returnType = Name.Case.valueOrModified typeLike.Name |> Ast.LongIdent
        typeLike.Constructors
        |> List.map (fun parameters ->
            let parameters =
                parameters
                |> List.map (TypedNameRender.renderAsNamedTypeWithOptionName ctx)
                |> Ast.Tuple
            let signature = Ast.Funs(parameters, returnType)
            Ast.AbstractMember("Create", signature).attributes(attributes {
                emitConstructor
            })
            )
    let renderMemberConstructors (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
        let returnType = Name.Case.valueOrModified typeLike.Name |> Ast.LongIdent
        typeLike.Constructors
        |> List.map (fun parameters ->
            let parameters =
                parameters
                |> List.map (TypedNameRender.renderAsPatternWithOptionName ctx)
                |> Ast.TuplePat
                |> Ast.ParenPat
            Ast.Member("Create", parameters, Exprs.jsUndefined, returnType).attributes(attributes {
                emitConstructor
            })
            |> Documentation.renderForMember typeLike
            )
    let renderInheritance (ctx: GeneratorContext) (typeRefRender: TypeRefRender) =
        typeRefRender
        |> TypeRefRender.nonNullable
        |> TypeRefRender.render
        |> Ast.Inherit
    let renderConstructors (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
        typeLike.Constructors
        |> List.map (fun parameters ->
            Ast.Constructor(
                parameters
                |> List.map (TypedNameRender.renderAsPatternWithOptionName ctx)
                |> Ast.TuplePat,
                Exprs.jsUndefined
            )
            |> Documentation.renderForMember typeLike
            |> Attributes.renderAttributesForMember (attributes {
                emitConstructor
            })
            )
    // renders an interface
    let renderInterface (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
        let renderName = Name.Case.valueOrModified typeLike.Name
        let typeParameters =
            if List.isEmpty typeLike.TypeParameters then
                ValueNone
            else
            typeLike.TypeParameters
            |> List.map (TypeParameterRender.renderTypeParameter ctx)
            |> Ast.PostfixList
            |> ValueSome
        let members =
            typeLike.Members
            |> List.map (TypedNameRender.renderAbstract ctx)
        let functions =
            typeLike.Functions
            |> List.collect (FunctionLikeRender.renderAbstract ctx)
        let memberCollection = members @ functions
        let builder =
            if List.isEmpty memberCollection
            then Ast.InterfaceEnd(renderName)
            else Ast.TypeDefn(renderName)
        builder {
            yield! renderAbstractConstructors ctx typeLike
            yield!
                typeLike.Inheritance
                |> List.map (renderInheritance ctx)
            yield! memberCollection
        }
        |> Documentation.renderForTypeDefn typeLike
        |> if typeParameters.IsSome then _.typeParams(typeParameters.Value) else id
        
    // renders a class
    let renderClass (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
        let renderName =
            Name.Case.valueOrModified typeLike.Name
        let typeParameters =
            if List.isEmpty typeLike.TypeParameters then
                ValueNone
            else
                typeLike.TypeParameters
                |> List.map (TypeParameterRender.renderTypeParameter ctx)
                |> Ast.PostfixList
                |> ValueSome
        let members =
            typeLike.Members
            |> List.map (TypedNameRender.renderMember ctx)
        let functions =
            typeLike.Functions
            |> List.collect (FunctionLikeRender.renderMember ctx)
        let memberCollection = members @ functions
        let constructors = renderConstructors ctx typeLike
        let builder =
            match memberCollection, constructors with
            | [], [] -> Ast.ClassEnd(renderName, Ast.Constructor().toPrivate())
            | _, _ -> Ast.TypeDefn(renderName, Ast.Constructor().toPrivate())
        builder {
            yield! memberCollection
            yield! constructors
        }
        |> Documentation.renderForTypeDefn typeLike
        |> if typeParameters.IsSome then _.typeParams(typeParameters.Value) else id
    // renders as a tag (type without explicit constructor)
    let renderTag (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders as a record
    let renderRecord (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    // renders as an anonymous record
    let renderAnonymousRecord (ctx: GeneratorContext) (typeLike: TypeLikeRender) = ()
    
module SpecialRender =
    /// <summary>
    /// Renders a type definition for a SRTP bound <c>typekeyof</c> accessor.
    /// Given a member name like <c>ofValue</c>, the following type definition will be rendered:
    /// <code lang="fsharp">
    /// [&lt;Erase&gt;]
    /// type OfValueAccessor =
    ///     static member inline create&lt;^T, ^ReturnType when ^T:(member ofValue: ^ReturnType)&gt;(?object: ^T): typekeyof&lt;^T, ^ReturnType> = unbox "ofValue"
    ///     static member inline access&lt;^T, ^ReturnType when ^T:(member ofValue: ^ReturnType)&gt;(object: ^T): ^ReturnType = object.ofValue
    /// </code>
    /// </summary>
    /// <remarks>
    /// <para>Now we can observe usage. If we were to create an untyped ofValue <c>typekeyof</c> by using
    /// the unit method call, then the result type will be <c>typekeyof&lt;obj, obj></c></para>
    /// <para>Subsequent usage would instantly resolve the type to the object it is used on, but this would
    /// invalidate it for usage on other types with the same property.</para>
    /// <code lang="fsharp">
    /// type TestObject = {
    ///     ofValue: int
    /// }
    /// type TestObject2 = {
    ///     ofValue: string
    /// }
    /// let testObject = { TestObject.ofValue = 1 }
    /// let testObject2 = { TestObject2.ofValue = "1" }
    ///
    /// let ofValueAccessor = OfValueAccessor.create()
    /// <br/>
    /// // the line below will resolve ofValueAccessor to typekeyof&lt;TestObject, int>
    /// TypeKeyOf.access ofValueAccessor testObject
    /// 
    /// // The line before can no longer be used on testObject2, as it is of a different type
    /// // TypeKeyOf.access ofValueAccessor testObject2
    /// </code>
    /// <para>We can also alternatively use the SRTP static method to access the value directly, without restricting the
    /// method to a single object type.</para>
    /// <code lang="fsharp">
    /// // Both are valid and are resolved by the compiler correctly
    /// OfValueAccessor.access testObject // int
    /// OfValueAccessor.access testObject2 // string
    /// </code>
    /// </remarks>
    /// <param name="srtpMember"></param>
    let renderTypeKeyOfSRTP (srtpMember: Name) =
        let typeDefnName =
            srtpMember
            |> Name.map (sprintf "%sAccessor")
            |> Name.Pascal.fromName
        Ast.TypeDefn(Name.Case.valueOrModified typeDefnName) {
            Ast.Member(
                "create",
                [ Ast.ParenPat(Ast.ParameterPat("?object", Ast.LongIdent "^T")) ],
                $"unbox \"{Name.valueOrSource srtpMember}\"",
                Types.typekeyof (Ast.LongIdent "^ReturnType") (Ast.LongIdent "^T")
            )   .toStatic()
                .toInlined()
                .typeParams(Ast.PostfixList $"^T, ^ReturnType when ^T:(member %s{Name.valueOrModified srtpMember}: ^ReturnType)")
            
            Ast.Member(
                "access",
                [ Ast.ParenPat(Ast.ParameterPat("object", Ast.LongIdent "^T")) ],
                $"object.{Name.valueOrModified srtpMember}",
                Ast.LongIdent "^ReturnType"
            )   .toStatic()
                .toInlined()
                .typeParams(Ast.PostfixList $"^T, ^ReturnType when ^T:(member %s{Name.valueOrModified srtpMember}: ^ReturnType)")
        }
        |> _.attribute(Attributes.erase)
    
    module private ErasedUnionHelpers =
        [<Literal>]
        let private lowerBoundTypeParam = 'A'
        [<Literal>]
        let private upperBoundTypeParam = 'Z'
        [<Literal>]
        let private typeParameterCharRange = upperBoundTypeParam - lowerBoundTypeParam
        let inline typeParam (caseIdx: int) =
            let range = byte typeParameterCharRange + 1uy
            let repl = (caseIdx / int range) + 1
            byte caseIdx % range + byte lowerBoundTypeParam
            |> char
            |> Array.replicate repl
            |> Array.insertAt 0 '''
            |> System.String
        let inline caseName (caseIdx: int) = $"Case%i{caseIdx + 1}"

        let renderStaticErase (caseIdx: int) =
            Ast.Member("op_ErasedCast", [ $"x: {typeParam caseIdx}" ], $"{caseName caseIdx} x")
            |> _.toStatic()
            |> _.attribute(Attributes.emit "$0")
        let renderStaticImplicit (caseIdx: int) =
            Ast.Member("op_Implicit", [ $"x: {typeParam caseIdx}" ], $"{caseName caseIdx} x")
            |> _.toStatic()
            |> _.attribute(Attributes.emit "$0")
            
    let renderErasedUnion (caseCount: int) =
        let typeParameters =
            [0..caseCount - 1]
            |> List.map (ErasedUnionHelpers.typeParam >> Ast.TyparDecl)
            |> Ast.PostfixList
        let name = $"U{caseCount}"
        let documentation = Documentation.render [
            TsComment.Summary [
                $"Erased union type to represent 1 of {caseCount} possible values."
                "<a href=\"https://fable.io/docs/javascript/features.html#erased-unions\">Read more</a>"
            ]
        ]
        (Ast.Union(name) {
            for i in 0..caseCount - 1 do
                Ast.UnionCase(ErasedUnionHelpers.caseName i, Ast.LongIdent (ErasedUnionHelpers.typeParam i))
        }).members() {
            for i in 0..caseCount - 1 do
                ErasedUnionHelpers.renderStaticErase i
        }
        |> _.xmlDocs(documentation.Value)
        |> _.typeParams(typeParameters)
        |> _.attribute(Attributes.erase)
        
