namespace Xantham.SimpleGenerator.Generator

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

type AttributesBuilder() =
    member inline _.Zero() = []
    member inline _.Delay(f: unit -> WidgetBuilder<AttributeNode> list) = f()
    member inline _.Yield(_: unit) = []
    member inline _.Yield(value: WidgetBuilder<AttributeNode>) = [ value ]
    member inline _.Yield(value: WidgetBuilder<AttributeNode> list) = value
    member inline _.Yield(value: WidgetBuilder<AttributeNode> voption) = value |> ValueOption.toList
    member inline _.Yield(value: WidgetBuilder<AttributeNode> option) = value |> Option.toList
    member inline _.YieldFrom(value: WidgetBuilder<AttributeNode> list) = value
    member inline _.Combine(l: WidgetBuilder<AttributeNode> list, r: WidgetBuilder<AttributeNode> list) = l @ r
    member inline _.Combine(l: WidgetBuilder<AttributeNode>, r: WidgetBuilder<AttributeNode> list) = l :: r
    member inline _.Combine(l: WidgetBuilder<AttributeNode> list, r: WidgetBuilder<AttributeNode>) = l @ [ r ]
    member inline _.For(state: WidgetBuilder<AttributeNode> list, [<InlineIfLambda>] f: unit -> WidgetBuilder<AttributeNode> list) = f() @ state
    static member inline MakeAttributeIfModified (value: Name<_>) ([<InlineIfLambda>] fn: string -> WidgetBuilder<AttributeNode>) state =
        if Name.Case.isModified value then
            fn (Name.Case.valueOrSource value) :: state
        else state
    static member inline MakeAttributeIfModifiedElse (value: Name<_>) ([<InlineIfLambda>] fn: string -> WidgetBuilder<AttributeNode>) (orElse: WidgetBuilder<AttributeNode>) =
        if Name.Case.isModified value then
            fn (Name.Case.valueOrSource value)
        else orElse
    /// <summary>
    /// Will add a <c>CompiledName</c> attribute with the given name source if it is modified.
    /// </summary>
    [<CustomOperation("compiledName")>]
    member inline _.CompiledName(state, value: Name<_>) =
        AttributesBuilder.MakeAttributeIfModified value Attributes.compiledName state
    [<CustomOperation("compiledNameOrErase")>]
    member inline _.CompiledNameOrErase(state, value: Name<_>) =
        AttributesBuilder.MakeAttributeIfModifiedElse value Attributes.compiledName Attributes.erase :: state
    [<CustomOperation("emitProperty")>]
    member inline _.EmitProperty(state, value: Name<_>) =
        AttributesBuilder.MakeAttributeIfModified value Attributes.emitProperty state
    [<CustomOperation("emitPropertyOrErase")>]
    member inline _.EmitPropertyErase(state, value: Name<_>) =
        AttributesBuilder.MakeAttributeIfModifiedElse value Attributes.emitProperty Attributes.erase :: state
        
    member inline _.Run(state: WidgetBuilder<AttributeNode> list) =
        state

[<AutoOpen>]
module AttributesBuilder =
    /// <summary>
    /// General purpose helper for collecting attributes without nonsense.
    /// Premade operations for compiledName which only print the attribute if
    /// the name passed is modified. Will automatically handle options/value options of
    /// attributes.
    /// </summary>
    /// <example>
    /// Instead of writing:
    /// <code lang="fsharp">
    /// let attributes = [
    ///     if Name.Case.isModified value.Name then
    ///         Name.valueOrSource value.Name
    ///         |> Attributes.compiledName
    ///     if attributeMaybe.IsSome then attributeMaybe.Value
    /// ]
    /// </code>
    /// You can write:
    /// <code lang="fsharp">
    /// let attributes = attributes {
    ///     compiledName value.Name
    ///     attributeMaybe
    /// }
    /// </code>
    /// </example>
    let attributes = AttributesBuilder()
    
module Prelude =
    type Renders =
        static member globalThis = Render.createShortOnly false Types.globalThis
        static member unit = Render.createShortOnly false Types.unit
        static member bool = Render.createShortOnly false Types.bool
        static member int = Render.createShortOnly false Types.int
        static member float = Render.createShortOnly false Types.float
        static member string = Render.createShortOnly false Types.string
        static member bigint = Render.createShortOnly false Types.bigint
        static member symbol = Render.createShortOnly false Types.obj
        static member obj = Render.createShortOnly false Types.obj
        static member char = Render.createShortOnly false Types.char
        static member objNull = Render.createShortOnly true Types.obj
        static member recordType = Render.createShortOnly false Types.recordType
        static member keyof = Render.createShortOnly false Types.keyofType
        static member typekeyof = Render.createShortOnly false Types.typekeyofType
        static member proptypekey = Render.createShortOnly false Types.proptypekeyType
        static member proptypelock = Render.createShortOnly false Types.proptypelockType
    type TypeRefRenders =
        static member bool = TypeRefRender.create false Types.bool
        static member keyof = TypeRefRender.create false Types.keyofType
        static member typekeyof = TypeRefRender.create false Types.typekeyofType
        static member record = TypeRefRender.create false Types.recordType
        static member unit = TypeRefRender.create false Types.unit
        static member proptypekey = TypeRefRender.create false Types.proptypekeyType
        static member proptypelock = TypeRefRender.create false Types.proptypelockType
    
    module TypeDefns =
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
                |> Name.pascalCase
            Ast.TypeDefn(Name.valueOrModified typeDefnName) {
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

module Utils =
    let inline renderDefaultMemberName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)>(value: ^T) =
        Name.Case.valueOrModified value.Name
        |> sprintf "_.%s"
    let inline renderDefaultMemberNameFromString (value: string) =
        $"_.%s{value}"
    let inline renderStaticMemberName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)>(value: ^T) =
        Name.Case.valueOrModified value.Name
    let inline renderStaticMemberNameFromString (value: string) = value
    let inline renderMemberName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>) and ^T:(member IsStatic: bool)>(value: ^T) =
        if value.IsStatic
        then renderStaticMemberName value
        else renderDefaultMemberName value
    let inline renderRequiredParameterName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)>(value: ^T) =
        Name.Case.valueOrModified value.Name
    let inline renderOptionalParameterName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)>(value: ^T) =
        Name.Case.valueOrModified value.Name
        |> sprintf "?%s"
    let inline renderParameterName<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>) and ^T:(member IsOptional: bool)>
        (value: ^T) =
        if value.IsOptional
        then renderOptionalParameterName value
        else renderRequiredParameterName value
        
    let inline
        attributesIfNotEmpty<^T, ^U
            when ^U:(static member attributes: WidgetBuilder<^T> -> WidgetBuilder<AttributeNode> seq -> WidgetBuilder<^T>)>
        (attributes: WidgetBuilder<AttributeNode> list)
        (widget: WidgetBuilder<^T>) =
        if List.isEmpty attributes
        then widget
        else 'U.attributes(widget, attributes)
    module Member =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<MemberDefn, MemberDefnModifiers> attributes
        let inline toStaticIfTrue (value: bool) (widget: WidgetBuilder<MemberDefn>) =
            if value then widget.toStatic() else widget
        let inline toPrivateIfTrue (value: bool) (widget: WidgetBuilder<MemberDefn>) =
            if value then widget.toPrivate() else widget
        let inline withTyparsIfNotEmpty (typars: WidgetBuilder<TyparDeclNode> list) (widget: WidgetBuilder<MemberDefn>) =
            if List.isEmpty typars then
                widget
            else widget.typeParams(Ast.PostfixList typars)
    module AbstractMember =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<MemberDefn, MemberDefnModifiers> attributes
        let inline toStaticIfTrue (value: bool) (widget: WidgetBuilder<MemberDefn>) =
            if value then widget.toStatic() else widget
    module TypeDefn =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<TypeDefn, TypeDefnModifiers> attributes
        let inline withTyparsIfNotEmpty (typars: WidgetBuilder<TyparDeclNode> list) (widget: WidgetBuilder<TypeDefn>) =
            if List.isEmpty typars then
                widget
            else widget.typeParams(Ast.PostfixList typars)
    module Pattern =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<Pattern, PatternModifiers> attributes
    module SignatureParameter =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<Type, SignatureParameterModifiers> attributes
    module UnionCaseNode =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<UnionCaseNode, UnionCaseModifiers> attributes
    module EnumCase =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<EnumCaseNode, EnumCaseModifiers> attributes
    module GetSet =
        let inline attributesIfNotEmpty attributes =
            attributesIfNotEmpty<PropertyGetSetBindingNode, PropertyGetSetBindingModifiers> attributes
    module Getter =
        let attributesIfNotEmpty = GetSet.attributesIfNotEmpty
    module Setter =
        let attributesIfNotEmpty = GetSet.attributesIfNotEmpty
    
    let inline renderMemberWithAttributes<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)>
        (isStatic: bool)
        (parameters: WidgetBuilder<Pattern> array)
        (returnType: WidgetBuilder<Type>)
        (attributes: WidgetBuilder<AttributeNode> seq)
        (namedValue: ^T) =
        let name =
            if not isStatic
            then renderDefaultMemberName namedValue
            else renderStaticMemberName namedValue
        let attributes = [
            if Name.Case.isModified namedValue.Name then
                Name.Case.valueOrSource namedValue.Name
                |> Attributes.compiledName
            yield! attributes
        ]
        Ast.Member(
            name,
            Ast.ParenPat(Ast.TuplePat(parameters)),
            Exprs.jsUndefined,
            returnType
            )
        |> Member.attributesIfNotEmpty attributes
        |> Member.toStaticIfTrue isStatic
    let inline renderMemberWithParamIdx
        (isStatic: bool)
        (idx: int)
        (parameters: WidgetBuilder<Pattern> array)
        (returnType: WidgetBuilder<Type>)
        (namedValue: ^T when ^T:(member Name: Name)) =
        renderMemberWithAttributes
            isStatic
            parameters
            returnType
            [ Attributes.paramObject idx ]
            namedValue
    let inline renderMemberWithParamIdxAndOverload
        (isStatic: bool)
        (idx: int)
        (parameters: WidgetBuilder<Pattern> array)
        (returnType: WidgetBuilder<Type>)
        (namedValue: ^T when ^T:(member Name: Name)) =
        [|
            renderMemberWithParamIdx isStatic idx parameters returnType namedValue
            renderMemberWithAttributes
                isStatic
                (Array.truncate idx parameters)
                returnType
                Seq.empty
                namedValue
        |]
    
module Patterns =
    /// <summary>
    /// SRTP Helper for <c>addPath</c> and <c>(|AddPath|)</c>
    /// </summary>
    type AddPathValueHelper =
        static member inline Create (path: KeyPathKind) = ValueSome path
        static member inline Create (path: KeyPathKind voption) = path
        
    let isOptionalUnion = function
        | MasterKey.KeyType.Union (Union.IsTypeOrNullable value) -> ValueSome value
        | _ -> ValueNone
    
    /// <summary>
    /// Adds to the path from the given master key value.
    /// The path must be one of <c>KeyPath</c> or <c>KeyPath voption</c>.
    /// If the path is <c>KeyPath voption</c> then the path will be initialized if it is <c>ValueNone</c>.
    /// Use <c>appendPath</c> for a SRTP-less version.
    /// </summary>
    /// <param name="path"></param>
    /// <param name="value"></param>
    let inline addPath (path: ^T) (value: PatternContextHolder<MasterKey>) =
        match
            ((^T or AddPathValueHelper):(static member Create: ^T -> KeyPath voption) path)
        with
        | ValueSome path ->
            value
            |> PatternContext.mapc (fun ctx key -> KeyPath.append ctx key path)
            |> PatternContext.value
        | ValueNone ->
            value
            |> PatternContext.mapc KeyPath.init
            |> PatternContext.value
    
    /// <summary>
    /// Adds to the path from the given master key value.
    /// </summary>
    /// <param name="path"></param>
    /// <param name="value"></param>
    let inline appendPath (path: KeyPath) (value: PatternContextHolder<MasterKey>) =
        value |> PatternContext.mapc (fun ctx key -> KeyPath.append ctx key path) |> PatternContext.value
    
    [<return: Struct>]
    let (|OptionalUnion|_|): PatternContextHolder<MasterKey> -> PatternContextHolder<MasterKey> voption = isOptionalUnion
    
    /// <summary>
    /// Initializes the path from the given master key value.
    /// </summary>
    /// <param name="value"></param>
    let (|InitPath|) (value: PatternContextHolder<MasterKey>) =
        value
        |> PatternContext.mapc KeyPath.init
        |> PatternContext.value
    
    /// <summary>
    /// Adds to the path from the given master key value.
    /// The path must be one of <c>KeyPath</c> or <c>KeyPath voption</c>.
    /// If the path is <c>KeyPath voption</c> then the path will be initialized if it is <c>ValueNone</c>.
    /// Use <c>AppendPath</c> for a SRTP-less version.
    /// </summary>
    /// <param name="path"></param>
    /// <param name="value"></param>
    let inline (|AddPath|) (path: 'T) (value: PatternContextHolder<MasterKey>) = addPath path value
    /// <summary>
    /// Adds to the path from the given master key value.
    /// </summary>
    /// <param name="path"></param>
    /// <param name="value"></param>
    let (|AppendPath|) (path: KeyPath) (value: PatternContextHolder<MasterKey>) = appendPath path value
        
