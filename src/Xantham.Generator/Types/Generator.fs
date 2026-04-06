namespace Xantham.Generator

open System.Collections.Generic
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder

(*
== MEMBERS ==
Member types are simple to render
== TYPE PARAMETERS ==
Type parameters with constraints can be turned into concrete type parameters
such as
'a when 'a :> IComparable
can be made into
type A<'A when 'A:>IComparable> = 'A
and now references to the type parameter can be constrained simple as 'A when A<'A>
== LITERALS ==
Combination of literals should be rendered into unions
They should be reduced to enums if they are all digits
A singular literal should be rendered as the underlying type for the moment
== ENUMS ==
Enums should be rendered as unions or enums.
Subsets of enums should be rendered as separate enums.
Converters should be added which can produce the optional subset value or the original set.
Supersets of enums should be rendered as enums with subsets inlined.
Converters should be added which can produce the optional subset value.

Subsets of unions should be rendered as unions.
Converters should also be added
== IndexAccess ==
An index access which has a generic index or object type should be rendered
as a PropertyAccess.
== Index ==
An index type should be rendered as a propertyaccess/indexof
== Interface ==
Should be rendered as interfaces with the 'I' prefix, and as pojos. All references
to the interface should use the 'I' prefix. The pojos will have all the members inlined.
== Class ==
Should be rendered as classes with a private unit constructor, and all constructors should
be overloads/augments instead.
== Conditional ==
Should render an erased union of the true and false branch.
References to their members should be resolved as the intersection of their members.
== Tuples ==
If they are fixed tuples with no variadic elements, then they should be rendered as a tuple.
If they have variadic elements, then they should be rendered as an array of an erased union.
== TypeLiteral ==
Render interface and pojo
== Module ==
Render as a module/namespace
== Variable ==
Render as a let binding
*)

/// <summary>
/// A builder for <c>WidgetBuilder&lt;Type&gt;</c>s that represent erased unions in Fable.
/// It tracks the lengths of the unions it builds to ensure that we have all erased unions defined in the base set,
/// otherwise we can generate our own.
/// </summary>
type UnionBuilder(maxLengthExisting: int) =
    let unionLengths = HashSet<int>()
    member _.Yield(value: WidgetBuilder<Type>) = [ value ]
    member _.Yield(value: string) = [ Ast.Anon value ]
    member _.YieldFrom(value: WidgetBuilder<Type> list) = value
    member _.YieldFrom(value: string list) = value |> List.map Ast.Anon
    member _.Combine(l: WidgetBuilder<Type> list, r: WidgetBuilder<Type> list) = l @ r
    member _.Delay(f: unit -> WidgetBuilder<Type> list) = f()
    member _.Run(state: WidgetBuilder<Type> list) =
        let length = state |> List.length
        match state with
        | [] -> Ast.Unit()
        | [ value ] -> value
        | values ->
            let prefixType = Ast.Anon $"U{length}"
            if length > maxLengthExisting then
                unionLengths.Add length |> ignore
            Ast.AppPrefix(prefixType, values)
    member this.UnionLengths = unionLengths

[<AutoOpen>]
module UnionBuilder =
    /// <summary>
    /// Erased union builder to build <c>WidgetBuilder&lt;Type&gt;</c>s that represent erased unions in Fable.
    /// It tracks the lengths of the unions it builds to ensure that we have all erased unions defined in the base set,
    /// otherwise we can generate our own.
    /// </summary>
    let erasedUnion = UnionBuilder(8)
type Types =
    /// <summary><c>Browser.Dom.Window</c></summary>
    static member globalThis = Ast.LongIdent [ "Browser"; "Dom"; "Window" ]
    /// <summary><c>bool</c></summary>
    static member bool = Ast.Boolean()
    /// <summary><c>string</c></summary>
    static member string = Ast.String()
    /// <summary><c>unit</c></summary>
    static member unit = Ast.Unit()
    /// <summary><c>int</c></summary>
    static member int = Ast.Int()
    /// <summary><c>float</c></summary>
    static member float = Ast.Float()
    /// <summary><c>bigint</c></summary>
    static member bigint = Ast.Anon "bigint"
    /// <summary><c>obj</c></summary>
    static member obj = Ast.Obj()
    /// <summary><c>char</c></summary>
    static member char = Ast.Char()
    /// <summary><c>obj | null</c></summary>
    static member objNull = Ast.Obj() |> Ast.TypeOrNull
    /// <summary><c>Array&lt;[Type]></c></summary>
    static member array: WidgetBuilder<Type> -> WidgetBuilder<Type> = Ast.Array
    /// <summary><c>option&lt;[Type]></c></summary>
    static member option: WidgetBuilder<Type> -> WidgetBuilder<Type> = Ast.OptionPrefix
    /// <summary><c>U#&lt;T1, T2, ..., T#></c></summary>
    static member union (types: WidgetBuilder<Type> seq) = erasedUnion { yield! Seq.toList types }
    /// <summary><c>PropertyRecord</c></summary>
    static member recordType = Ast.Anon "PropertyRecord"
    /// <summary><c>PropertyRecord&lt;[PropertyType], [ResultType]></c></summary>
    static member record (propertyType: WidgetBuilder<Type>) (resultType: WidgetBuilder<Type>) =
        Ast.AppPrefix(Types.recordType, [ propertyType; resultType ])
    /// <summary><c>keyof</c></summary>
    /// <remarks>Backed by a string in JS. Provides static typing for F#</remarks>
    static member keyofType = Ast.Anon "keyof"
    /// <summary><c>keyof&lt;[value]></c></summary>
    /// <remarks>Backed by a string in JS. Provides static typing for F#</remarks>
    static member keyof (value: WidgetBuilder<Type>) = Ast.AppPrefix(Types.keyofType, [ value ])
    /// <summary><c>typekeyof</c></summary>
    /// <remarks>Backed by a string in JS. Provides static typing for F#</remarks>
    static member typekeyofType = Ast.Anon "typekeyof"
    /// <summary><c>typekeyof&lt;[object], [returnType]></c></summary>
    /// <remarks>Backed by a string in JS. Provides static typing for F#</remarks>
    static member typekeyof (returnType: WidgetBuilder<Type>) (object: WidgetBuilder<Type>) = Ast.AppPrefix(Types.typekeyofType, [ object; returnType ])
    static member proptypelockType = Ast.Anon "proptypelock"
    static member proptypelock (lockedType: WidgetBuilder<Type>) = Ast.AppPrefix(Types.proptypelockType, [ lockedType ])
    static member proptypekeyType = Ast.Anon "proptypekey"
    static member proptypekey (returnType: WidgetBuilder<Type>) (lockedType: WidgetBuilder<Type>) = Ast.AppPrefix(Types.proptypekeyType, [ lockedType; returnType ])

type Attributes =
    /// <summary>
    /// <c>AutoOpen</c>
    /// </summary>
    static member autoOpen = Ast.Attribute("AutoOpen")
    /// <summary><c>CompiledName("[value]")</c></summary>
    /// <param name="value"></param>
    static member compiledName(value: string) = Ast.Attribute("CompiledName", Ast.ParenExpr(Ast.String value))
    /// <summary><c>CompiledValue([value])</c></summary>
    /// <param name="value"></param>
    static member compiledValue(value: int) = Ast.Attribute("CompiledValue", Ast.ParenExpr(Ast.Int value))
    /// <summary><c>CompiledValue([value])</c></summary>
    /// <param name="value"></param>
    static member compiledValue(value: float) = Ast.Attribute("CompiledValue", Ast.ParenExpr(Ast.Float value))
    /// <summary><c>CompiledValue([value])</c></summary>
    /// <param name="value"></param>
    static member compiledValue(value: bool) = Ast.Attribute("CompiledValue", Ast.ParenExpr(Ast.Constant(if value then "true" else "false")))
    /// <summary><c>RequireQualifiedAccess</c></summary>
    static member requireQualifiedAccess = Ast.Attribute("RequireQualifiedAccess")
    /// <summary><c>Interface</c></summary>
    static member ``interface`` = Ast.Attribute("Interface")
    /// <summary><c>Class</c></summary>
    static member ``class`` = Ast.Attribute("Class")
    /// <summary><c>AllowNullLiteral</c></summary>
    static member allowNullLiteral = Ast.Attribute("AllowNullLiteral")
    /// <summary><c>Obsolete("[message]")</c></summary>
    /// <param name="message"></param>
    static member obsolete (?message: string) = Ast.Attribute("Obsolete", Ast.ParenExpr(Ast.String(message |> Option.defaultValue "")))
    /// <summary><c>StringEnum(CaseRules.None)</c></summary>
    static member stringEnum = Ast.Attribute("StringEnum", Ast.ParenExpr("CaseRules.None"))
    /// <summary><c>Erase</c></summary>
    static member erase = Ast.Attribute("Erase")
    /// <summary><c>Emit( "[value]" )</c></summary>
    /// <param name="value"></param>
    static member emit (value: string) = Ast.Attribute("Emit", Ast.ParenExpr(Ast.String value))
    /// <summary><c>EmitConstructor</c></summary>
    static member emitConstructor = Ast.Attribute("EmitConstructor")
    /// <summary><c>EmitIndexer</c></summary>
    static member emitIndexer = Ast.Attribute("EmitIndexer")
    /// <summary><c>EmitProperty( "[propertyName]" )</c></summary>
    /// <param name="value"></param>
    static member emitProperty (value: string) = Ast.Attribute("EmitProperty", Ast.ParenExpr(Ast.String value))
    /// <summary><c>Import( "[libName]", "[libMember]" )</c></summary>
    /// <param name="libName"></param>
    /// <param name="libMember"></param>
    static member import (libName: string, libMember: string) = Ast.Attribute("Import", Ast.ParenExpr(Ast.TupleExpr [ Ast.String libName; Ast.String libMember ]))
    /// <summary><c>ImportMember( "[libName]" )</c></summary>
    /// <param name="libName"></param>
    static member importMember (libName: string) = Ast.Attribute("ImportMember", Ast.ParenExpr(Ast.String libName))
    /// <summary><c>JS.Pojo</c></summary>
    static member pojo = Ast.Attribute("JS.Pojo")
    /// <summary><c>ParamObject( [index] )</c></summary>
    /// <param name="index"></param>
    static member paramObject(?index: int) =
        match index with
        | Some i when i > 0 ->
            Ast.Attribute("ParamObject", Ast.ParenExpr(Ast.Int i))
        | _ -> Ast.Attribute("ParamObject")
    /// <summary><c>ParamArray</c></summary>
    static member paramArray = Ast.Attribute("ParamArray")
    /// <summary><c>DefaultValue</c></summary>
    static member defaultValueAttribute = Ast.Attribute("DefaultValue")
    /// <summary><c>Global</c></summary>
    static member ``global`` = Ast.Attribute("Global")
    /// <summary><c>System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)</c></summary>
    static member editorHidden = Ast.Attribute("System.ComponentModel.EditorBrowsable", Ast.ParenExpr("System.ComponentModel.EditorBrowsableState.Never"))
    /// <summary><c>System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Advanced)</c></summary>
    static member editorAdvanced = Ast.Attribute("System.ComponentModel.EditorBrowsable", Ast.ParenExpr("System.ComponentModel.EditorBrowsableState.Advanced"))
    /// <summary><c>CLIMutable</c></summary>
    static member cliMutable = Ast.Attribute("CLIMutable")
type Exprs =
    /// <summary>
    /// <c>JS.undefined</c>
    /// </summary>
    static member jsUndefined = Ast.ConstantExpr("JS.undefined")
    /// <summary>
    /// <c>()</c>
    /// </summary>
    static member unit = Ast.UnitExpr()

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
// How do we need all the final types to be represented?
// paths, masterkey

(*

All types are referenced by a master key.
References are resolved via knowing the 'pattern' of the type, and whether
it renders a path, or a conjugate of its types.

*)

[<Struct; RequireQualifiedAccess>]
type TypeRefKind =
    /// <summary>
    /// The type reference resolves to a prebuilt widget.
    /// </summary>
    /// <param name="widget"></param>
    | Widget of widget: WidgetBuilder<Type>

            

[<Struct>]
type TypeRefRender =
    {
        Type: TypeRefKind
        /// <summary>
        /// Whether the type reference implies nullability somewhere along its path.
        /// This could be either to the type being a union with a nullable primitive such as <c>undefined</c>
        /// or because it is a type reference of an optional property on an interface.
        /// </summary>
        Nullable: bool
    }

[<Struct>]
type ParameterRender = {
    Name: Name<Case.camel>
    Type: TypeRefRender
    IsOptional: bool
    IsParamArray: bool
}

[<Struct; RequireQualifiedAccess>]
type ParameterRenderArray =
    | Standard of parameters: ParameterRender array
    | ParamObject of parameters: ParameterRender array * paramObjectRenders: ParameterRender array * paramObjectIndex: int
    
[<Struct>]
type ParameterArrayPrerenderParamObject<'T> = {
    ParamObjectPrerenders: 'T array
    Index: int
}


[<Struct>]
type ParameterArrayPrerenderOverloads<'T> = {
    Default: 'T array
    ParamObject: ParameterArrayPrerenderParamObject<'T> voption
    Optionless: int voption
}

[<Struct>]
type MethodRender = {
    IsStatic: bool
    IsOptional: bool
    Name: Name<Case.camel>
    Parameters: ParameterRenderArray
    ReturnType: TypeRefRender
}

[<Struct>]
type CallSignatureRender = {
    Parameters: ParameterRenderArray
    ReturnType: TypeRefRender
}

[<Struct>]
type ConstructSignatureRender = {
    Parameters: ParameterRenderArray
    ReturnType: TypeRefRender
}

[<Struct>]
type IndexSignatureRender = {
    ReturnType: TypeRefRender
    Parameters: ParameterRenderArray
    IsReadOnly: bool
}

[<Struct>]
type PropertyDefaultRender = {
    Name: Name<Case.camel>
    Type: TypeRefRender
    IsOptional: bool
    IsStatic: bool
    Accessor: TsAccessor
    IsPrivate: bool
}

[<Struct>]
type PropertyMethodRender = {
    Name: Name<Case.camel>
    CallSignatureRender: CallSignatureRender
    IsOptional: bool
    IsStatic: bool
    Accessor: TsAccessor
    IsPrivate: bool
}

type PropertyRender =
    | Default of PropertyDefaultRender
    | Method of PropertyMethodRender

[<Struct>]
type GetterSetterRender = {
    Name: Name<Case.camel>
    Type: TypeRefRender
    IsStatic: bool
    IsPrivate: bool
}

[<Struct; RequireQualifiedAccess>]
type GetSetRender =
    | Getter of GetterSetterRender
    | Setter of GetterSetterRender
    member inline private this.GetPrerender =
        match this with
        | Getter value
        | Setter value -> value
    member this.IsStatic = this.GetPrerender.IsStatic
    member this.IsPrivate = this.GetPrerender.IsPrivate
    member this.Name = this.GetPrerender.Name
    member this.Type = this.GetPrerender.Type

type MemberRender =
    | Property of PropertyRender
    | GetSet of GetSetRender
    | Method of MethodRender
    | IndexSignature of IndexSignatureRender
    | CallSignature of CallSignatureRender
    | ConstructSignature of ConstructSignatureRender


[<Struct>]
type UnionCaseRender = {
    Name: Name<Case.pascal>
    LiteralValue: TsLiteral
    IsEnum: bool
}

[<Struct>]
type UnionRender = {
    Name: Name<Case.pascal>
    Cases: UnionCaseRender array
}

[<Struct>]
type VariableRender = {
    Name: Name<Case.camel>
    Type: TypeRefRender
}

[<Struct>]
type TypeParameterRender = {
    Name: Name<Case.typar>
    Constraint: TypeRefRender voption
    Default: TypeRefRender voption
}

[<Struct>]
type TypeReferenceRender = {
    Type: TypeRefRender
    TypeArguments: TypeRefRender array
}

[<Struct>]
type InterfaceRender = {
    Name: Name<Case.pascal>
    Heritage: TypeReferenceRender array
    Members: MemberRender array
    TypeParameters: TypeParameterRender array
    Extensions: MemberRender array
}

[<Struct>]
type ConstructorRender = {
    Parameters: ParameterRenderArray
}

[<Struct>]
type ClassRender = {
    Name: Name<Case.pascal>
    Heritage: TypeReferenceRender array
    Members: MemberRender array
    Constructors: ConstructorRender array
    TypeParameters: TypeParameterRender array
    Extensions: MemberRender array
}


[<Struct>]
type ErasedUnionRender = {
    Types: TypeRefRender array
}

[<Struct>]
type TypeLiteralRender = {
    Members: MemberRender array
}

/// <summary>
/// If we can resolve an index access, we return the members that match the index,
/// and the type of the object that is being indexed.
/// </summary>
[<Struct>]
type IndexAccessResolved = {
    Member: MemberRender array
    Type: TypeRefRender
}

/// <summary>
/// If we can only resolve the object being indexed (guarantee it has members), we can either
/// return a union of the available types on the object, or we can return a proptypelock of
/// the object.
/// </summary>
[<Struct>]
type IndexAccessObjectResolved = {
    Object: TypeRefRender
}

/// <summary>
/// If we can only resolve the index, then we return the index reference, and the resolved string.
/// </summary>
[<Struct>]
type IndexAccessIndexResolved = {
    IndexType: TypeRefRender
    Index: string
}

/// <summary>
/// If we can't resolve the index access, then we return the unresolved type references.
/// </summary>
[<Struct>]
type IndexAccessRenderUnresolved = {
    Object: TypeRefRender
    Index: TypeRefRender
}

[<Struct>]
type IndexResolved = {
    Members: MemberRender array
    MemberLiterals: string array
}

[<Struct>]
type IndexUnresolved = {
    Object: TypeRefRender
}

type IndexRender =
    | Resolved of IndexResolved
    | Unresolved of IndexUnresolved

type IndexAccessRender =
    | Resolved of IndexAccessResolved
    | Object of IndexAccessObjectResolved
    | Index of IndexAccessIndexResolved
    | Unresolved of IndexAccessRenderUnresolved

[<Struct>]
type FunctionRender = {
    Name: Name<Case.camel>
    Parameters: ParameterRenderArray
    ReturnType: TypeRefRender
    TypeParameters: TypeParameterRender array
}

type TypeAliasUnderlyingType =
    | Union of UnionRender
    | TypeLiteral of TypeLiteralRender
    | CallSignature of CallSignatureRender
    | Interface of InterfaceRender
    | Class of ClassRender
    | TypeReference of TypeReferenceRender

[<Struct>]
type TypeAliasRender = {
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender array
    UnderlyingType: TypeAliasUnderlyingType
}

[<Struct>]
type TupleElementRender = {
    Name: Name<Case.camel> voption
    Type: TypeRefRender
    IsRest: bool
}

[<Struct>]
type FixedTupleRender = {
    Types: TupleElementRender array
}

[<Struct>]
type OptionalTupleRender = {
    Fixed: TupleElementRender array
    Optional: TupleElementRender array
}

type TupleRender =
    | Fixed of FixedTupleRender
    | Optional of OptionalTupleRender

type TransientRender =
    | Index of IndexRender
    | IndexAccess of IndexAccessRender

type NodeRender =
    | Parameter of ParameterRender
    | Member of MemberRender
    | UnionCase of UnionCaseRender
    | TupleElement of TupleElementRender
    
/// <summary>
/// Repr of end point types that can be rendered into
/// F# AST.<br/>
/// Does not need to represent the full type set of TypeScript.
/// Restricted to represent the <i>final, actualised</i> rendering
/// of the full type set to a F# counterpart.
/// </summary>
type TypeRender =
    | Interface of InterfaceRender
    | Class of ClassRender
    /// <summary>
    /// Denotes a DU with <c>StringEnum</c> attributet.
    /// </summary>
    | LiteralUnion of UnionRender
    /// <summary>
    /// Denotes an F#/dotnet <c>Enum</c> type.
    /// </summary>
    | Enum of UnionRender
    | Variable of VariableRender
    | Function of FunctionRender
    | TypeAlias of TypeAliasRender