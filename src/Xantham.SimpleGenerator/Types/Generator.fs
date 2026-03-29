namespace Xantham.SimpleGenerator

open System.Collections.Generic
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.SimpleGenerator.Patterns

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
    /// <summary>
    /// Reference the type by a path.
    /// </summary>
    | Path of KeyPathKind
    static member Create (widget: WidgetBuilder<Type>) = Widget widget
    static member Create (path: KeyPathKind) = Path path

    /// <summary>
    /// FOR SRTP
    /// </summary>
    static member inline Create (kind: TypeRefKind) = kind
    member this.toWidget (ctx: KeyResolutionContext) (refContext: KeyPathKind) =
        match this with
        | Widget widget -> widget
        | Path path ->
            KeyPath.localiser refContext path 
            |> KeyPath.createPathTypeRenderer ctx
    member this.toWidgetWithFn (fn: PathRenderingFunc) (refContext: KeyPathKind) =
        match this with
        | Widget widget -> widget
        | Path path ->
            fn (KeyPath.localiser refContext path)
            

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

type PathedRender = {
    Render: KeyPathKind -> TypeRender
    Path: KeyPathKind
}

type TypeMaybePathedRender =
    | Pathed of PathedRender
    | RefOnly of TypeRefRender

[<Struct>]
type Render = {
    /// <summary>
    /// Short circuits can be rendered immediately without accessing the GeneratorContext
    /// by returning a TypeRefRender which is calculated from the keyPath
    /// </summary>
    ShortCircuit: ComputeShortCircuitFunc
    /// <summary>
    /// Lazy evaluation is great, because it allows us to defer rendering, with the capacity
    /// to check whether it has been completed (at which point it can be inspected even when
    /// in contexts that wouldn't allow it.).
    /// </summary>
    Full: Lazy<TypeMaybePathedRender> voption
}
// and [<Struct>] RenderResult =
    // | Auxilliary of Render * auxilliary: Render
    // | Default of Render
/// <summary>
/// Rendering a type definition requires immediate knowledge of its position in the type/path hierarchy.
/// All of the type references from within the type definition are resolved relative to the type definition.
/// </summary>
and ComputeRenderFunc = KeyPathKind -> TypeRender
and ComputeShortCircuitFunc = KeyPathKind -> TypeRefRender

type SourceTree = {
    Branches: Dictionary<SourceKey, ModuleTree>
}
and ModuleTree = {
    Branches: Dictionary<NameKey, ModuleTree>
    Keys: HashSet<MasterKey>
}

module ModuleTree =
    let getModule ctx (path: KeyPathKind) (tree: ModuleTree) =
        // if path.IsConcrete then
            path.Value
            |> KeyPath.popQualifier
            |> fst
            |> _.Qualifiers
            |> Array.fold (fun tree (qualifier: NameKey) ->
                tree.Branches
                |> Dictionary.tryItem qualifier
                |> ValueOption.defaultWith (fun () ->
                    tree.Branches
                    |> Dictionary.tryAddOrGet qualifier { Branches = Dictionary(); Keys = HashSet() }
                    )
                ) tree
        // else
        
            
module SourceTree =
    let getModule ctx (path: KeyPathKind) (tree: SourceTree) =
        if path.IsConcrete then
            let source,sourcePath =
                path.Value
                |> KeyPath.popQualifier
                |> fst
                |> fun keyPath ->
                    let source =
                        keyPath.Source
                        |> ValueOption.defaultValue KeyNodeHashing.SourceKey.nullKey
                    let sourcePath =
                        keyPath
                        |> KeyPath.resolve ctx
                        |> fst |> ValueOption.map (List.map ctx.createNameKey)
                    (source, sourcePath)
            let moduleTree =
                tree.Branches
                |> Dictionary.tryItem source
                |> ValueOption.defaultWith (fun () ->
                    tree.Branches
                    |> Dictionary.tryAddOrGet
                           source
                           { Branches = Dictionary(); Keys = HashSet() }
                    )
            if sourcePath.IsSome then
                sourcePath.Value
                |> List.fold (fun tree key ->
                    tree.Branches
                    |> Dictionary.tryItem key
                    |> ValueOption.defaultWith (fun () ->
                        tree.Branches
                        |> Dictionary.tryAddOrGet key
                            { Branches = Dictionary(); Keys = HashSet() }
                        )
                    ) moduleTree
            else moduleTree
            |> ValueSome
        else
            ValueNone
            
            
        

type InterceptorStrategy =
    | MasterKeyPredicate of (GeneratorContext -> MasterKey -> bool)
    | NamePredicate of (string -> bool)
    | EsLibOnly of InterceptorStrategy
and InterceptorAction =
    | ChangeRender of (GeneratorContext -> Render -> Render)
    | Intercept of RenderFunc
and Interceptor = {
    Source: (string -> bool) voption
    Strategy: InterceptorStrategy
    Action: InterceptorAction
}
and [<Struct>] ComputedChangeInterceptor = {
    SourceKeySet: Set<SourceKey> voption
    Predicate: MasterKey -> bool
    Action: Render -> Render
}
and [<Struct>] ComputedInterceptInterceptor = {
    SourceKeySet: Set<SourceKey> voption
    Predicate: MasterKey -> bool
    Action: RenderFunc
}
and [<Struct>] ComputedInterceptors = {
    Change: ComputedChangeInterceptor array
    Intercept: ComputedInterceptInterceptor array
}
and Interceptors =
    | Precompute of Interceptor list
    | Computed of ComputedInterceptors
and RenderFunc = GeneratorContext -> MasterKey -> Render
and GeneratorContext = {
    /// <summary>
    /// The set of keys that have been seen in the current rendering context.
    /// </summary>
    seenKeys: HashSet<MasterKey>
    /// <summary>
    /// Related keys based on access patterns.
    /// </summary>
    keyDependencies: Dictionary<MasterKey, HashSet<MasterKey>>
    /// <summary>
    /// Observes the current rendering key; dependent keys are those that
    /// are accessed/rendered while rendering the current key. 
    /// </summary>
    renderingStack: Stack<MasterKey>
    interceptors: Interceptors
    /// <summary>
    /// Caches the type reference render function for each key.
    /// </summary>
    shortCircuitCache: Dictionary<MasterKey, ComputeShortCircuitFunc>
    /// <summary>
    /// Caches the finalised render function for each key.
    /// </summary>
    cache: Dictionary<MasterKey, TypeMaybePathedRender>
    auxilliary: Dictionary<MasterKey, TypeMaybePathedRender>
    renderPaths: Dictionary<MasterKey, HashSet<KeyPathKind>>
    /// <summary>
    /// The rendering key resolution context. Holds the cache that
    /// matches keys against their expanded structs.
    /// </summary>
    ctx: KeyResolutionContext
    /// <summary>
    /// The rendering function that is used to render a type key.
    /// </summary>
    renderFunc: RenderFunc
    /// <summary>
    /// A set of functions that are used to create and localise key paths.
    /// </summary>
    pathResolver: KeyPathContext
}

module ComputeRenderFunc =
    let inline create (fn: GeneratorContext -> KeyPathKind -> TypeRender) genCache: ComputeRenderFunc voption = ValueSome (fn genCache)
    let none: ComputeRenderFunc voption = ValueNone

module ComputeShortCircuitFunc =
    let inline create (fn: KeyPathKind -> TypeRefRender): ComputeShortCircuitFunc = fn

module Interceptor =
    let inline private createMasterKeyStrategyInterceptor (predicate: GeneratorContext -> MasterKey -> bool) (action: InterceptorAction) =
        {
            Source = ValueNone
            Strategy = MasterKeyPredicate predicate
            Action = action
        }
    let inline private createNameStrategyInterceptor (predicate: string -> bool) (action: InterceptorAction) =
        {
            Source = ValueNone
            Strategy = NamePredicate predicate
            Action = action
        }
    let createMasterKeyChanger (predicate: GeneratorContext -> MasterKey -> bool) (action: GeneratorContext -> Render -> Render) =
        createMasterKeyStrategyInterceptor predicate (ChangeRender action)
    let createMasterKeyInterceptor (predicate: GeneratorContext -> MasterKey -> bool) action =
        createMasterKeyStrategyInterceptor predicate (Intercept action)
    let createNameChanger (predicate: string -> bool) (action: GeneratorContext -> Render -> Render) =
        createNameStrategyInterceptor predicate (ChangeRender action)
    let createNameInterceptor (predicate: string -> bool) action =
        createNameStrategyInterceptor predicate (Intercept action)
    let withSourcePredicate (sourcePredicate: string -> bool) (value: Interceptor) =
        { value with Source = ValueSome sourcePredicate }
    let withEsLibOnly (value: Interceptor) =
        { value with Strategy = EsLibOnly value.Strategy }


module ComputedInterceptors =
    let computeInterceptor (genCache: GeneratorContext) (value: Interceptor) =
        let sourceSet =
            value.Source
            |> ValueOption.map (fun sourcePredicate ->
                genCache.ctx.cache.sourceKeys
                |> Seq.choose (function
                    | KeyValue(key, sourceValue) when sourcePredicate sourceValue -> Some key
                    | _ -> None
                    )
                |> Set
                )
        let predicate =
            let rec computeStrategy strategy =
                match strategy with
                | MasterKeyPredicate predicate ->
                    predicate genCache
                | NamePredicate predicate ->
                    fun masterKey ->
                        match PatternContext.prepare genCache.ctx masterKey with
                        | MasterKey.HasName (Value name) when predicate name -> true
                        | _ -> false
                | EsLibOnly (MasterKeyPredicate predicate) ->
                    fun masterKey ->
                        match PatternContext.prepare genCache.ctx masterKey with
                        | MasterKey.VisitationFlags.IsEsLib ->
                            predicate genCache masterKey
                        | _ -> false
                | EsLibOnly (NamePredicate predicate) ->
                    fun masterKey ->
                        match PatternContext.prepare genCache.ctx masterKey with
                        | MasterKey.VisitationFlags.IsEsLib & MasterKey.HasName (Value name) when  predicate name -> true
                        | _ -> false
                | EsLibOnly strategy -> computeStrategy strategy
            computeStrategy value.Strategy
            
        match value.Action with
        | ChangeRender action ->
            {
                ComputedChangeInterceptor.SourceKeySet = sourceSet
                Predicate = predicate
                Action = action genCache
            }
            |> Choice1Of2
        | Intercept fn ->
            {
                ComputedInterceptInterceptor.SourceKeySet = sourceSet
                Predicate = predicate
                Action = fn
            }
            |> Choice2Of2
    let create genCache (interceptors: Interceptor list) =
        interceptors
        |> Seq.map (computeInterceptor genCache)
        |> Seq.fold (fun acc -> function
            | Choice1Of2 changer -> { acc with Change = Array.appendOne changer acc.Change }
            | Choice2Of2 interceptor -> { acc with Intercept = Array.appendOne interceptor acc.Intercept }) { Change = Array.empty; Intercept = Array.empty }

    let append genCache interceptor computedInterceptors =
        computeInterceptor genCache interceptor
        |> function
            | Choice1Of2 changer -> { computedInterceptors with Change = Array.appendOne changer computedInterceptors.Change }
            | Choice2Of2 interceptor -> { computedInterceptors with Intercept = Array.appendOne interceptor computedInterceptors.Intercept }
            
    let flipAppend genCache computedInterceptors interceptor =
        append genCache interceptor computedInterceptors
module Interceptors =
    let init = Precompute []
    let append genCache interceptor = function
        | Precompute interceptors -> Precompute (interceptor :: interceptors)
        | Computed interceptors ->
            interceptor
            |> ComputedInterceptors.flipAppend genCache interceptors
            |> Computed
    let compute genCache = function
        | Precompute values ->
            ComputedInterceptors.create genCache values
            |> Computed
        | value -> value

module TypeRefRender =
    /// <summary>
    /// SRTP helper which allows you to use the GeneratorContext in place of the KeyResolutionContext without
    /// using interfaces.
    /// </summary>
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type KeyResolutionContextHelper =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        static member inline ToKeyResolutionContext (ctx: KeyResolutionContext) = ctx
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        static member inline ToKeyResolutionContext (ctx: GeneratorContext) = ctx.ctx
        
        
    [<EditorBrowsable(EditorBrowsableState.Advanced)>]
    let inline createWidget (isNullable: bool) (widget: WidgetBuilder<Type>) =
        {
            Type = TypeRefKind.Widget widget
            Nullable = isNullable
        }
        
    [<EditorBrowsable(EditorBrowsableState.Advanced)>]
    let inline createPath (isNullable: bool) (path: KeyPathKind) =
        {
            Type = TypeRefKind.Path path
            Nullable = isNullable
        }
        
    /// SRTP router
    let inline create (isNullable: bool) (value: ^T) =
        {
            Type =
                ((^T or TypeRefKind):(static member Create: ^T -> TypeRefKind) value)
            Nullable = isNullable
        }
        
    /// <summary>
    /// Renders a type reference to a widget. Nullability is ignored.
    /// </summary>
    let inline toWidgetNoOption (ctx: ^T) (refContext: KeyPathKind) (render: TypeRefRender) =
        let ctx = ((^T or KeyResolutionContextHelper):(static member ToKeyResolutionContext: ^T -> KeyResolutionContext) ctx)
        render.Type.toWidget ctx refContext
    
    /// <summary>
    /// Renders a type reference to a widget. If the type reference is
    /// nullable, then it is wrapped in an option.
    /// </summary>
    let inline toWidget (ctx: ^T) (refContext: KeyPathKind) (render: TypeRefRender) =
        toWidgetNoOption ctx refContext render
        |> if render.Nullable then Types.option else id

type DelayedTypeMaybePathedRender =
    static member inline Create (render: TypeMaybePathedRender) = render
    static member inline Create (render: TypeRefRender): TypeMaybePathedRender = render |> TypeMaybePathedRender.RefOnly
    static member inline Create(render: PathedRender): TypeMaybePathedRender = render |> TypeMaybePathedRender.Pathed
    static member inline Create (render: KeyPathKind * ComputeRenderFunc) = render |> fun (path, render) -> Pathed { Render = render; Path = path }
    static member inline Create (render: ComputeRenderFunc * KeyPathKind) = render |> fun (render, path) -> Pathed { Render = render; Path = path }

module Render =
    let inline create genCache (isNullable: bool) (shortCircuit: ^T) ([<InlineIfLambda>] full: unit -> ^U) =
        {
            ShortCircuit = fun parentPath -> 
                match TypeRefRender.create isNullable shortCircuit with
                | { Type = TypeRefKind.Path path } as render ->
                    { Type = TypeRefKind.Widget (TypeRefRender.toWidget genCache parentPath render); Nullable = render.Nullable }
                | render -> render
                |> TypeRefRender.toWidget genCache parentPath
                |> TypeRefRender.create isNullable
            Full =
                (lazy
                    let full = full ()
                    ((^U or DelayedTypeMaybePathedRender):(static member Create: ^U -> TypeMaybePathedRender) full))
                |> ValueSome 
        }
    
    let inline createShortOnly (isNullable: bool) (shortCircuit: ^T) =
        {
            ShortCircuit =
                TypeRefRender.create isNullable shortCircuit
                |> fun render _ -> render
            Full = ValueNone
        }
    let inline createPathedShortOnly (isNullable: bool) ([<InlineIfLambda>] fn: KeyPathKind -> ^T) =
        {
            ShortCircuit = fn >> TypeRefRender.create isNullable
            Full = ValueNone
        }

module GeneratorContext =
    /// <summary>
    /// Initialises the GeneratorContext with the given key resolution context, and the render function.
    /// </summary>
    let init (ctx: KeyResolutionContext) (renderFunc: RenderFunc) =
        let interceptors = Interceptors.init
        let seenKeys = HashSet()
        let shortCircuits = Dictionary()
        let cache = Dictionary()
        let auxilliary = Dictionary()
        let renderPaths = Dictionary()
        let keyDependencies = Dictionary()
        let renderingStack = Stack()
        {
            seenKeys = seenKeys
            keyDependencies = keyDependencies
            renderingStack = renderingStack
            interceptors = interceptors
            shortCircuitCache = shortCircuits
            cache = cache
            auxilliary = auxilliary
            renderPaths = renderPaths
            ctx = ctx
            renderFunc = renderFunc
            pathResolver = {
                Initializer = keyPathInitFunc
                Prerenderer = keyPathFunc
                Localiser = KeyPath.localiser
                Renderer = KeyPath.createPathTypeRenderer ctx
            }
        }
    
    let addInterceptor (interceptor: Interceptor) (ctx: GeneratorContext) =
        { ctx with interceptors = Interceptors.append ctx interceptor ctx.interceptors }
    
    let computeInterceptors (ctx: GeneratorContext) =
        { ctx with interceptors = Interceptors.compute ctx ctx.interceptors }
    
    type SeenKeyStatus =
        | Unseen
        | Rendering of ComputeShortCircuitFunc
        | Seen of TypeMaybePathedRender

    /// <summary>
    /// Any interaction with a key should hit this function.<br/>
    /// If the key has never been seen, then a fieldless DU is returned.<br/>
    /// If the key has been seen, then we check the cache to see whether the
    /// final rendered type information is available; if so, this value is returned.<br/>
    /// On failing this, we return the short circuit value of the key, which must be
    /// returned prior to the cache lookup.<br/>
    /// <i>WE CANNOT</i> request lookups which will resolve members
    /// when resolving a short circuit value.<br/>
    /// </summary>
    let private seeKey (ctx: GeneratorContext) (key: MasterKey) =
        if ctx.seenKeys.Add(key) then Unseen
        else
        ctx.cache
        |> Dictionary.tryItem key
        |> ValueOption.map Seen
        |> ValueOption.orElseWith (fun () ->
            ctx.shortCircuitCache
            |> Dictionary.tryItem key
            |> ValueOption.map Rendering
            )
        |> ValueOption.defaultValue Unseen
    
    let private addPathToMasterKey (ctx: GeneratorContext) (path: KeyPathKind) (key: MasterKey) =
        ctx.renderPaths
        |> Dictionary.tryAddOrGet key (HashSet())
        |> _.Add(path)
        |> ignore
    
    let inline private registerShortCircuit (ctx: GeneratorContext) (key: MasterKey) (render: Render) =
        render.ShortCircuit
        |> Dictionary.Flip.tryAdd ctx.shortCircuitCache key
        render
    
    let inline private registerFullRender (ctx: GeneratorContext) (key: MasterKey) (render: Render) =
        render.Full
        |> ValueOption.iter (_.Value >> Dictionary.Flip.tryAdd ctx.cache key)
        render
    
    /// <summary>
    /// Adds the key to the dependency graph.
    /// </summary>
    let private color (ctx: GeneratorContext) (key: MasterKey) =
        Stack.tryPeek ctx.renderingStack
        |> ValueOption.iter (fun stackKey ->
            ctx.keyDependencies
            |> Dictionary.tryAddOrGet stackKey (HashSet())
            |> _.Add(key)
            |> ignore)
    
    /// <summary>
    /// Removes the key from the dependency tracking stack. If the removed
    /// key was not the same as the given key, then we throw an exception.
    /// </summary>
    let inline private stripPaint (ctx: GeneratorContext) (key: MasterKey) = fun pass ->
        if not (
                ctx.renderingStack
                |> Stack.tryPop
                |> ValueOption.exists ((=) key)
        ) then failwith "Paint stack is corrupt"
        pass
    /// <summary>
    /// Force correct tracking and detracking of master key dependencies via
    /// a stack. The key is tracked prior to the function being called, after
    /// which it is removed from the dependency tracking stack.
    /// </summary>
    let inline private paint (ctx: GeneratorContext) (key: MasterKey) ([<InlineIfLambda>] fn: unit -> 'T) =
        color ctx key // Associate the key to the key already on the stack BEFORE adding this key to the stack
        ctx.renderingStack
        |> Stack.push key // push key to stack
        fn() // run fn
        |> stripPaint ctx key // pop key from stack
        
    /// <summary>
    /// Returns the full render function for the given key if available; otherwise, returns the
    /// short circuit function value. If the key has never been seen, then the render function
    /// in the current context is used to visit the key.
    /// </summary>
    let visit (ctx: GeneratorContext) (key: MasterKey) = paint ctx key (
        fun () ->
            match seeKey ctx key with
            | Unseen ->
                    ctx.renderFunc ctx key
                    |> registerShortCircuit ctx key
                    |> registerFullRender ctx key
                    |> function
                        | { Full = ValueSome fn } ->
                            match fn.Value with
                            | Pathed render ->
                                render.Render render.Path
                                |> ignore
                            | _ -> ()
                            Choice2Of2 fn.Value
                        | { ShortCircuit = fn } -> Choice1Of2 fn
            | Rendering typeRefRender -> Choice1Of2 typeRefRender
            | Seen maybePathedRender -> Choice2Of2 maybePathedRender
        )

    /// <summary>
    /// Alias to <c>visit >> ignore</c>
    /// </summary>
    let touch (ctx: GeneratorContext) (key: MasterKey) =
        visit ctx key |> ignore
    
    /// <summary>
    /// Gets a TypeRefRender for the given key.
    /// </summary>
    let getTypeRef (ctx: GeneratorContext) (masterKey: MasterKey) : ComputeShortCircuitFunc =
        match visit ctx masterKey with
        | Choice1Of2 typeRefRender -> typeRefRender
        | Choice2Of2 pathedRender ->
            match pathedRender with
            | RefOnly typeRefRender -> fun _ -> typeRefRender
            | Pathed { Path = path } -> fun _ -> { Type = TypeRefKind.Path path; Nullable = false }
    
    /// <summary>
    /// Gets the full render for the given key, if one exists & it is available
    /// </summary>
    let tryGetTypeRender (ctx: GeneratorContext) (masterKey: MasterKey): TypeMaybePathedRender voption =
        match visit ctx masterKey with
        | Choice2Of2 maybePathedRender ->
            ValueSome maybePathedRender
        | _ -> ValueNone
    
    let inline private renderTypeRefImpl
        (fnPathed: KeyResolutionContext -> KeyPathKind -> TypeRefRender -> WidgetBuilder<Type>)
        (ctx: GeneratorContext) (currentPath: KeyPathKind) (masterKey: MasterKey) =
        getTypeRef ctx masterKey currentPath
        |> fnPathed ctx.ctx  currentPath
    let renderTypeRef (ctx: GeneratorContext) (currentPath: KeyPathKind) (masterKey: MasterKey) =
        renderTypeRefImpl TypeRefRender.toWidget ctx currentPath masterKey 
    let renderTypeRefNoOption (ctx: GeneratorContext) (currentPath: KeyPathKind) (masterKey: MasterKey) =
        renderTypeRefImpl TypeRefRender.toWidgetNoOption ctx currentPath masterKey
        
    let createPathFrom (ctx: GeneratorContext) (parentPath: KeyPathKind) (next: PatternContextHolder<MasterKey>) =
        ctx.pathResolver.Prerenderer parentPath next
    let inline createPathTo ctx next parentPath = createPathFrom ctx parentPath next
    let inline prepareAndCreatePathFrom (ctx: GeneratorContext) (parentPath: KeyPathKind) (next: MasterKey) =
        PatternContext.prepare ctx.ctx next
        |> createPathFrom ctx parentPath
    let inline prepareAndCreatePathTo ctx next parentPath = createPathFrom ctx parentPath next
    
type GeneratorContext with
    static member Create(ctx: KeyResolutionContext, renderFunc: RenderFunc,  ?interceptors: Interceptor list) =
        let result = GeneratorContext.init ctx renderFunc
        match interceptors with
        | Some interceptors ->
            { result with interceptors = Interceptors.Precompute interceptors }
        | _ -> result
    member inline this.Add(interceptor: Interceptor) = GeneratorContext.addInterceptor interceptor this
    member inline this.ComputeInterceptors() = GeneratorContext.computeInterceptors this
    member inline this.RenderTypeRef(
        masterKey: MasterKey,
        currentPath: KeyPathKind,
        [<Struct>] ?respectOptional: bool
    ) =
        if defaultValueArg respectOptional true
        then GeneratorContext.renderTypeRef this currentPath masterKey 
        else GeneratorContext.renderTypeRefNoOption this currentPath masterKey 
    member inline this.Visit(masterKey: MasterKey) =
        GeneratorContext.visit this masterKey
    member inline this.Touch(masterKey: MasterKey) =
        GeneratorContext.touch this masterKey
    member inline this.GetTypeRef(masterKey: MasterKey) =
        GeneratorContext.getTypeRef this masterKey
    member inline this.GetTypeRender(masterKey: MasterKey) =
        GeneratorContext.tryGetTypeRender this masterKey