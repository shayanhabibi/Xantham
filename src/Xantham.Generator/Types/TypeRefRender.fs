module Xantham.Generator.TypeRefRender

open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.Generator.NamePath

// type ref renders are either directly widgets, or are
// paths to the type
[<System.Flags>]
type TypedNameTraits =
    | None = 0
    | Optional = (1 <<< 0)
    | ParamArray = (1 <<< 1)
    | Static = (1 <<< 2)
    | Readable = (1 <<< 3)
    | Writable = (1 <<< 4)
    | Literal = (1 <<< 5)
    | JSGetter = (1 <<< 6)
    | JSSetter = (1 <<< 7)
    | JSIndexer = (1 <<< 8)
    | JSConstructor = (1 <<< 9)
    | JSCallSignature = (1 <<< 10)
    | EmitSelf = (1 <<< 11)
    | Inline = (1 <<< 12)
    | StringBuilder = (1 <<< 13)

module TypedNameTraits =
    [<Literal>]
    let ParameterTraits =
        TypedNameTraits.Optional ||| TypedNameTraits.ParamArray
    [<Literal>]
    let GetterTraits =
        TypedNameTraits.JSGetter ||| TypedNameTraits.Static
    [<Literal>]
    let SetterTraits =
        TypedNameTraits.JSSetter ||| TypedNameTraits.Static
    [<Literal>]
    let PropertyTraits =
        TypedNameTraits.Readable
        ||| TypedNameTraits.Writable
        ||| TypedNameTraits.Static
        ||| TypedNameTraits.Optional
    let inline isOptional (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.Optional)
    let inline isParamArray (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.ParamArray)
    let inline isStatic (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.Static)
    let inline isReadable (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.Readable)
    let inline isWritable (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.Writable)
    let inline isLiteral (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.Literal)
    let inline isJSGetter (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.JSGetter)
    let inline isJSSetter (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.JSSetter)
    let inline isJSIndexer (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.JSIndexer)
    let inline isJSConstructor (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.JSConstructor)
    let inline isJSCallSignature (traits: TypedNameTraits) = traits.HasFlag(TypedNameTraits.JSCallSignature)

(*
What is the recursive issue with rendering?
The issue is that we may introspect a type and enter a cycle when we introspect
further to determine what should be done.

When we process a type to be referenced, we will produce either:
1 a primitive widget render
2 a path to a concrete type
3 a transient path for a transient type that has to be rendered with whoever
    is calling for the reference.
4 a composition of the above.

1 & 2 can be immediately processed without further inspecting the type.
3 usually can only be elicited after inspecting a type to determine whether
it needs to be rendered as a transient type at all. So long as we are not
requesting further introspection, we can safely proceed with heuristics.

Naturally, we cache these results against the resolved type that elicited the
reference.

When we finalise our renders, concrete paths that contain transient references will
require those transient references to be anchored - and in doing so, will require
those references to point to the renders that we will create.

Therefore:
> Transient anchoring will lift the renders to the caller
> Type reference rendering can lift renders
> Any final rendering of a type/reference can lift renders

What if a transient type also contains a transient reference? What if that transient
reference forms a cycle? We would endleslly try to lift the transient render and
cause a stack overflow.

> Any transient render is caused by an anchored path.
> Concrete paths (distinct from anchored paths) are the root of any transient render graph.

If we track all the types that are associated with a concrete paths render until it
hits another concrete path or simple primitive, we can determine whether we need to
recreate a transient type reference, or if we already have the reference in the concrete
paths scope.

That's actually a valid topology for the issue. Each concrete path forms a 'scope'.

Transient Scope graph root (transient path):
0. Transient Root 
1. Transient paths - terminal node
2. Primitive - terminal node
3. Concrete path - terminal node
4. Composite - branch
> Associated renders: root render

Concrete Scope graph:
1. Concrete path - root / terminal node
2. Primitive - terminal only
3. Transient path - terminal only
4. Composite - branch
> Associated renders: root render

Anchored Scope graph root (concrete):
0. Concrete root
1. Concrete path - terminal node
2. Transient path - branch / cycles
3. Primitive - terminal only
4. Composite - branch
> Associated renders: root render, and all transient renders

As we build the anchored scope graph, we can only have one transient
anchored per scope. So cyclic references, or cross references must be resolved
with this in mind.

Best to do this hierarchically, rather than in sequence.
*)

/// <summary>
/// <para>Building block, or sole component of any type reference.<br/>
/// Essentially, either a precomposed widget, or a path.</para>
/// <para>Paths must be <c>anchored</c> (i.e. concrete)</para>
/// </summary>
type [<RequireQualifiedAccess>] TypeRefAtom =
    | Widget of WidgetBuilder<Type>
    | AnchorPath of TypePath
    | TransientPath of path: TransientTypePath 

/// <summary>
/// <para>Composite reference types such as function signatures, tuples, unions, or
/// type prefix applications.</para>
/// <para>They are composed of <c>TypeRefRender</c> so that the concept of <c>nullability</c>
/// is available to a molecule in isolation. This provides enough information to independently
/// handle optionality in each of its cases.</para>
/// </summary>
and [<RequireQualifiedAccess>] TypeRefMolecule =
    | Tuple of TypeRefRender list
    | Union of TypeRefRender list
    | Function of parameters: TypeRefRender list * returnType: TypeRefRender
    | Prefix of prefix: TypeRefRender * args: TypeRefRender list

and [<RequireQualifiedAccess>] TypeRefKind =
    | Atom of TypeRefAtom
    | Molecule of TypeRefMolecule

and TypeRefRender = {
    Kind: TypeRefKind
    Nullable: bool
}

and RenderMetadata = { Path: Path voption }

and TypeParameterRender = {
    Metadata: RenderMetadata
    Name: Name<Case.typar>
    Constraint: Render voption
    Default: Render voption
    Documentation: TsComment list
}

and TypedNameRender = {
    Metadata: RenderMetadata
    Name: Name<Case.camel>
    Type: Render
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

and FunctionLikeSignature = {
    Metadata: RenderMetadata
    Parameters: TypedNameRender array
    ReturnType: Render
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

and FunctionLikeRender = {
    Metadata: RenderMetadata
    Name: Name<Case.camel>
    Signatures: FunctionLikeSignature array
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

and LiteralCaseRender<'value> = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    Value: 'value
    Documentation: TsComment list
}

and LiteralUnionRender<'value> = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    Cases: LiteralCaseRender<'value> array
    Documentation: TsComment list
}

and TypeLikeRender = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender array
    Members: TypedNameRender array
    Functions: FunctionLikeRender array
    Inheritance: Render array
    // each array is a constructor signature
    Constructors: TypedNameRender[][]
    Documentation: TsComment list
}

and [<RequireQualifiedAccess>] TypeAliasRender =
    | Alias of TypeAliasRenderRef
    | TypeDefn of TypeLikeRender
    | StringUnion of LiteralUnionRender<TsLiteral>
    | EnumUnion of LiteralUnionRender<int>
    | Function of FunctionLikeRender

and TypeAliasRenderRef = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
    Type: Render
}

and [<RequireQualifiedAccess>] TypeRender =
    | TypeDefn of TypeLikeRender
    | StringUnion of LiteralUnionRender<TsLiteral>
    | EnumUnion of LiteralUnionRender<int>
    | Function of FunctionLikeRender
    | Variable of TypedNameRender
    | TypeAlias of TypeAliasRender

and [<RequireQualifiedAccess>] MemberRender =
    | Property of TypedNameRender
    | Method of FunctionLikeRender

and [<RequireQualifiedAccess>] Render =
    | RefOnly of TypeRefRender
    | Render of TypeRefRender * TypeRender
    | TransientRender of TypeRefRender * (AnchorPath -> TypeRender)
    member inline this.TypeRefRender =
        match this with
        | TransientRender (ref, _)
        | Render (ref, _)
        | RefOnly ref -> ref
    member inline this.Nullable =
        this.TypeRefRender.Nullable

module AnchorPath =
    let createModule (modulePath: ModulePath) = AnchorPath.Module modulePath
    let createType (typePath: TypePath) = AnchorPath.Type typePath
    let createMember (memberPath: MemberPath) = AnchorPath.Member memberPath
    let createParameter (parameterPath: ParameterPath) = AnchorPath.Parameter parameterPath
    let createTypeParameter (typeParameterPath: TypeParamPath) = AnchorPath.TypeParam typeParameterPath
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(modulePath: ModulePath) = createModule modulePath
        static member inline Create(typePath: TypePath) = createType typePath
        static member inline Create(memberPath: MemberPath) = createMember memberPath
        static member inline Create(parameterPath: ParameterPath) = createParameter parameterPath
        static member inline Create(typeParameterPath: TypeParamPath) = createTypeParameter typeParameterPath
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> AnchorPath) value)

module TransientPath =
    let createType (typePath: TransientTypePath) = TransientPath.Type typePath
    let createMember (memberPath: TransientMemberPath) = TransientPath.Member memberPath
    let createParameter (parameterPath: TransientParameterPath) = TransientPath.Parameter parameterPath
    let createTypeParameter (typeParameterPath: TransientTypePath) = TransientPath.TypeParam typeParameterPath
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(typePath: TransientTypePath) = createType typePath
        static member inline Create(memberPath: TransientMemberPath) = createMember memberPath
        static member inline Create(parameterPath: TransientParameterPath) = createParameter parameterPath
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TransientPath) value)

module Path =
    let createAnchor (anchorPath: AnchorPath) = Path.Anchor anchorPath
    let createTransient (transientPath: TransientPath) = Path.Transient transientPath
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(anchorPath: AnchorPath) = createAnchor anchorPath
        static member inline Create(transientPath: TransientPath) = createTransient transientPath
        static member inline Create(typePath: TransientTypePath) = TransientPath.createType typePath |> SRTPHelper.Create
        static member inline Create(memberPath: TransientMemberPath) = TransientPath.createMember memberPath |> SRTPHelper.Create
        static member inline Create(parameterPath: TransientParameterPath) = TransientPath.createParameter parameterPath |> SRTPHelper.Create
        static member inline Create(modulePath: ModulePath) = AnchorPath.createModule modulePath |> SRTPHelper.Create
        static member inline Create(typePath: TypePath) = AnchorPath.createType typePath |> SRTPHelper.Create
        static member inline Create(memberPath: MemberPath) = AnchorPath.createMember memberPath |> SRTPHelper.Create
        static member inline Create(parameterPath: ParameterPath) = AnchorPath.createParameter parameterPath |> SRTPHelper.Create
        static member inline Create(typeParameterPath: TypeParamPath) = AnchorPath.createTypeParameter typeParameterPath |> SRTPHelper.Create
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> Path) value)
    
module TypeRefAtom =
    let createWidget (widget: WidgetBuilder<Type>) =
        TypeRefAtom.Widget widget
    let createAnchorPath (path: TypePath) =
        TypeRefAtom.AnchorPath path
    let createTransientPath (path: TransientTypePath) =
        TypeRefAtom.TransientPath path
    let anchor (anchorPath: AnchorPath) (typeRefAtom: TypeRefAtom) =
        match typeRefAtom with
        | TypeRefAtom.TransientPath transientPath ->
            TransientTypePath.anchor anchorPath transientPath
            |> TypeRefAtom.AnchorPath
        | _ -> typeRefAtom
    let localisePaths (localOrientationPath: AnchorPath) (typeRefAtom: TypeRefAtom) =
        match typeRefAtom with
        | TypeRefAtom.AnchorPath path ->
            Path.getRelativePath path localOrientationPath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
            |> TypeRefAtom.Widget
        | TypeRefAtom.TransientPath transientPath ->
            TransientTypePath.anchor localOrientationPath transientPath
            |> Path.getRelativePath
            |> funApply localOrientationPath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
            |> TypeRefAtom.Widget
        | _ -> typeRefAtom
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(value: WidgetBuilder<Type>): TypeRefAtom = createWidget value
        static member inline Create(value: TypePath): TypeRefAtom = createAnchorPath value
        static member inline Create(value: TransientTypePath): TypeRefAtom = createTransientPath value
    
    let inline create (value: ^T) : TypeRefAtom = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefAtom) value)

module TypeRefMolecule =
    let createTuple (atoms: TypeRefRender list) : TypeRefMolecule =
        TypeRefMolecule.Tuple atoms
    let createUnion (atoms: TypeRefRender list) : TypeRefMolecule =
        TypeRefMolecule.Union atoms
    let createFunction (returnValue: TypeRefRender) (atoms: TypeRefRender list) : TypeRefMolecule =
        TypeRefMolecule.Function(atoms, returnValue)
    let createPrefix (prefix: TypeRefRender) (args: TypeRefRender list) : TypeRefMolecule =
        TypeRefMolecule.Prefix(prefix, args)
        
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        /// <summary>
        /// For SRTP purposes, we delineate a union from a tuple construction by the type of the collection.
        /// </summary>
        static member inline Create(values: TypeRefRender array): TypeRefMolecule = createTuple (Array.toList values)
        static member inline Create(values: TypeRefRender list): TypeRefMolecule = createUnion values
        static member inline Create(value: TypeRefRender list * TypeRefRender): TypeRefMolecule = createFunction (snd value) (fst value)
        static member inline Create(prefix: TypeRefRender, args: TypeRefRender list): TypeRefMolecule = createPrefix prefix args
        
    let inline create (value: ^T) : TypeRefMolecule = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefMolecule) value)
    
module TypeRefKind =
    let createWidget (widget: WidgetBuilder<Type>) : TypeRefKind =
        TypeRefKind.Atom (TypeRefAtom.createWidget widget)
    let createAnchorPath (path: TypePath) : TypeRefKind =
        TypeRefKind.Atom (TypeRefAtom.createAnchorPath path)
    let createTransientPath (path: TransientTypePath) : TypeRefKind =
        TypeRefKind.Atom (TypeRefAtom.createTransientPath path)
    let createUnion (atoms: TypeRefRender list) : TypeRefKind =
        TypeRefKind.Molecule (TypeRefMolecule.createUnion atoms)
    let createFunction (returnValue: TypeRefRender) (atoms: TypeRefRender list) : TypeRefKind =
        TypeRefKind.Molecule (TypeRefMolecule.createFunction returnValue atoms)
    let createPrefix (prefix: TypeRefRender) (args: TypeRefRender list) : TypeRefKind =
        TypeRefKind.Molecule (TypeRefMolecule.createPrefix prefix args)
    let createTuple (atoms: TypeRefRender list) : TypeRefKind =
        TypeRefKind.Molecule (TypeRefMolecule.createTuple atoms)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(value: WidgetBuilder<Type>): TypeRefKind = createWidget value
        static member inline Create(value: TypePath): TypeRefKind = createAnchorPath value
        static member inline Create(value: TransientTypePath): TypeRefKind = createTransientPath value
        static member inline Create(values: TypeRefRender list): TypeRefKind = createUnion values
        static member inline Create(value: TypeRefRender list * TypeRefRender): TypeRefKind = createFunction (snd value) (fst value)
        static member inline Create(prefix: TypeRefRender, args: TypeRefRender list): TypeRefKind = createPrefix prefix args
        static member inline Create(values: TypeRefRender array): TypeRefKind = createTuple (Array.toList values)
    
    let inline create (value: ^T) : TypeRefKind = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefKind) value)

module TypeRefRender =
    // flattens a list of renders by recursively seeking out and expanding unions.
    let rec private flatten (render: TypeRefRender list) : TypeRefRender list =
        render
        |> List.collect (function
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = true } ->
                { Kind = TypeRefKind.createWidget (Ast.Unit()); Nullable = false }
                :: flatten atoms
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = false } ->
                flatten atoms
            | { Nullable = true } as render ->
                [ { render with Nullable = false }; { Kind = TypeRefKind.createWidget (Ast.Unit()); Nullable = false } ]
            | render -> [ render ]
            )
    let inline createWithKind (nullable: bool) (kind: TypeRefKind) : TypeRefRender = {
        Kind = kind
        Nullable = nullable
    }
    let createWidget (nullable: bool) (widget: WidgetBuilder<Type>) : TypeRefRender =
        TypeRefKind.createWidget widget
        |> createWithKind nullable
    let createAnchorPath (nullable: bool) (path: TypePath) : TypeRefRender =
        TypeRefKind.createAnchorPath path
        |> createWithKind nullable
    let createTransientPath (nullable: bool) (path: TransientTypePath) : TypeRefRender =
        TypeRefKind.createTransientPath path
        |> createWithKind nullable
    let createUnion (nullable: bool) (atoms: TypeRefRender list) : TypeRefRender =
        flatten atoms
        |> TypeRefKind.createUnion 
        |> createWithKind nullable
    let createFunction (nullable: bool) (returnValue: TypeRefRender) (atoms: TypeRefRender list) : TypeRefRender =
        TypeRefKind.createFunction returnValue atoms
        |> createWithKind nullable
    let createPrefix (nullable: bool) (prefix: TypeRefRender) (args: TypeRefRender list) : TypeRefRender =
        TypeRefKind.createPrefix prefix args
        |> createWithKind nullable
    let createTuple (nullable: bool) (atoms: TypeRefRender list) : TypeRefRender =
        TypeRefKind.createTuple atoms
        |> createWithKind nullable
    let inline orNullable (value: bool) (typeRefRender: TypeRefRender) : TypeRefRender = { typeRefRender with Nullable = typeRefRender.Nullable || value }
    /// <summary>
    /// <para>Sets the nullability of the type reference render.</para>
    /// <para><b>IMPORTANT: USE <c>orNullable</c>?</b><br/><c>setNullable</c> should not be used when handling
    /// type reference rendering of parameters, properties, and other member-like structures that have
    /// <c>IsOptional</c> fields.<br/>
    /// These would ignore the underlying types optionality. Use <c>orNullable</c> instead, to apply
    /// a <c>OR</c> operation, instead of overwriting the field.</para>
    /// </summary>
    /// <param name="value"></param>
    /// <param name="typeRefRender"></param>
    let inline setNullable (value: bool) (typeRefRender: TypeRefRender) : TypeRefRender = { typeRefRender with Nullable = value }
    let nullable: TypeRefRender -> TypeRefRender = setNullable true
    let nonNullable: TypeRefRender -> TypeRefRender = setNullable false
    
    /// <summary>
    /// Recursively localises a type reference render to the provided anchor path.
    /// </summary>
    /// <param name="anchorPath"></param>
    /// <param name="typeRefRender"></param>
    let rec localisePaths (anchorPath: AnchorPath) (typeRefRender: TypeRefRender) : TypeRefRender =
        let wrap value = { typeRefRender with Kind = value }
        match typeRefRender.Kind with
        | TypeRefKind.Atom atom ->
            TypeRefAtom.localisePaths anchorPath atom
            |> TypeRefKind.Atom
            |> wrap
        | TypeRefKind.Molecule molecule ->
            match molecule with
            | TypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map (localisePaths anchorPath)
                |> TypeRefMolecule.Tuple
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Union typeRefRenders ->
                typeRefRenders
                |> List.map (localisePaths anchorPath)
                |> TypeRefMolecule.Union
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Function(parameters, returnType) ->
                TypeRefMolecule.Function(
                    parameters
                    |> List.map (localisePaths anchorPath),
                    returnType |> localisePaths anchorPath
                    )
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Prefix(prefix, args) ->
                TypeRefMolecule.Prefix(
                    localisePaths anchorPath prefix,
                    args |> List.map (localisePaths anchorPath)
                    )
                |> TypeRefKind.Molecule
                |> wrap

    /// <summary>
    /// Recursively traces a type reference render and anchors transient paths
    /// to the provided anchor path.
    /// </summary>
    /// <param name="anchorPath"></param>
    /// <param name="typeRefRender"></param>
    let rec anchor (anchorPath: AnchorPath) (typeRefRender: TypeRefRender) : TypeRefRender =
        let wrap value = { typeRefRender with Kind = value }
        match typeRefRender.Kind with
        | TypeRefKind.Atom atom ->
            TypeRefAtom.anchor anchorPath atom
            |> TypeRefKind.Atom
            |> wrap
        | TypeRefKind.Molecule molecule ->
            match molecule with
            | TypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map (anchor anchorPath)
                |> TypeRefMolecule.Tuple
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Union typeRefRenders ->
                typeRefRenders
                |> List.map (anchor anchorPath)
                |> TypeRefMolecule.Union
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Function(parameters, returnType) ->
                TypeRefMolecule.Function (
                    parameters
                    |> List.map (anchor anchorPath),
                    anchor anchorPath returnType
                    )
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Prefix(prefix, args) ->
                TypeRefMolecule.Prefix (
                    prefix |> anchor anchorPath,
                    args |> List.map (anchor anchorPath)
                    )
                |> TypeRefKind.Molecule
                |> wrap
    
    let inline localiseAndAnchor (anchorPath: AnchorPath) (typeRefRender: TypeRefRender) : TypeRefRender =
        anchor anchorPath typeRefRender
        |> localisePaths anchorPath
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(tuple: TypeRefRender array, nullable: bool): TypeRefRender = createTuple nullable (List.ofArray tuple)
        static member inline Create(widget: WidgetBuilder<Type>, nullable: bool): TypeRefRender = createWidget nullable widget
        static member inline Create(path: TypePath, nullable: bool): TypeRefRender = createAnchorPath nullable path
        static member inline Create(path: TransientTypePath, nullable: bool): TypeRefRender = createTransientPath nullable path
        static member inline Create(unions: TypeRefRender list, nullable: bool): TypeRefRender = createUnion nullable unions
        static member inline Create(funcValues: TypeRefRender list * TypeRefRender, nullable: bool): TypeRefRender = createFunction nullable (snd funcValues) (fst funcValues)
        static member inline Create(prefixValues: TypeRefRender * TypeRefRender list, nullable: bool): TypeRefRender = createPrefix nullable (fst prefixValues) (snd prefixValues)
    
    let inline create (nullable: bool) (value: ^T) : TypeRefRender =
        ((^T or SRTPHelper): (static member Create: ^T * bool -> TypeRefRender) (value, nullable))

module TypeRender =
    let createDefn (typeLike: TypeLikeRender) : TypeRender =
        TypeRender.TypeDefn typeLike
    let createStringUnion (union: LiteralUnionRender<TsLiteral>) : TypeRender =
        TypeRender.StringUnion union
    let createEnumUnion (union: LiteralUnionRender<int>) : TypeRender =
        TypeRender.EnumUnion union
    let createFunction (func: FunctionLikeRender) : TypeRender =
        TypeRender.Function func
    let createVariable (variable: TypedNameRender) : TypeRender =
        TypeRender.Variable variable
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(typeDefn: TypeLikeRender): TypeRender = createDefn typeDefn
        static member inline Create(stringUnion: LiteralUnionRender<TsLiteral>): TypeRender = createStringUnion stringUnion
        static member inline Create(enumUnion: LiteralUnionRender<int>): TypeRender = createEnumUnion enumUnion
        static member inline Create(func: FunctionLikeRender): TypeRender = createFunction func
        static member inline Create(variable: TypedNameRender): TypeRender = createVariable variable
    
    let inline create (value: ^T) : TypeRender = ((^T or SRTPHelper): (static member Create: ^T -> TypeRender) value)

module Render =
    let makeRefOnly (render: TypeRefRender) : Render =
        Render.RefOnly render
    let makeWithRefRender (typeRefRender: TypeRefRender) (typeRender: TypeRender) : Render =
        Render.Render (typeRefRender, typeRender)
    let inline createRefOnly (nullable: bool) (refRender: ^T) : Render =
        TypeRefRender.create nullable refRender
        |> makeRefOnly
    /// <summary>
    /// </summary>
    /// <param name="nullable"></param>
    /// <param name="refRender"></param>
    /// <param name="render"></param>
    let inline create (nullable: bool) (refRender: ^T) (render: ^U) : Render =
        let refRender = TypeRefRender.create nullable refRender
        let render = TypeRender.create render
        makeWithRefRender refRender render
        

module RenderMetadata =
    let empty = { Path = ValueNone }
    let withTransientPath (metadata: RenderMetadata) (path: TransientPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Transient path) }
    let withAnchorPath (metadata: RenderMetadata) (path: AnchorPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor path) }
    let withTransientTypePath (metadata: RenderMetadata) (path: TransientTypePath) : RenderMetadata = { metadata with Path = ValueSome (Path.Transient (TransientPath.Type path)) }
    let withTransientMemberPath (metadata: RenderMetadata) (path: TransientMemberPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Transient (TransientPath.Member path)) }
    let withTransientParameterPath (metadata: RenderMetadata) (path: TransientParameterPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Transient (TransientPath.Parameter path)) }
    let withAnchorModulePath (metadata: RenderMetadata) (path: ModulePath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor (AnchorPath.Module path)) }
    let withAnchorTypePath (metadata: RenderMetadata) (path: TypePath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor (AnchorPath.Type path)) }
    let withAnchorMemberPath (metadata: RenderMetadata) (path: MemberPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor (AnchorPath.Member path)) }
    let withAnchorParameterPath (metadata: RenderMetadata) (path: ParameterPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor (AnchorPath.Parameter path)) }
    let withAnchorTypeParameterPath (metadata: RenderMetadata) (path: TypeParamPath) : RenderMetadata = { metadata with Path = ValueSome (Path.Anchor (AnchorPath.TypeParam path)) }
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(): RenderMetadata = empty
        static member inline Create(path: Path): RenderMetadata = { Path = ValueSome path }
        static member inline Create(path: AnchorPath): RenderMetadata = { Path = ValueSome (Path.Anchor path) }
        static member inline Create(path: TransientPath): RenderMetadata = { Path = ValueSome (Path.Transient path) }
        static member inline Create(path: TypePath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: ModulePath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: MemberPath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: ParameterPath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: TypeParamPath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: TransientTypePath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: TransientMemberPath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline Create(path: TransientParameterPath): RenderMetadata = { Path = ValueSome (Path.create path) }
        static member inline WithData(renderMetadata, data) = { renderMetadata with RenderMetadata.Path = ValueSome data }
        static member inline WithData(renderMetadata, data) = { renderMetadata with RenderMetadata.Path = ValueSome (Path.create data) }
        static member inline WithData(container: TypeParameterRender, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: TypedNameRender, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: FunctionLikeSignature, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: FunctionLikeRender, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: LiteralCaseRender<_>, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: LiteralUnionRender<_>, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        static member inline WithData(container: TypeLikeRender, data: 'a) = { container with Metadata = SRTPHelper.WithData(container.Metadata, data) }
        
    /// <summary>
    /// Highly versatile SRTP helper which allows for a field to be set for a metadata field of
    /// a <c>RenderMetadata</c>.
    /// </summary>
    /// <param name="data">A <c>QualifiedName</c> value, or any value that can be formed into a <c>Path</c> value.</param>
    /// <param name="metaDataOrMetadataContainer">A <c>RenderMetadata</c> record.</param>
    let inline withData (data: ^T) (metaDataOrMetadataContainer: ^Container) : ^Container = ((^T or SRTPHelper): (static member WithData: ^Container * ^T -> ^Container) (metaDataOrMetadataContainer, data))
    let inline create (pathOrSource: ^T) : RenderMetadata = ((^T or SRTPHelper): (static member Create: ^T -> RenderMetadata) pathOrSource)
    let inline createWithSource (source: QualifiedName) (path: ^U) : RenderMetadata = ((^U or SRTPHelper): (static member CreateWithSource: QualifiedName * ^U -> RenderMetadata) (source, path))

module TypeParameterRender =
    module Metadata =
        let rec inline withData (data: ^T) (typeParameter: TypeParameterRender) : TypeParameterRender = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module TypedNameRender =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: TypedNameRender) : TypedNameRender = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module FunctionLikeSignature =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: FunctionLikeSignature) : FunctionLikeSignature = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module FunctionLikeRender =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: FunctionLikeRender) : FunctionLikeRender = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module LiteralCaseRender =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: LiteralCaseRender<_>) : LiteralCaseRender<'a> = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module LiteralUnionRender =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: LiteralUnionRender<_>) : LiteralUnionRender<'a> = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
module TypeLikeRender =
    module Metadata =
        let inline withData (data: ^T) (typeParameter: TypeLikeRender) : TypeLikeRender = RenderMetadata.SRTPHelper.WithData(typeParameter, data)
