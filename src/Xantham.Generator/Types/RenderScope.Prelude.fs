[<AutoOpen>]
module Xantham.Generator.Types.Prelude

open System.Collections.Generic
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Decoder
open FSharp.SignalsDotnet

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

When building a transient scope graph, we must track:
- local transient path.

When building an anchored scope graph, we must track:
- what types are already in scope
- type set is atomic for all graph builders of the scope
- what renders must be created for the scope

When creating a render we must track:
- What the path of render is (where to place it in namespace/module/file)
- What associated renders there are (are there transient renders?)

When rendering a reference, we need:
- an anchored graph.

So first - build transient scope graphs.
Build concrete scope graphs.
Build anchored scope graphs.
Render anchored scope graphs.
*)


module TypeString =
    let [<Literal>] unit = "unit"
    let [<Literal>] string = "string"
    let [<Literal>] number = "float"
    let [<Literal>] int = "int"
    let [<Literal>] float = number
    let [<Literal>] bigint = "bigint"
    let [<Literal>] bool = "bool"
    let [<Literal>] obj = "obj"
    let [<Literal>] objNull = "objnull"
    let [<Literal>] globalThis = "Browser.Dom.Window"
    let [<Literal>] keyof = "keyof"
    let [<Literal>] array = "Array"
    let [<Literal>] option = "option"


// force construction via our render stores so that we can be assured
// of transient path tracking
type TypeRefAtom =
    | Widget of string
    | ConcretePath of TypePath
    | TransientPath of TransientTypePath

type TypeRefMolecule =
    | Tuple of TypeRefRender list 
    | Union of TypeRefRender list 
    | Function of parameters: TypeRefRender list * returnType: TypeRefRender
    | Prefix of prefix: TypeRefRender * args: TypeRefRender list

and TypeRefKind =
    | Atom of TypeRefAtom
    | Molecule of TypeRefMolecule 

and TypeRefRender = {
    Kind: TypeRefKind
    Nullable: bool
}

type TypeRefRenderer = {
    ModuleScope: ModulePath
    AnchorPath: AnchorPath
}

type TypeRefSignal = IReadOnlySignal<TypeRefRender>

module TypeRefRenderer =
    let inline create anchorPath =
        let anchorPath = AnchorPath.create anchorPath
        let modulePath, _ = AnchorPath.traceToParentModule anchorPath
        { ModuleScope = modulePath; AnchorPath = anchorPath }

module TypeRefRender =
    module Atom =
        type SRTPHelper =
            static member inline Create(widget: string) =
                TypeRefAtom.Widget widget
            static member inline Create(path: TypePath) =
                TypeRefAtom.ConcretePath path
            static member inline Create(transientPath: TransientTypePath) =
                TypeRefAtom.TransientPath transientPath
            static member inline Create(atom: TypeRefAtom) = atom
    module Molecule =
        type SRTPHelper =
            static member inline Create(tuple: TypeRefRender list) =
                TypeRefMolecule.Union tuple
            static member inline Create(union: TypeRefRender array) =
                TypeRefMolecule.Tuple (Array.toList union)
            static member inline Create(parameters: TypeRefRender list, returnType: TypeRefRender) =
                TypeRefMolecule.Function (parameters, returnType)
            static member inline Create(prefix: TypeRefRender, args: TypeRefRender list) =
                TypeRefMolecule.Prefix (prefix, args)
            static member inline Create(molecule: TypeRefMolecule) = molecule
    module Kind =
        type SRTPHelper =
            static member inline Create(atom: TypeRefAtom) = TypeRefKind.Atom atom
            static member inline Create(molecule: TypeRefMolecule) = TypeRefKind.Molecule molecule
            static member inline Create(tuple: TypeRefRender list) = SRTPHelper.Create(Molecule.SRTPHelper.Create(tuple))
            static member inline Create(union: TypeRefRender array) =SRTPHelper.Create(Molecule.SRTPHelper.Create(union))
            static member inline Create(parameters: TypeRefRender list * TypeRefRender) =SRTPHelper.Create(Molecule.SRTPHelper.Create(fst parameters, snd parameters))
            static member inline Create(prefix: TypeRefRender * TypeRefRender list) =SRTPHelper.Create(Molecule.SRTPHelper.Create(fst prefix, snd prefix))
            static member inline Create(widget: string) = SRTPHelper.Create(Atom.SRTPHelper.Create(widget))
            static member inline Create(path: TypePath) = SRTPHelper.Create(Atom.SRTPHelper.Create(path))
            static member inline Create(transientPath: TransientTypePath) = SRTPHelper.Create(Atom.SRTPHelper.Create(transientPath))
    
    let inline create<^T when (^T or Kind.SRTPHelper):(static member Create: ^T -> TypeRefKind)> nullable (value: ^T) =
        { Nullable = nullable
          Kind = ((^T or Kind.SRTPHelper): (static member Create: ^T -> TypeRefKind) value) }

    let rec private flatten (render: TypeRefRender list) : TypeRefRender list =
        render
        |> List.collect (function
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = true } ->
                { Kind = TypeRefKind.Atom (TypeRefAtom.Widget TypeString.unit); Nullable = false }
                :: flatten atoms
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = false } ->
                flatten atoms
            | { Nullable = true } as render ->
                [ { render with Nullable = false }
                  { Kind = TypeRefKind.Atom (TypeRefAtom.Widget TypeString.unit)
                    Nullable = false } ]
            | render -> [ render ]
            )
    let orNullable (value: bool) (typeRefRender: TypeRefRender) = { typeRefRender with Nullable = value || typeRefRender.Nullable }
    let setNullable (value: bool) (typeRefRender: TypeRefRender) = { typeRefRender with Nullable = value }
    let nullable (typeRefRender: TypeRefRender) = setNullable true typeRefRender
    let nonNullable (typeRefRender: TypeRefRender) = setNullable false typeRefRender

type RenderScopeStore = DictionarySignal<ResolvedType, TypeRefRender>

[<Struct>]
type RenderTraits =
    | Optional
    | ParamArray
    | Static
    | Readable
    | Writable
    | Literal
    | JSGetter
    | JSSetter
    | JSIndexer
    | JSConstructor
    | JSCallSignature
    | EmitSelf
    | Inline
    | StringBuilder

type RenderMetadata =
    | Concrete of AnchorPath
    | Transient of TransientPath

module RenderMetadata =
    type SRTPHelper =
        static member inline Create(anchorPath: AnchorPath) = Concrete anchorPath
        static member inline Create(transientPath: TransientPath) = Transient transientPath
        static member inline Create(metadata: RenderMetadata) = metadata
    let inline create value =
        let ir = ((^T or AnchorPath.SRTPHelper or TransientPath.SRTPHelper or SRTPHelper):(static member Create: ^T -> ^U) value)
        ((^U or SRTPHelper):(static member Create: ^U -> RenderMetadata) ir)

type IAnchoredName<^T, [<Measure>] ^U when ^T:(member WithName: Name<^U> -> ^T) and ^T:(member Metadata: RenderMetadata)> = ^T

module IAnchoredName =
    let inline anchorMember (anchorPath: AnchorPath) (value: IAnchoredName<^T, Case.camel>): IAnchoredName<^T, Case.camel> option =
        let metadata = value.Metadata
        match metadata with
        | Concrete _ -> Some value
        | Transient transientPath ->
            TransientPath.anchor anchorPath transientPath
            |> AnchorPath.traceToParentModule
            |> snd |> List.tryLast
            |> Option.map (Name.Camel.fromName >> value.WithName)
    let inline anchorType (anchorPath: AnchorPath) (value: IAnchoredName<^T, Case.pascal>): IAnchoredName<^T, Case.pascal> option =
        let metadata = value.Metadata
        match metadata with
        | Concrete _ -> Some value
        | Transient transientPath ->
            TransientPath.anchor anchorPath transientPath
            |> AnchorPath.traceToParentModule
            |> snd |> List.tryLast
            |> Option.map (Name.Pascal.fromName >> value.WithName)

type TypeParameterRender = {
    Metadata: RenderMetadata
    Name: Name<Case.typar>
    Constraint: TypeRefSignal voption
    Default: TypeRefSignal voption
    Documentation: TsComment list
} with member this.WithName name = { this with Name = name }

type TypedNameRender = {
    Metadata: RenderMetadata
    Name: Name<Case.camel>
    Type: TypeRefSignal
    Traits: RenderTraits Set
    TypeParameters: TypeParameterRender list
    Documentation: TsComment list
} with member this.WithName name = { this with Name = name }

type FunctionLikeSignature = {
    Metadata: RenderMetadata
    Parameters: TypedNameRender list
    ReturnType: TypeRefSignal
    Traits: RenderTraits Set
    Documentation: TsComment list
    TypeParameters: TypeParameterRender list
}

type FunctionLikeRender = {
    Metadata: RenderMetadata
    Name: Name<Case.camel>
    Signatures: FunctionLikeSignature list
    Traits: RenderTraits Set
    TypeParameters: TypeParameterRender list
    Documentation: TsComment list
} with member this.WithName name = { this with Name = name }

type LiteralCaseRender<'Value> = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    Value: 'Value
    Documentation: TsComment list
}

type LiteralUnionRender<'Value> = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    Cases: LiteralCaseRender<'Value> list
    Documentation: TsComment list
}

type TypeLikeRender = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender list
    Inheritance: TypeRefSignal list
    Members: TypedNameRender list
    Functions: FunctionLikeRender list
    Constructors: TypedNameRender list list
    Documentation: TsComment list
}

type TypeAliasRender =
    | Alias of TypeAliasRenderRef
    | TypeDefn of TypeLikeRender
    | StringUnion of LiteralUnionRender<TsLiteral>
    | EnumUnion of LiteralUnionRender<int>
    | Function of FunctionLikeRender

and TypeAliasRenderRef = {
    Metadata: RenderMetadata
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender list
    Documentation: TsComment list
    Type: TypeRefSignal
}

type TypeRender =
    | TypeDefn of TypeLikeRender
    | TypeAlias of TypeAliasRender
    | StringUnion of LiteralUnionRender<TsLiteral>
    | EnumUnion of LiteralUnionRender<int>
    | Function of FunctionLikeRender
    | Variable of TypedNameRender

type MemberRender =
    | Property of TypedNameRender
    | Method of FunctionLikeRender

type Render =
    | RefOnly of TypeRefRender
    | Render of TypeRefRender * TypeRender

module Render =
    type SRTPHelper =
        static member inline CreateRef(value: TypeRefRender) = value
        static member inline CreateRef(value: TypeRefSignal) = value.Value
    let inline createRef typeRef = RefOnly ((^T or SRTPHelper): (static member CreateRef: ^T -> TypeRefRender) typeRef)
    let inline create typeRef (typeRender: TypeRender) = Render (
        ((^T or SRTPHelper):(static member CreateRef: ^T -> TypeRefRender) typeRef),
        typeRender
        )
    
type RenderScope = {
    Type: ResolvedType
    Root: TypeLikePath voption
    TypeRef: TypeRefSignal
    // we calculate the render lazily
    Render: Render
    TransientChildren: RenderScopeStore
}

type RenderScopeFunc = ResolvedType -> RenderScope voption

module RenderScope =
    type SRTPHelper =
        static member inline CreateRoot(value) = TypeLikePath.Anchored value |> ValueSome
        static member inline CreateRoot(value) = TypeLikePath.Transient value |> ValueSome
        static member inline CreateRoot(value): TypeLikePath voption = ValueSome value
        static member inline CreateRoot(value): TypeLikePath voption = value
        
    let private dummyStore = DictionarySignal<ResolvedType, TypeRefRender>()
    let inline createWithFields resolvedType root typeRef render store = {
        Type = resolvedType
        Root = root
        TypeRef = typeRef
        Render = render
        TransientChildren = store
    }
    
    let linkWithDummyStore (resolvedType: ResolvedType) (typeRefRender: TypeRefSignal) =
        createWithFields resolvedType ValueNone typeRefRender (Render.RefOnly typeRefRender.Value) dummyStore
    let createWithDummyStore resolvedType typeRefRender =
        typeRefRender |> Signal.linkWithValue
        |> linkWithDummyStore resolvedType
    let link resolvedType typeRefRender =
        createWithFields resolvedType ValueNone typeRefRender (Render.RefOnly typeRefRender.Value) (RenderScopeStore())
    let create resolvedType typeRefRender =
        typeRefRender |> Signal.linkWithValue
        |> link resolvedType
    let inline linkWithRoot resolvedType root typeRefRender =
        createWithFields resolvedType (
            (^T or SRTPHelper):(static member CreateRoot: ^T -> TypeLikePath voption) root
            ) typeRefRender (Render.RefOnly typeRefRender.Value) (RenderScopeStore())
    let inline createWithRoot resolvedType root typeRefRender =
        typeRefRender |> Signal.linkWithValue
        |> linkWithRoot resolvedType root
    let createDefaultWithDummyStore resolvedType =
        TypeRefRender.create true TypeString.obj
        |> createWithDummyStore resolvedType
    let createDefault resolvedType =
        TypeRefRender.create true TypeString.obj
        |> create resolvedType
        