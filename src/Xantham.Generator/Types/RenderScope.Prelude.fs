[<AutoOpen>]
module Xantham.Generator.Types.Prelude

open System
open System.Collections.Generic
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath
open Xantham.Decoder

// of transient path tracking
type TypeRefAtom =
    private
    | Widget_ of WidgetBuilder<Type>
    | Intrinsic_ of string
    | ConcretePath_ of TypePath 
    | TransientPath_ of TransientTypePath 

type TypeRefMolecule =
    private
    | Tuple_ of TypeRefRender list 
    | Union_ of TypeRefRender list 
    | Function_ of parameters: TypeRefRender list * returnType: TypeRefRender
    | Prefix_ of prefix: TypeRefRender * args: TypeRefRender list

and TypeRefKind =
    private
    // nullability is stored on the atom
    | Atom_ of TypeRefAtom
    // nullability is computed once
    | Molecule_ of TypeRefMolecule 

and TypeRefRender = {
    Kind: TypeRefKind
    Nullable: bool
}

module TypeRefAtom =
    let (|Widget|ConcretePath|TransientPath|Intrinsic|) (atom: TypeRefAtom) =
        match atom with
        | Widget_(widgetBuilder) -> Widget(widgetBuilder)
        | ConcretePath_(typePath) -> ConcretePath(typePath)
        | TransientPath_(transientTypePath) -> TransientPath(transientTypePath)
        | Intrinsic_ s -> Intrinsic(s)

module TypeRefMolecule =
    let (|Tuple|Union|Function|Prefix|) (molecule: TypeRefMolecule) =
        match molecule with
        | Tuple_ typeRefs -> Tuple(typeRefs)
        | Union_ typeRefs -> Union(typeRefs)
        | Function_(parameters, returnType) -> Function(parameters, returnType)
        | Prefix_(prefix, args) -> Prefix(prefix, args)

module TypeRefKind =
    let (|Atom|Molecule|) (typeRef: TypeRefKind) =
        match typeRef with
        | Atom_ atom -> Atom(atom)
        | Molecule_ molecule -> Molecule(molecule)

module TypeRefRender =
    let rec private flatten (render: TypeRefRender list) : TypeRefRender list =
        render
        |> List.collect (function
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = true } ->
                { Kind = TypeRefKind.Atom_ (TypeRefAtom.Widget_ (Ast.Unit())); Nullable = false }
                :: flatten atoms
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = false } ->
                flatten atoms
            | { Nullable = true } as render ->
                [ { render with Nullable = false }
                  { Kind = TypeRefKind.Atom_ (TypeRefAtom.Widget_ (Ast.Unit()))
                    Nullable = false } ]
            | render -> [ render ]
            )
    let orNullable (value: bool) (typeRefRender: TypeRefRender) = { typeRefRender with Nullable = value || typeRefRender.Nullable }
    let setNullable (value: bool) (typeRefRender: TypeRefRender) = { typeRefRender with Nullable = value }
    let nullable (typeRefRender: TypeRefRender) = setNullable true typeRefRender
    let nonNullable (typeRefRender: TypeRefRender) = setNullable false typeRefRender

type RenderScopeStore = {
    PathContext: TransientPath
    TypeStore: Dictionary<ResolvedType, TransientTypePath>
}

/// <summary>
/// The strategy here is to force references being created with a contract that would
/// recognise and register any transient type paths that are defined.
/// </summary>
module RenderScopeStore =
    let create () = {
            PathContext = TransientPath.create TransientTypePath.Anchored
            TypeStore = Dictionary<ResolvedType, TransientTypePath>()
        }
    let mapPathContext (fn: TransientPath -> TransientPath) (scope: RenderScopeStore) = { scope with PathContext = fn scope.PathContext }
    let appendStringToPathContext scope (str: string) =
        mapPathContext (
            TransientPath.toTransientModulePath
            >> TransientTypePath.createOnTransientModule str
            >> TransientPath.create
            ) scope
    let appendNameToPathContext scope (name: Name<_>) =
        Name.Case.valueOrSource name
        |> appendStringToPathContext scope
    module TypeRefAtom =
        module Unsafe =
            let createWidget (widget: WidgetBuilder<Type>) =
                TypeRefAtom.Widget_(widget)
            let createConcretePath (path: TypePath) =
                TypeRefAtom.ConcretePath_(path)
            let createTransientPath  (path: TransientTypePath) =
                TypeRefAtom.TransientPath_(path)
            let createIntrinsic (intrinsic: string) =
                TypeRefAtom.Intrinsic_(intrinsic)
            
        let inline createWidget (_scope: RenderScopeStore) (_resolvedType: ResolvedType) (widget: WidgetBuilder<Type>) =
            Unsafe.createWidget widget
        let inline createConcretePath (_scope: RenderScopeStore) (_resolvedType: ResolvedType) (path: TypePath) =
            Unsafe.createConcretePath path
        let inline createIntrinsic (_scope: RenderScopeStore) (_resolvedType: ResolvedType) (intrinsic: string) =
            Unsafe.createIntrinsic intrinsic
        let createTransientPath (scope: RenderScopeStore) (resolvedType: ResolvedType) (path: TransientTypePath) =
            match path with
            | TransientTypePath.Anchored ->
                TransientPath.toTransientModulePath scope.PathContext
                |> TransientTypePath.graft
            | TransientTypePath.Moored(parent, name) ->
                parent
                |> TransientModulePath.graft (TransientPath.toTransientModulePath scope.PathContext)
                |> TransientTypePath.createOnTransientModuleWithName name
            | TransientTypePath.AnchoredAndMoored name ->
                TransientPath.toTransientModulePath scope.PathContext
                |> TransientTypePath.createOnTransientModuleWithName name
            |> fun path ->
                scope.TypeStore.TryAdd(resolvedType, path)
                |> ignore
            // scope.TryAdd(resolvedType, path) |> ignore
            Unsafe.createTransientPath path
        
        type SRTPHelper =
            static member inline Create(scope, resolvedType, widget) = createWidget scope resolvedType widget
            static member inline Create(scope, resolvedType, path) = createConcretePath scope resolvedType path
            static member inline Create(scope, resolvedType, path) = createTransientPath scope resolvedType path
            static member inline Create(scope, resolvedType, _, widget) = createWidget scope resolvedType widget
            static member inline Create(scope, resolvedType, _, path) = createConcretePath scope resolvedType path
            static member inline Create(scope, resolvedType, _, path) = createTransientPath scope resolvedType path
            static member inline Create(scope, resolvedType, intrinsic) = createIntrinsic scope resolvedType intrinsic
            static member inline Create(scope, resolvedType, _, intrinsic) = createIntrinsic scope resolvedType intrinsic
        
    let tryAdd (resolvedType: ResolvedType) (transientTypePath: TransientTypePath) (scope: RenderScopeStore) =
        TypeRefAtom.createTransientPath scope resolvedType transientTypePath |> ignore
            
    module TypeRefMolecule =
        module Unsafe =
            let createTuple (typeRefs: TypeRefRender list) =
                TypeRefMolecule.Tuple_(typeRefs)
            let createUnion (typeRefs: TypeRefRender list) =
                TypeRefMolecule.Union_(typeRefs)
            let createFunction (parameters: TypeRefRender list) (returnType: TypeRefRender) =
                TypeRefMolecule.Function_(parameters, returnType)
            let createPrefix (prefix: TypeRefRender) (args: TypeRefRender list) =
                TypeRefMolecule.Prefix_(prefix, args)
        
        let inline createTuple (_scope: RenderScopeStore) (typeRefs: TypeRefRender list) =
            Unsafe.createTuple typeRefs
        let inline createUnion (_scope: RenderScopeStore) (typeRefs: TypeRefRender list) =
            Unsafe.createUnion typeRefs
        let inline createFunction (_scope: RenderScopeStore) (parameters: TypeRefRender list) (returnType: TypeRefRender) =
            Unsafe.createFunction parameters returnType
        let inline createPrefix (_scope: RenderScopeStore) (prefix: TypeRefRender) (args: TypeRefRender list) =
            Unsafe.createPrefix prefix args
        
        type SRTPHelper =
            static member inline Create(scope, _resolvedType: ResolvedType, typeRefs) = createTuple scope (Array.toList typeRefs)
            static member inline Create(scope, _resolvedType: ResolvedType, typeRefs) = createUnion scope typeRefs
            static member inline Create(scope, _resolvedType: ResolvedType, (parameters, returnType)) = createFunction scope parameters returnType
            static member inline Create(scope, _resolvedType: ResolvedType, (prefix, args)) = createPrefix scope prefix args
            static member inline Create(scope, _resolvedType: ResolvedType, _, typeRefs) = createTuple scope (Array.toList typeRefs)
            static member inline Create(scope, _resolvedType: ResolvedType, _, typeRefs) = createUnion scope typeRefs
            static member inline Create(scope, _resolvedType: ResolvedType, _, (parameters, returnType)) = createFunction scope parameters returnType
            static member inline Create(scope, _resolvedType: ResolvedType, _, (prefix, args)) = createPrefix scope prefix args
            
    
    module TypeRef =
        module Unsafe =
            let createAtom (atom: TypeRefAtom) =
                TypeRefKind.Atom_(atom)
            let createMolecule (molecule: TypeRefMolecule) =
                TypeRefKind.Molecule_(molecule)
        type SRTPHelper =
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, atom) = Unsafe.createAtom atom
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, molecule) = Unsafe.createMolecule molecule
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, typeRef: TypeRefKind) = typeRef
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, _, atom) = Unsafe.createAtom atom
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, _, molecule) = Unsafe.createMolecule molecule
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, _, typeRef: TypeRefKind) = typeRef
        let inline createAtom (scope: RenderScopeStore) (resolvedType: ResolvedType) nullable value =
            ((^T or TypeRefAtom.SRTPHelper):(static member Create: RenderScopeStore * ResolvedType * (bool * ^T) -> TypeRefAtom) scope, resolvedType, (nullable, value))
        let inline create<
            ^T, ^U when
                (^T or TypeRefAtom.SRTPHelper or TypeRefMolecule.SRTPHelper or SRTPHelper):(static member Create: RenderScopeStore * ResolvedType * ^T -> ^U)
                and (^U or SRTPHelper):(static member Create: RenderScopeStore * ResolvedType * ^U -> TypeRefKind
            )
        > (scope: RenderScopeStore) (resolvedType: ResolvedType) (widgetConcreteTransientAtomOrMolecule: ^T) =
            let ir = ((^T or TypeRefAtom.SRTPHelper or TypeRefMolecule.SRTPHelper or SRTPHelper):(static member Create: RenderScopeStore * ResolvedType * ^T -> ^U) scope, resolvedType, widgetConcreteTransientAtomOrMolecule)
            ((^U or SRTPHelper):(static member Create: RenderScopeStore * ResolvedType * ^U -> TypeRefKind) scope, resolvedType, ir)
            
    module TypeRefRender =
        
        module Unsafe =
            let createFromKind (nullable: bool) (typeRef: TypeRefKind) =
                { Kind = typeRef; Nullable = nullable }
                
        let createKind (_scope: RenderScopeStore) (_resolvedType: ResolvedType) (nullable: bool) (typeRef: TypeRefKind) =
            Unsafe.createFromKind nullable typeRef
            
        type SRTPHelper =
            static member inline Create(scope: RenderScopeStore, resolvedType: ResolvedType, nullable: bool, typeRef: TypeRefKind) = createKind scope resolvedType nullable typeRef
            static member inline Create(_scope: RenderScopeStore, _resolvedType: ResolvedType, nullable: bool, typeRef: TypeRefRender) = { typeRef with Nullable = nullable }
        
        /// <summary>
        /// <code>
        /// let inline srtpFunc value =
        ///     RenderScopeStore.TypeRefRender.create scope resolvedType nullable value
        /// 
        /// // Tuple type
        /// typeRefs : TypeRefRender array
        /// |> srtpFunc
        ///
        /// 
        ///  // Union type
        /// typeRefs : TypeRefRender list
        /// |> srtpFunc
        ///
        /// 
        ///  // Function type
        /// (parameters, returnType) : (TypeRefRender list * TypeRefRender)
        /// |> srtpFunc
        ///
        /// 
        ///  // Prefix type
        /// (prefix, typeArgs) : (TypeRefRender * TypeRefRender list)
        /// |> srtpFunc
        ///
        /// 
        ///  // Transient path ref
        /// path : TransientTypePath
        /// |> srtpFunc
        ///
        /// 
        ///  // Concrete path ref
        /// path: TypePath
        /// |> srtpFunc
        ///
        /// 
        ///  // Widget
        /// widget: WidgetBuilder&lt;Type>
        /// |> srtpFunc
        ///
        /// 
        ///  // an atom
        /// atom: TypeRefAtom
        /// |> srtpFunc
        /// 
        ///
        ///  // a molecule
        /// molecule: TypeRefMolecule
        /// |> srtpFunc
        /// 
        ///
        ///  // a type ref
        /// typeRef: TypeRef
        /// |> srtpFunc
        /// </code>
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="resolvedType"></param>
        /// <param name="nullable"></param>
        /// <param name="data">
        /// </param>
        let inline create (scope: RenderScopeStore) (resolvedType: ResolvedType) (nullable: bool) (data: ^T) =
            let irAtomMoleculeMaybe = (
                (^T or TypeRefAtom.SRTPHelper or TypeRefMolecule.SRTPHelper or TypeRef.SRTPHelper or SRTPHelper):(
                static member Create: RenderScopeStore * ResolvedType * bool * ^T -> ^U
                ) scope, resolvedType, nullable, data)
            let irRefMaybe = (
                (^U or TypeRef.SRTPHelper or SRTPHelper):(
                static member Create: RenderScopeStore * ResolvedType * bool * ^U -> ^V
                ) scope, resolvedType, nullable, irAtomMoleculeMaybe)
            (
                (^V or SRTPHelper):(
                static member Create: RenderScopeStore * ResolvedType * bool * ^V -> TypeRefRender
            ) scope, resolvedType, nullable, irRefMaybe)

type RenderScope<'RootPathType, 'RenderType> = {
    Type: ResolvedType
    Root: 'RootPathType
    TypeRef: TypeRefRender
    // we calculate the render lazily
    Render: 'RenderType
    TransientChildren: RenderScopeStore voption
}

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

    

[<Struct>]
type RenderMetadata = {
    Path: Path
    Original: Path
    Source: ArenaInterner.QualifiedNamePart voption
    FullyQualifiedName: ArenaInterner.QualifiedNamePart list voption
}

type TypeParameterRender<'RenderType, 'TyparName> = {
    Metadata: RenderMetadata
    Name: 'TyparName
    Constraint: 'RenderType voption
    Default: 'RenderType voption
    Documentation: TsComment list
}

type TypedNameRender<'RenderType, 'MemberName, 'TyparName> = {
    Metadata: RenderMetadata
    Name: 'MemberName
    Type: 'RenderType
    Traits: RenderTraits Set
    TypeParameters: TypeParameterRender<'RenderType, 'TyparName> list
    Documentation: TsComment list
}

type FunctionLikeSignature<'RenderType, 'MemberName, 'TyparName> = {
    Metadata: RenderMetadata
    Parameters: TypedNameRender<'RenderType, 'MemberName, 'TyparName> list
    ReturnType: 'RenderType
    Traits: RenderTraits Set
    Documentation: TsComment list
    TypeParameters: TypeParameterRender<'RenderType, 'TyparName> list
}

type FunctionLikeRender<'RenderType, 'MemberName, 'TyparName> = {
    Metadata: RenderMetadata
    Name: 'MemberName
    Signatures: FunctionLikeSignature<'RenderType, 'MemberName, 'TyparName> list
    Traits: RenderTraits Set
    TypeParameters: TypeParameterRender<'RenderType, 'TyparName> list
    Documentation: TsComment list
}

type LiteralCaseRender<'Value, 'TypeName> = {
    Metadata: RenderMetadata
    Name: 'TypeName
    Value: 'Value
    Documentation: TsComment list
}

type LiteralUnionRender<'Value, 'TypeName> = {
    Metadata: RenderMetadata
    Name: 'TypeName
    Cases: LiteralCaseRender<'Value, 'TypeName> list
    Documentation: TsComment list
}

type TypeLikeRender<'RenderType, 'TypeName, 'MemberName, 'TyparName> = {
    Metadata: RenderMetadata
    Name: 'TypeName
    TypeParameters: TypeParameterRender<'RenderType, 'TyparName> list
    Inheritance: 'RenderType list
    Members: TypedNameRender<'RenderType, 'MemberName, 'TyparName> list
    Functions: FunctionLikeRender<'RenderType, 'MemberName, 'TyparName> list
    Constructors: TypedNameRender<'RenderType, 'MemberName, 'TyparName> list list
    Documentation: TsComment list
}

type TypeAliasRender<'RenderType, 'TypeName, 'MemberName, 'TyparName> =
    | Alias of TypeAliasRenderRef<'RenderType, 'TypeName, 'TyparName>
    | TypeDefn of TypeLikeRender<'RenderType, 'TypeName, 'MemberName, 'TyparName>
    | StringUnion of LiteralUnionRender<TsLiteral, 'TypeName>
    | EnumUnion of LiteralUnionRender<int, 'TypeName>
    | Function of FunctionLikeRender<'RenderType, 'MemberName, 'TyparName>

and TypeAliasRenderRef<'RenderType, 'TypeName, 'TyparName> = {
    Metadata: RenderMetadata
    Name: 'TypeName
    TypeParameters: TypeParameterRender<'RenderType, 'TyparName> list
    Documentation: TsComment list
    Type: 'RenderType
}

type TypeRender<'RenderType, 'TypeName, 'MemberName, 'TyparName> =
    | TypeDefn of TypeLikeRender<'RenderType, 'TypeName, 'MemberName, 'TyparName>
    | TypeAlias of TypeAliasRender<'RenderType, 'TypeName, 'MemberName, 'TyparName>
    | StringUnion of LiteralUnionRender<TsLiteral, 'TypeName>
    | EnumUnion of LiteralUnionRender<int, 'TypeName>
    | Function of FunctionLikeRender<'RenderType, 'MemberName, 'TyparName>
    | Variable of TypedNameRender<'RenderType, 'MemberName, 'TyparName>

type MemberRender<'RenderType, 'MemberName, 'TyparName> =
    | Property of TypedNameRender<'RenderType, 'MemberName, 'TyparName>
    | Method of FunctionLikeRender<'RenderType, 'MemberName, 'TyparName>

type RenderKind<'RenderType, 'TypeName, 'MemberName, 'TyparName> = 'RenderType * Lazy<TypeRender<'RenderType, 'TypeName, 'MemberName, 'TyparName>>

module Transient =
    type TypeName = Name<Case.pascal> voption
    type MemberName = Name<Case.camel>
    type TyparName = Name<Case.typar>
    type TypeParameterRender = TypeParameterRender<TypeRefRender, TyparName>
    type TypedNameRender = TypedNameRender<TypeRefRender, MemberName, TyparName>
    type FunctionLikeSignature = FunctionLikeSignature<TypeRefRender, MemberName, TyparName>
    type FunctionLikeRender = FunctionLikeRender<TypeRefRender, MemberName, TyparName>
    type LiteralCaseRender<'T> = LiteralCaseRender<'T, TypeName>
    type LiteralUnionRender<'T> = LiteralUnionRender<'T, TypeName>
    type TypeLikeRender = TypeLikeRender<TypeRefRender, TypeName, MemberName, TyparName>
    type TypeAliasRender = TypeAliasRender<TypeRefRender, TypeName, MemberName, TyparName>
    type TypeAliasRenderRef = TypeAliasRenderRef<TypeRefRender, TypeName, TyparName>
    type TypeRender = TypeRender<TypeRefRender, TypeName, MemberName, TyparName>
    type MemberRender = MemberRender<TypeRefRender, MemberName, TyparName>
    type Render = RenderKind<TypeRefRender, TypeName, MemberName, TyparName>
    type RenderScope = RenderScope<TransientTypePath, Render>
    type RenderScopeFunc = ResolvedType -> RenderScope voption

module Concrete =
    type TypeName = Name<Case.pascal>
    type MemberName = Name<Case.camel>
    type TyparName = Name<Case.typar>
    type TypeParameterRender = TypeParameterRender<TypeRefRender, TyparName>
    type TypedNameRender = TypedNameRender<TypeRefRender, MemberName, TyparName>
    type FunctionLikeSignature = FunctionLikeSignature<TypeRefRender, MemberName, TyparName>
    type FunctionLikeRender = FunctionLikeRender<TypeRefRender, MemberName, TyparName>
    type LiteralCaseRender<'T> = LiteralCaseRender<'T, TypeName>
    type LiteralUnionRender<'T> = LiteralUnionRender<'T, TypeName>
    type TypeLikeRender = TypeLikeRender<TypeRefRender, TypeName, MemberName, TyparName>
    type TypeAliasRender = TypeAliasRender<TypeRefRender, TypeName, MemberName, TyparName>
    type TypeAliasRenderRef = TypeAliasRenderRef<TypeRefRender, TypeName, TyparName>
    type TypeRender = TypeRender<TypeRefRender, TypeName, MemberName, TyparName>
    type MemberRender = MemberRender<TypeRefRender, MemberName, TyparName>
    type Render = RenderKind<TypeRefRender, TypeName, MemberName, TyparName>

type Render =
    | RefOnly of TypeRefRender
    | Concrete of Concrete.Render
    | Transient of Transient.Render

type RenderScope = RenderScope<TypeLikePath voption, Render>

module Render =
    type SRTPHelper =
        static member Create(typeRef: TypeRefRender, render) = Render.Concrete(typeRef, render)
        static member Create(typeRef: TypeRefRender, render) = Render.Transient(typeRef, render)
        static member Create(typeRef: TypeRefRender, renderer) = Render.Concrete(typeRef, lazy renderer())
        static member Create(typeRef: TypeRefRender, renderer) = Render.Transient(typeRef, lazy renderer())
        static member Create(typeRef: TypeRefRender, render) = Render.Concrete(typeRef, lazy render)
        static member Create(typeRef: TypeRefRender, render) = Render.Transient(typeRef, lazy render)
        static member Create(typeRef: TypeRefRender) = Render.RefOnly(typeRef)
    let inline createRefOnly (typeRef: TypeRefRender) = SRTPHelper.Create(typeRef)
    let inline createFromConcreteLazy (typeRef: TypeRefRender) (render: Lazy<Concrete.TypeRender>) = SRTPHelper.Create(typeRef, render)
    let inline createFromTransientLazy (typeRef: TypeRefRender) (render: Lazy<Transient.TypeRender>) = SRTPHelper.Create(typeRef, render)
    let inline createFromConcrete (typeRef: TypeRefRender) (render: Concrete.TypeRender) = SRTPHelper.Create(typeRef, render)
    let inline createFromTransient (typeRef: TypeRefRender) (render: Transient.TypeRender) = SRTPHelper.Create(typeRef, render)
    let inline create (typeRef: TypeRefRender) renderOrRenderer =
        ((^T or SRTPHelper):(static member Create: TypeRefRender * ^T -> Render) typeRef, renderOrRenderer)

module RenderScope =
    let private dummyStore = RenderScopeStore.create()
    let createRootless resolvedType (typeRef: TypeRefRender): RenderScope =
        {
            Type = resolvedType
            Root = ValueNone
            TypeRef = typeRef
            Render = Render.RefOnly typeRef
            TransientChildren = ValueNone
        }

module RenderMetadata =
    let withSourceOption (source: ArenaInterner.QualifiedNamePart option) metadata =
        { metadata with RenderMetadata.Source = ValueOption.ofOption source }
    let withSource (source: ArenaInterner.QualifiedNamePart) metadata =
        { metadata with RenderMetadata.Source = ValueSome source }
    let withSourceString (source: string) (metadata: RenderMetadata) =
        withSource (ArenaInterner.QualifiedNamePart.Normal source) metadata
    let withFullyQualifiedName (fullyQualifiedName: ArenaInterner.QualifiedNamePart list) metadata =
        { metadata with RenderMetadata.FullyQualifiedName = ValueSome fullyQualifiedName }
    let withFullyQualifiedNameStrings (fullyQualifiedName: string list) (metadata: RenderMetadata) =
        withFullyQualifiedName (fullyQualifiedName |> List.map ArenaInterner.QualifiedNamePart.Normal) metadata
    let create (path: Path) (original: Path) (source: ArenaInterner.QualifiedNamePart voption) (fullyQualifiedName: ArenaInterner.QualifiedNamePart list voption) =
        {
            Path = path
            Original = original
            Source = source
            FullyQualifiedName = fullyQualifiedName
        }
    let createWithPath (path: Path) =
        create path path ValueNone ValueNone
    let createWithOriginalPath (original: Path) (path: Path) =
        create path original ValueNone ValueNone
    let createWithTransientPath = Path.createTransient >> createWithPath
    let createWithAnchorPath = Path.createAnchor >> createWithPath
    let createWithOriginalTransientPath original = Path.createTransient >> createWithOriginalPath (Path.createTransient original)
    let createWithOriginalAnchorPath original = Path.createAnchor >> createWithOriginalPath (Path.createAnchor original)
    let inline createWithPathFromExport<^T
        when ^T: (member FullyQualifiedName: ArenaInterner.QualifiedNamePart list)
        and ^T: (member Source: ArenaInterner.QualifiedNamePart option)
        and ^T: (member IsLibEs: bool)
        > (path: Path) (export: ^T) =
        if export.IsLibEs && export.Source |> Option.exists (_.Value >> _.Equals("typescript", StringComparison.OrdinalIgnoreCase)) then
            createWithPath path
            |> withFullyQualifiedName export.FullyQualifiedName
        else
            create path path (ValueOption.ofOption export.Source) (ValueSome export.FullyQualifiedName)
            