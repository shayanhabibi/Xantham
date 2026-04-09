module Xantham.Generator.TypeRefRender

open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.Generator.NamePath

// type ref renders are either directly widgets, or are
// paths to the type

/// <summary>
/// <para>Building block, or sole component of any type reference.<br/>
/// Essentially, either a precomposed widget, or a path.</para>
/// <para>Paths can be either <c>transient</c> or <c>anchored</c> (i.e. concrete)</para>
/// </summary>
type [<RequireQualifiedAccess>] TypeRefAtom =
    | Widget of WidgetBuilder<Type>
    | AnchorPath of TypePath
    | TransientPath of TransientTypePath

/// <summary>
/// <para>Composite reference types such as function signatures, tuples, unions, or
/// type prefix applications.</para>
/// <para>They are composed of <c>TypeRefRender</c> so that the concept of <c>nullability</c>
/// is available to a molecule in isolation. This provides enough information to independently
/// handle optionality in each of its cases.</para>
/// </summary>
type [<RequireQualifiedAccess>] TypeRefMolecule =
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
    

type TypeParameterRender = {
    Name: Name<Case.typar>
    Constraint: TypeRefRender voption
    Default: TypeRefRender voption
    Documentation: TsComment list
}

type TypedNameRender = {
    Name: Name<Case.camel>
    Type: TypeRefRender
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

type FunctionLikeSignature = {
    Parameters: TypedNameRender array
    ReturnType: TypeRefRender
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

type FunctionLikeRender = {
    Name: Name<Case.camel>
    Signatures: FunctionLikeSignature array
    Traits: TypedNameTraits
    TypeParameters: TypeParameterRender array
    Documentation: TsComment list
}

type LiteralCaseRender<'value> = {
    Name: Name<Case.pascal>
    Value: 'value
    Documentation: TsComment list
}

type LiteralUnionRender<'value> = {
    Name: Name<Case.pascal>
    Cases: LiteralCaseRender<'value> array
    Documentation: TsComment list
}

type TypeLikeRender = {
    Name: Name<Case.pascal>
    TypeParameters: TypeParameterRender array
    Members: TypedNameRender array
    Functions: FunctionLikeRender array
    Inheritance: TypeRefRender array
    // each array is a constructor signature
    Constructors: TypedNameRender[][]
    Documentation: TsComment list
}

[<RequireQualifiedAccess>]
type TypeRender =
    | TypeDefn of TypeLikeRender
    | StringUnion of LiteralUnionRender<TsLiteral>
    | EnumUnion of LiteralUnionRender<int>
    | Function of FunctionLikeRender
    | Variable of TypedNameRender

[<RequireQualifiedAccess>]
type Render =
    | RefOnly of TypeRefRender
    | Render of TypeRefRender * TypeRender

module private Implementation =
    let rec isFunctionRender (render: TypeRefRender) =
        match render.Kind with
        | TypeRefKind.Molecule (TypeRefMolecule.Function _) -> true
        | _ -> false
    let renderAtom (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Widget widgetBuilder ->
            widgetBuilder
        | TypeRefAtom.AnchorPath typePath ->
            TypePath.flatten typePath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
        | TypeRefAtom.TransientPath transientTypePath ->
            TransientTypePath.toAnchored transientTypePath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
    let rec renderMolecule (molecule: TypeRefMolecule) =
        match molecule with
        | TypeRefMolecule.Tuple typeRefRenders ->
            typeRefRenders
            |> List.map render
            |> Ast.Tuple
        | TypeRefMolecule.Union typeRefRenders ->
            typeRefRenders
            |> List.map render
            |> function
                | [] -> Ast.Unit()
                | [ widget ] -> widget
                | types ->
                    let length = List.length types
                    let prefix = Ast.Anon $"U{length}"
                    Ast.AppPrefix(prefix, types)
        // if we have no parameters, then we render a unit function
        | TypeRefMolecule.Function([], returnType) ->
            let returnType = render returnType
            Ast.Funs(Ast.Unit(), returnType)
        | TypeRefMolecule.Function(parameters, returnType) ->
            let parameters =
                parameters
                |> List.map (fun ref ->
                    render ref
                    |> if isFunctionRender ref
                        then Ast.Paren
                        else id)
            let returnType = render returnType
            Ast.Funs(parameters, returnType)
        | TypeRefMolecule.Prefix(prefix, args) ->
            let isNullable = prefix.Nullable
            let prefix = render { prefix with Nullable = false }
            let args = args |> List.map render
            Ast.AppPrefix(prefix, args)
            |> if isNullable then Ast.OptionPrefix else id

    and render (typeRefRender: TypeRefRender): WidgetBuilder<Type> =
        match typeRefRender.Kind with
        | TypeRefKind.Atom typeRefAtom ->
            renderAtom typeRefAtom
        | TypeRefKind.Molecule typeRefMolecule ->
            renderMolecule typeRefMolecule
        |> if typeRefRender.Nullable then
            Ast.OptionPrefix 
            else id
       

[<EditorBrowsable(EditorBrowsableState.Never)>]
module TestHelpers =
    let simpleRender (typeRefRender: TypeRefRender): WidgetBuilder<Type> = Implementation.render typeRefRender

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
        static member inline Create(value: WidgetBuilder<Type>) = createWidget value
        static member inline Create(value: TypePath) = createAnchorPath value
        static member inline Create(value: TransientTypePath) = createTransientPath value
    
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefAtom) value)

    let render (atom: TypeRefAtom) = Implementation.renderAtom atom

module TypeRefMolecule =
    let createTuple (atoms: TypeRefRender list) =
        TypeRefMolecule.Tuple atoms
    let createUnion (atoms: TypeRefRender list) =
        TypeRefMolecule.Union atoms
    let createFunction (returnValue: TypeRefRender) (atoms: TypeRefRender list) =
        TypeRefMolecule.Function(atoms, returnValue)
    let createPrefix (prefix: TypeRefRender) (args: TypeRefRender list) =
        TypeRefMolecule.Prefix(prefix, args)
        
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        /// <summary>
        /// For SRTP purposes, we delineate a union from a tuple construction by the type of the collection.
        /// </summary>
        static member inline Create(values: TypeRefRender array) = createTuple (Array.toList values)
        static member inline Create(values: TypeRefRender list) = createUnion values
        static member inline Create(value: TypeRefRender list * TypeRefRender) = createFunction (snd value) (fst value)
        static member inline Create(prefix: TypeRefRender, args: TypeRefRender list) = createPrefix prefix args
        
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefMolecule) value)
    
    let render (molecule: TypeRefMolecule) = Implementation.renderMolecule molecule

module TypeRefKind =
    let createWidget (widget: WidgetBuilder<Type>) =
        TypeRefKind.Atom (TypeRefAtom.createWidget widget)
    let createAnchorPath (path: TypePath) =
        TypeRefKind.Atom (TypeRefAtom.createAnchorPath path)
    let createTransientPath (path: TransientTypePath) =
        TypeRefKind.Atom (TypeRefAtom.createTransientPath path)
    let createUnion (atoms: TypeRefRender list) =
        TypeRefKind.Molecule (TypeRefMolecule.createUnion atoms)
    let createFunction (returnValue: TypeRefRender) (atoms: TypeRefRender list) =
        TypeRefKind.Molecule (TypeRefMolecule.createFunction returnValue atoms)
    let createPrefix (prefix: TypeRefRender) (args: TypeRefRender list) =
        TypeRefKind.Molecule (TypeRefMolecule.createPrefix prefix args)
    let createTuple (atoms: TypeRefRender list) =
        TypeRefKind.Molecule (TypeRefMolecule.createTuple atoms)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(value: WidgetBuilder<Type>) = createWidget value
        static member inline Create(value: TypePath) = createAnchorPath value
        static member inline Create(value: TransientTypePath) = createTransientPath value
        static member inline Create(values: TypeRefRender list) = createUnion values
        static member inline Create(value: TypeRefRender list * TypeRefRender) = createFunction (snd value) (fst value)
        static member inline Create(prefix: TypeRefRender, args: TypeRefRender list) = createPrefix prefix args
        static member inline Create(values: TypeRefRender array) = createTuple (Array.toList values)
    
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TypeRefKind) value)

module TypeRefRender =
    // flattens a list of renders by recursively seeking out and expanding unions.
    let rec private flatten (render: TypeRefRender list) =
        render
        |> List.collect (function
            | { Kind = TypeRefKind.Molecule (TypeRefMolecule.Union atoms); Nullable = nullable' } ->
                { Kind = TypeRefKind.createWidget (Ast.Unit()); Nullable = false }
                :: flatten atoms
            | { Nullable = true } as render ->
                [ { render with Nullable = false }; { Kind = TypeRefKind.createWidget (Ast.Unit()); Nullable = false } ]
            | render -> [ render ]
            )
    let inline createWithKind nullable kind = {
        Kind = kind
        Nullable = nullable
    }
    let createWidget nullable widget =
        TypeRefKind.createWidget widget
        |> createWithKind nullable
    let createAnchorPath nullable path =
        TypeRefKind.createAnchorPath path
        |> createWithKind nullable
    let createTransientPath nullable path =
        TypeRefKind.createTransientPath path
        |> createWithKind nullable
    let createUnion nullable atoms =
        flatten atoms
        |> TypeRefKind.createUnion 
        |> createWithKind nullable
    let createFunction nullable returnValue atoms =
        TypeRefKind.createFunction returnValue atoms
        |> createWithKind nullable
    let createPrefix nullable prefix args =
        TypeRefKind.createPrefix prefix args
        |> createWithKind nullable
    let createTuple nullable atoms =
        TypeRefKind.createTuple atoms
        |> createWithKind nullable
    let inline orNullable value typeRefRender = { typeRefRender with Nullable = typeRefRender.Nullable || value }
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
    let inline setNullable value typeRefRender = { typeRefRender with Nullable = value }
    let nullable = setNullable true
    let nonNullable = setNullable false
    
    let rec localisePaths (anchorPath: AnchorPath) (typeRefRender: TypeRefRender) =
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

    let rec anchor anchorPath typeRefRender =
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
    
    let render (typeRefRender: TypeRefRender) = Implementation.render typeRefRender
                
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(tuple, nullable) = createTuple nullable (List.ofArray tuple)
        static member inline Create(widget, nullable) = createWidget nullable widget
        static member inline Create(path, nullable) = createAnchorPath nullable path
        static member inline Create(path, nullable) = createTransientPath nullable path
        static member inline Create(unions, nullable) = createUnion nullable unions
        static member inline Create(funcValues, nullable) = createFunction nullable (snd funcValues) (fst funcValues)
        static member inline Create(prefixValues, nullable) = createPrefix nullable (fst prefixValues) (snd prefixValues)
    
    let inline create nullable (value: ^T) =
        ((^T or SRTPHelper): (static member Create: ^T * bool -> TypeRefRender) (value, nullable))

module TypeRender =
    let createDefn (typeLike: TypeLikeRender) =
        TypeRender.TypeDefn typeLike
    let createStringUnion (union: LiteralUnionRender<TsLiteral>) =
        TypeRender.StringUnion union
    let createEnumUnion (union: LiteralUnionRender<int>) =
        TypeRender.EnumUnion union
    let createFunction (func: FunctionLikeRender) =
        TypeRender.Function func
    let createVariable (variable: TypedNameRender) =
        TypeRender.Variable variable
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(typeDefn: TypeLikeRender) = createDefn typeDefn
        static member inline Create(stringUnion: LiteralUnionRender<TsLiteral>) = createStringUnion stringUnion
        static member inline Create(enumUnion: LiteralUnionRender<int>) = createEnumUnion enumUnion
        static member inline Create(func: FunctionLikeRender) = createFunction func
        static member inline Create(variable: TypedNameRender) = createVariable variable
    
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TypeRender) value)

module Render =
    let makeRefOnly (render: TypeRefRender) =
        Render.RefOnly render
    let makeWithRefRender (typeRefRender: TypeRefRender) (typeRender: TypeRender) =
        Render.Render (typeRefRender, typeRender)
        
    let inline createRefOnly nullable (refRender: ^T) =
        TypeRefRender.create nullable refRender
        |> makeRefOnly
    /// <summary>
    /// </summary>
    /// <param name="nullable"></param>
    /// <param name="refRender"></param>
    /// <param name="render"></param>
    let inline create nullable (refRender: ^T) (render: ^U) =
        let refRender = TypeRefRender.create nullable refRender
        let render = TypeRender.create render
        makeWithRefRender refRender render
        

    