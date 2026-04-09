module Xantham.Generator.Generator.Path
open Xantham.Generator.NamePath
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

let inline private getQualifiedName (container: ^T when ^T:(member FullyQualifiedName: string list)) =
    container.FullyQualifiedName
    |> QualifiedNamePart.parse
    |> QualifiedName.create

let inline private createModulePath (qualifiedName: QualifiedName) (source: string option) =
    match source, qualifiedName.MemberPath with
    | None, [] -> ModulePath.init "Global"
    | None, head :: tail ->
        let init = ModulePath.init head
        tail
        |> List.fold (fun acc s -> ModulePath.create s acc) init
    | Some s, path ->
        path
        |> List.fold (fun acc s -> ModulePath.create s acc) (ModulePath.init s)

let fromVariable (variable: Variable) =
    let qualifiedName = getQualifiedName variable
    let source = variable.Source
    let renderName = Name.Case.valueOrModified variable.Name
    let path =
        createModulePath qualifiedName source
        |> MemberPath.createOnModule renderName
    path

let fromInterface (iface: Interface) =
    let qualifiedName = getQualifiedName iface
    let source = iface.Source
    let renderName = Name.Case.valueOrModified iface.Name
    let path =
        createModulePath qualifiedName source
        |> TypePath.create renderName
    path

let fromTypeAlias (typeAlias: TypeAlias) =
    let qualifiedName = getQualifiedName typeAlias
    let source = typeAlias.Source
    let renderName = Name.Case.valueOrModified typeAlias.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromClass (cls: Class) =
    let qualifiedName = getQualifiedName cls
    let source = cls.Source
    let renderName = Name.Case.valueOrModified cls.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromEnum (enum: EnumType) =
    let qualifiedName = getQualifiedName enum
    let source = enum.Source
    let renderName = Name.Case.valueOrModified enum.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromFunction (function': Function) =
    let qualifiedName = getQualifiedName function'
    let source = function'.Source
    let renderName = Name.Case.valueOrModified function'.Name
    createModulePath qualifiedName source
    |> MemberPath.createOnModule renderName

let fromModule (module': Module) =
    let qualifiedName = getQualifiedName module'
    let source = module'.Source
    let renderName = Name.Case.valueOrModified module'.Name
    createModulePath qualifiedName source
    |> ModulePath.create renderName

let fromResolvedExport (resolvedExport: ResolvedExport) =
    match resolvedExport with
    | ResolvedExport.Variable variable -> fromVariable variable |> AnchorPath.Member
    | ResolvedExport.Interface ``interface`` -> fromInterface ``interface`` |> AnchorPath.Type
    | ResolvedExport.TypeAlias typeAlias -> fromTypeAlias typeAlias |> AnchorPath.Type
    | ResolvedExport.Class ``class`` -> fromClass ``class`` |> AnchorPath.Type
    | ResolvedExport.Enum enumType -> fromEnum enumType |> AnchorPath.Type
    | ResolvedExport.Function (func :: _) ->
        fromFunction func |> AnchorPath.Member
    | ResolvedExport.Function [] -> failwith "Resolved export contained no functions for the function case."
    | ResolvedExport.Module ``module`` -> fromModule ``module`` |> AnchorPath.Module

let fromIntersection (intersection: Intersection) =
    TransientTypePath.Anchored

let fromTypeLiteral (typeLiteral: TypeLiteral) =
    TransientTypePath.Anchored

let fromResolvedType (resolvedType: ResolvedType) =
    match resolvedType with
    | ResolvedType.GlobalThis ->
        ModulePath.init "Browser"
        |> ModulePath.create "Dom"
        |> TypePath.create "Window"
    // | ResolvedType.Conditional conditionalType ->
    //     TransientModulePath.Anchored
    | ResolvedType.Interface ``interface`` ->
        fromInterface ``interface``
    | ResolvedType.Class ``class`` -> fromClass ``class``
    | ResolvedType.Primitive typeKindPrimitive -> failwith "todo"
    | ResolvedType.Union union -> failwith "todo"
    | ResolvedType.Intersection intersection -> failwith "todo"
    | ResolvedType.Literal tsLiteral -> failwith "todo"
    | ResolvedType.IndexedAccess indexAccessType -> failwith "todo"
    | ResolvedType.Index index -> failwith "todo"
    | ResolvedType.TypeReference typeReference -> failwith "todo"
    | ResolvedType.Array resolvedType -> failwith "todo"
    | ResolvedType.Enum enumType -> failwith "todo"
    | ResolvedType.EnumCase enumCase -> failwith "todo"
    | ResolvedType.TypeParameter typeParameter -> failwith "todo"
    | ResolvedType.ReadOnly resolvedType -> failwith "todo"
    | ResolvedType.Tuple tuple -> failwith "todo"
    | ResolvedType.Predicate predicate -> failwith "todo"
    | ResolvedType.TypeLiteral typeLiteral -> failwith "todo"
    | ResolvedType.TemplateLiteral templateLiteral -> failwith "todo"
    | ResolvedType.Optional typeReference -> failwith "todo"
    | ResolvedType.Substitution substitutionType -> failwith "todo"