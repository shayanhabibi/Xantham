module Xantham.Generator.Generator.Path

open System.ComponentModel
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

[<EditorBrowsable(EditorBrowsableState.Never)>]
let inline getQualifiedName (container: ^T when ^T:(member FullyQualifiedName: ArenaInterner.QualifiedNamePart list)) =
    container.FullyQualifiedName
    |> QualifiedNamePart.parse
    |> QualifiedName.create

let inline private sanitizeSource (source: string) =
    source.Trim('@').Split([|'\\'; '/'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
let inline private createModulePath (qualifiedName: QualifiedName) (source: ArenaInterner.QualifiedNamePart option) =
    let hasNodeModuleFilePath =
        qualifiedName.FilePath
        |> List.exists _.Contains("node_modules")
    match source, qualifiedName.MemberPath with
    | None, [] -> ModulePath.init "Global"
    | None, head :: tail ->
        let init = ModulePath.init head
        tail
        |> List.fold (fun acc s -> ModulePath.create s acc) init
    // ================================
    // TODO - should this be done?
    // abnormal qualified source name - likely not a 'module' import, but a file import from
    // a declaration file. The qualified name also contains the file path.
    | Some (ArenaInterner.QualifiedNamePart.Abnormal (s, _)), path when hasNodeModuleFilePath ->
        // proceed if the abnormal source name contains the file name from the qualified name
        if s.Contains(System.IO.Path.GetFileNameWithoutExtension qualifiedName.FilePath.Head) then
            // extract path after node_modules for direct file import
            let fileNameParts =
                qualifiedName.FilePath.Head
                |> _.Split("node_modules")
                |> Array.last
                |> _.Split([|'\\'; '/'|], System.StringSplitOptions.RemoveEmptyEntries)
            // change the source name to the first qualifier in the file path
            let s = fileNameParts |> Array.head
            // add the remaining qualifiers to the head of the path
            fileNameParts
            |> Array.tail
            |> Array.toList
            |> List.append
            |> funApply path
            // proceed as normal
            |> List.fold (fun acc s -> ModulePath.create s acc) (ModulePath.init s)
        else
            let s = sanitizeSource s
            path
            |> List.fold (fun acc s -> ModulePath.create s acc) (ModulePath.createFromList s)
    // ==================================
    | Some (ArenaInterner.QualifiedNamePart.Abnormal(s, _) | ArenaInterner.QualifiedNamePart.Normal s), path ->
        let s = sanitizeSource s
        path
        |> List.fold (fun acc s -> ModulePath.create s acc) (ModulePath.createFromList s)

let fromVariable (variable: Variable) =
    let qualifiedName = getQualifiedName variable
    let source = variable.Source
    let renderName = Name.Case.valueOrSource variable.Name
    let path =
        createModulePath qualifiedName source
        |> MemberPath.createOnModule renderName
    path

let fromInterface (iface: Interface) =
    let qualifiedName = getQualifiedName iface
    let source = iface.Source
    let renderName = Name.Case.valueOrSource iface.Name
    let path =
        createModulePath qualifiedName source
        |> TypePath.create renderName
    path

let fromTypeAlias (typeAlias: TypeAlias) =
    let qualifiedName = getQualifiedName typeAlias
    let source = typeAlias.Source
    let renderName = Name.Case.valueOrSource typeAlias.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromClass (cls: Class) =
    let qualifiedName = getQualifiedName cls
    let source = cls.Source
    let renderName = Name.Case.valueOrSource cls.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromEnum (enum: EnumType) =
    let qualifiedName = getQualifiedName enum
    let source = enum.Source
    let renderName = Name.Case.valueOrSource enum.Name
    createModulePath qualifiedName source
    |> TypePath.create renderName

let fromEnumCase (parentPath: TypePath option) (enum: EnumCase) =
    let parentPath =
        parentPath
        |> Option.defaultWith(fun () -> fromEnum enum.Parent.Value)
    parentPath
    |> MemberPath.createOnType (enum.Name |> Name.Case.valueOrSource)
    

let fromFunction (function': Function) =
    let qualifiedName = getQualifiedName function'
    let source = function'.Source
    let renderName = Name.Case.valueOrSource function'.Name
    createModulePath qualifiedName source
    |> MemberPath.createOnModule renderName

let fromModule (module': Module) =
    let qualifiedName = getQualifiedName module'
    let source = module'.Source
    let renderName = Name.Case.valueOrSource module'.Name
    createModulePath qualifiedName source
    |> ModulePath.create renderName

let fromResolvedExport (resolvedExport: ResolvedExport) =
    match resolvedExport with
    | ResolvedExport.Variable variable -> fromVariable variable |> AnchorPath.Member
    | ResolvedExport.Interface ``interface`` -> fromInterface ``interface`` |> AnchorPath.Type
    | ResolvedExport.TypeAlias typeAlias -> fromTypeAlias typeAlias |> AnchorPath.Type
    | ResolvedExport.Class ``class`` -> fromClass ``class`` |> AnchorPath.Type
    | ResolvedExport.Enum enumType -> fromEnum enumType |> AnchorPath.Type
    | ResolvedExport.Module ``module`` -> fromModule ``module`` |> AnchorPath.Module
    | ResolvedExport.Function (func :: _) ->
        fromFunction func |> AnchorPath.Member
    | ResolvedExport.Function [] -> failwith "Resolved export contained no functions for the function case."

let inline private mkRctx (owner: ResolvedType voption) (position: PathPosition) =
    {
        Position = PathPos position
        Owner = owner
        Render = ValueNone
        Stage = RenderStage.PathResolution
    }

/// Runs the `PathResolutionType` slot for a raw `TypePath` produced by one of
/// the type-level `from*` builders (interface/class/enum/typeAlias). Caller
/// supplies the owning `ResolvedType` (or `ValueNone` if not applicable) and
/// the `PathPosition`. `Render` is `ValueNone` because path resolution fires
/// before any `RenderScope` is entered.
let resolveType
    (ctx: GeneratorContext)
    (owner: ResolvedType voption)
    (position: PathPosition)
    (path: TypePath) : SkippableHookResult<TypePath> =
    SkippableHookSlot.run ctx.Customisation.PathResolutionType ctx (mkRctx owner position) path

/// Runs the `PathResolutionMember` slot for a raw `MemberPath` produced by one
/// of the member-level `from*` builders (variable/function or member-of-type
/// paths).
let resolveMember
    (ctx: GeneratorContext)
    (owner: ResolvedType voption)
    (position: PathPosition)
    (path: MemberPath) : SkippableHookResult<MemberPath> =
    SkippableHookSlot.run ctx.Customisation.PathResolutionMember ctx (mkRctx owner position) path

let tryResolveTypePath
    (ctx: GeneratorContext)
    (owner: ResolvedType voption)
    (position: PathPosition)
    (path: TypePath) : TypePath voption =
    match resolveType ctx owner position path with
    | SkippableHookResult.Pass        -> ValueSome path
    | SkippableHookResult.Replace p   -> ValueSome p
    | SkippableHookResult.Skip        -> ValueNone

let tryResolveMemberPath
    (ctx: GeneratorContext)
    (owner: ResolvedType voption)
    (position: PathPosition)
    (path: MemberPath) : MemberPath voption =
    match resolveMember ctx owner position path with
    | SkippableHookResult.Pass        -> ValueSome path
    | SkippableHookResult.Replace p   -> ValueSome p
    | SkippableHookResult.Skip        -> ValueNone

/// Dispatcher convenience: resolves the anchor path for a `ResolvedExport`,
/// returning `ValueNone` when the appropriate `PathResolution*` chain returned
/// `Skip`. Replaces today's `Interceptors.shouldIgnoreExport` short-circuit
/// *and* the kind-keyed `pipe*` family in one call. Modules currently bypass
/// resolution to preserve the legacy behaviour from `pipeExport`.
let tryResolveExport (ctx: GeneratorContext) (export: ResolvedExport) : AnchorPath voption =
    let inline asType (owner: ResolvedType) (raw: TypePath) =
        match resolveType ctx (ValueSome owner) PathPosition.TopLevelType raw with
        | SkippableHookResult.Pass      -> ValueSome (AnchorPath.Type raw)
        | SkippableHookResult.Replace p -> ValueSome (AnchorPath.Type p)
        | SkippableHookResult.Skip      -> ValueNone
    let inline asMember (position: PathPosition) (raw: MemberPath) =
        match resolveMember ctx ValueNone position raw with
        | SkippableHookResult.Pass      -> ValueSome (AnchorPath.Member raw)
        | SkippableHookResult.Replace p -> ValueSome (AnchorPath.Member p)
        | SkippableHookResult.Skip      -> ValueNone
    match export with
    | ResolvedExport.Interface iface     -> asType (ResolvedType.Interface iface) (fromInterface iface)
    | ResolvedExport.Class cls           -> asType (ResolvedType.Class cls)       (fromClass cls)
    | ResolvedExport.Enum enumType       -> asType (ResolvedType.Enum enumType)   (fromEnum enumType)
    // ResolvedType has no `TypeAlias` case; pass `ValueNone` for Owner.
    | ResolvedExport.TypeAlias typeAlias ->
        match resolveType ctx ValueNone PathPosition.TopLevelType (fromTypeAlias typeAlias) with
        | SkippableHookResult.Pass      -> ValueSome (AnchorPath.Type (fromTypeAlias typeAlias))
        | SkippableHookResult.Replace p -> ValueSome (AnchorPath.Type p)
        | SkippableHookResult.Skip      -> ValueNone
    | ResolvedExport.Variable variable   -> asMember PathPosition.VariablePath (fromVariable variable)
    | ResolvedExport.Function (func :: _) -> asMember PathPosition.FunctionPath (fromFunction func)
    | ResolvedExport.Function []         -> failwith "Resolved export contained no functions for the function case."
    | ResolvedExport.Module module'      -> ValueSome (AnchorPath.Module (fromModule module'))