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

module Interceptors =
    let inline shouldIgnoreRender (interceptor: Interceptors) (value: ^T when ^T:(member Source: ArenaInterner.QualifiedNamePart option) and ^T:(member FullyQualifiedName: ArenaInterner.QualifiedNamePart list)) =
        Option.exists interceptor.IgnorePathRender.Source value.Source
        ||
        getQualifiedName value
        |> interceptor.IgnorePathRender.QualifiedName
    let shouldIgnoreExport (interceptor: Interceptors) (value: ResolvedExport) =
        match value with
        | ResolvedExport.Variable value -> shouldIgnoreRender interceptor value
        | ResolvedExport.Interface value -> shouldIgnoreRender interceptor value
        | ResolvedExport.TypeAlias value -> shouldIgnoreRender interceptor value
        | ResolvedExport.Class value -> shouldIgnoreRender interceptor value
        | ResolvedExport.Enum value -> shouldIgnoreRender interceptor value
        | ResolvedExport.Function value -> shouldIgnoreRender interceptor value[0]
        | ResolvedExport.Module value -> shouldIgnoreRender interceptor value
    let pipeInterface (ctx: GeneratorContext) (iface: Interface) =
        (Choice1Of4 iface, fromInterface iface)
        ||> ctx.Customisation.Interceptors.Paths.TypePaths ctx
    let pipeClass (ctx: GeneratorContext) (cls: Class) =
        (Choice3Of4 cls, fromClass cls)
        ||> ctx.Customisation.Interceptors.Paths.TypePaths ctx
    let pipeEnum (ctx: GeneratorContext) (enum: EnumType) =
        (Choice2Of4 enum, fromEnum enum)
        ||> ctx.Customisation.Interceptors.Paths.TypePaths ctx
    let pipeTypeAlias (ctx: GeneratorContext) (typeAlias: TypeAlias) =
        (Choice4Of4 typeAlias, fromTypeAlias typeAlias)
        ||> ctx.Customisation.Interceptors.Paths.TypePaths ctx
    let pipeVariable (ctx: GeneratorContext) (variable: Variable) =
        (Choice1Of2 variable, fromVariable variable)
        ||> ctx.Customisation.Interceptors.Paths.MemberPaths ctx
    let pipeFunction (ctx: GeneratorContext) (function': Function) =
        (Choice2Of2 function', fromFunction function')
        ||> ctx.Customisation.Interceptors.Paths.MemberPaths ctx
    let pipeExport (ctx: GeneratorContext) (export: ResolvedExport) =
        match export with
        | ResolvedExport.Variable variable ->
            pipeVariable ctx variable
            |> AnchorPath.create
        | ResolvedExport.Interface ``interface`` ->
            pipeInterface ctx ``interface``
            |> AnchorPath.create
        | ResolvedExport.TypeAlias typeAlias ->
            pipeTypeAlias ctx typeAlias
            |> AnchorPath.create
        | ResolvedExport.Class ``class`` ->
            pipeClass ctx ``class``
            |> AnchorPath.create
        | ResolvedExport.Enum enumType ->
            pipeEnum ctx enumType
            |> AnchorPath.create
        | ResolvedExport.Function functions ->
            pipeFunction ctx functions[0]
            |> AnchorPath.create
        | ResolvedExport.Module ``module`` ->
            ``module``
            |> fromModule
            |> AnchorPath.create
    