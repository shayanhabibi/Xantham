[<RequireQualifiedAccess>]
module Path

open Fable.Core.JsInterop
open Node.Api
open TypeScript


module private Literals =
    [<Literal>]
    let nodeModulesSpecifier = "node_modules"
    
type FileExtension =
    | Unknown of string
    | Extension of Ts.Extension
    static member inline Create(ext: string) =
        match unbox ext with
        | Ts.Extension.Ts -> Extension Ts.Extension.Ts
        | Ts.Extension.Tsx -> Extension Ts.Extension.Tsx
        | Ts.Extension.Dts -> Extension Ts.Extension.Dts
        | Ts.Extension.Js -> Extension Ts.Extension.Js
        | Ts.Extension.Jsx -> Extension Ts.Extension.Jsx
        | Ts.Extension.Json -> Extension Ts.Extension.Json
        | Ts.Extension.Mjs -> Extension Ts.Extension.Mjs
        | Ts.Extension.Mts -> Extension Ts.Extension.Mts
        | Ts.Extension.Dmts -> Extension Ts.Extension.Dmts
        | Ts.Extension.Cjs -> Extension Ts.Extension.Cjs
        | Ts.Extension.Cts -> Extension Ts.Extension.Cts
        | Ts.Extension.Dcts -> Extension Ts.Extension.Dcts
        | ext when ext = Ts.Extension.TsBuildInfo -> Extension Ts.Extension.TsBuildInfo
        | ext -> unbox ext |> Unknown
    static member inline op_Implicit(this: FileExtension): string = this.Value
    member inline this.Value = emitJsExpr this "$0.fields[0]"
    member inline this.Length = this.Value.Length

let inline fileExtension path =
    match Node.Api.path.extname path with
    | "" | Null -> ValueNone
    | ext -> FileExtension.Create ext |> ValueSome
    
let inline extname path = fileExtension path

let inline dir path = Node.Api.path.dirname path |> Measures.String.add<Measures.String.directory>

let inline basename path = Node.Api.path.basename path
let inline combine path1 path2 = path.join(path1, path2)
let inline combine3 path1 path2 path3 = path.join(path1, path2, path3)
let inline combine4 path1 path2 path3 path4 = path.join(path1, path2, path3, path4)

let hasNodeModules (path: string) =
    path.Contains(Literals.nodeModulesSpecifier, System.StringComparison.OrdinalIgnoreCase)

let relativeNodeModules (path: string) =
    if hasNodeModules path then
        path.Substring(
            path.IndexOf(Literals.nodeModulesSpecifier, System.StringComparison.OrdinalIgnoreCase)
            + Literals.nodeModulesSpecifier.Length
        )
    else path
    |> Measures.String.add<Measures.String.nodeModulesRelative>
    
let relativeNodeModulesBack (path: string) =
    if hasNodeModules path then
        path.Substring(
            path.LastIndexOf(Literals.nodeModulesSpecifier, System.StringComparison.OrdinalIgnoreCase)
            + Literals.nodeModulesSpecifier.Length
        )
    else path
    |> Measures.String.add<Measures.String.nodeModulesRelative>

let inferredPackageDirectory (path: string) =
    if not (hasNodeModules path) then Node.Api.path.dirname path |> Measures.String.add<Measures.String.packageDirectory> else
    let pathParts =
        path.Split(Node.Api.path.sep, System.StringSplitOptions.RemoveEmptyEntries ||| System.StringSplitOptions.TrimEntries)
    pathParts
    |> Array.truncate (
        pathParts
        |> Array.findIndexBack _.Equals(Literals.nodeModulesSpecifier, System.StringComparison.OrdinalIgnoreCase)
        |> fun idx ->
            if
                pathParts.Length - idx - 1 > 1
                &&
                pathParts
                |> Array.tryItem (idx + 1)
                |> Option.exists _.Equals("@types", System.StringComparison.OrdinalIgnoreCase)
            then idx + 2
            else idx + 1
        )
    |> String.concat Node.Api.path.sep
    |> Measures.String.add<Measures.String.packageDirectory>
    
let extensionless path =
    let ext = fileExtension path
    ext
    |> ValueOption.map (fun ext ->
        path.Substring(0, path.Length - ext.Length)
        )
    |> ValueOption.defaultValue path
    |> Measures.String.add<Measures.String.extensionless>