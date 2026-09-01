module Xantham.TypeScript.Packages
open Fable.Core
open Xantham.Fable
open Xantham.TypeScript
open Xantham.Annotations

type [<Measure>] exportPath
type [<Measure>] packageName
type [<Measure>] packageVersion
type [<Measure>] packageSubModule

[<TypeScriptTaggedUnion("path"); RequireQualifiedAccess>]
type ConditionalExport =
    | Types of value: ExportValue
    | Default of value: ExportValue
    | Browser of value: ExportValue
    | Development of value: ExportValue
    | Production of value: ExportValue
    | NodeAddons of value: ExportValue
    | Node of value: ExportValue
    | Import of value: ExportValue
    | Require of value: ExportValue
    | ModuleSync of value: ExportValue
    | Module of value: ExportValue
    | ESNext of value: ExportValue
    | Unknown of key: string * value: ExportValue

and [<RequireQualifiedAccess; Erase>] ExportValue =
    | String of string
    | Conditional of NonEmptyArray<ConditionalExport>
    
type PackageInfo = {
    Name: string<packageName>
    Version: string<packageVersion>
    SourceFiles: Node.SourceKind array
}

[<Erase>]
type PackageId = PackageId of name: string<packageName> * version: string<packageVersion>

[<Erase>]
type SubModuleId = SubModuleId of name: string<packageName> * version: string<packageVersion> * subModuleName: string<packageSubModule>

type Export = Map<string<exportPath>, ExportValue>