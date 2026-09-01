/// Tier 4 - Render: F# source text plus the fidelity manifest, from the shaped model alone.
/// The printer is generator-owned (decision O2): no formatter dependency, golden stability
/// over delegated style, and the compile gate absorbs the correctness risk. The tier's
/// invariant is byte-identical output for an identical model - nothing here may consult the
/// clock, the environment, or hash order.
module Xantham.Generator.Render

open System.Text.Json
open Xantham.TypeScript.Wire.Proto

/// F# keywords and reserved words that force backticks when a JavaScript name collides.
let private keywords =
    Set.ofList
        [ "abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default"; "delegate"
          "do"; "done"; "downcast"; "downto"; "elif"; "else"; "end"; "exception"; "extern"
          "false"; "finally"; "fixed"; "for"; "fun"; "function"; "global"; "if"; "in"
          "inherit"; "inline"; "interface"; "internal"; "lazy"; "let"; "match"; "member"
          "module"; "mutable"; "namespace"; "new"; "not"; "null"; "of"; "open"; "or"
          "override"; "private"; "public"; "rec"; "return"; "select"; "static"; "struct"
          "then"; "to"; "true"; "try"; "type"; "upcast"; "use"; "val"; "void"; "when"
          "while"; "with"; "yield"; "atomic"; "break"; "checked"; "component"; "const"
          "constraint"; "constructor"; "continue"; "eager"; "event"; "external"; "functor"
          "include"; "method"; "mixin"; "object"; "parallel"; "process"; "protected"; "pure"
          "sealed"; "tailcall"; "trait"; "virtual" ]

let private identifierShaped =
    System.Text.RegularExpressions.Regex @"^[A-Za-z_][A-Za-z0-9_']*$"

/// Source names are kept verbatim (mapping doc §4.14); anything F# rejects as an identifier is
/// backticked rather than renamed.
let ident (name: string) =
    if Set.contains name keywords || not (identifierShaped.IsMatch name) then
        $"``{name}``"
    else
        name

let rec printType =
    function
    | FsBool -> "bool"
    | FsString -> "string"
    | FsFloat -> "float"
    | FsUnit -> "unit"
    | FsObj -> "obj"
    | FsOption inner -> $"{printType inner} option"
    | FsArray element -> $"{printType element}[]"
    // Delegates guarantee arity at the Fable boundary (D5): `Action` when nothing is
    // returned, `Func` otherwise.
    | FsDelegate([], FsUnit) -> "Action"
    | FsDelegate(args, FsUnit) -> args |> List.map printType |> String.concat ", " |> sprintf "Action<%s>"
    | FsDelegate(args, ret) ->
        args @ [ ret ] |> List.map printType |> String.concat ", " |> sprintf "Func<%s>"
    // A name may be qualified into another group's templated module (O7); each segment
    // escapes on its own.
    | FsNamed name -> name.Split '.' |> Array.map ident |> String.concat "."

/// An F# string literal with the escapes source text needs.
let stringLit (text: string) =
    let escaped =
        text
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")

    $"\"{escaped}\""

/// A literal as attribute-argument source text: `CompiledName`/`CompiledValue` payloads.
let printLiteral =
    function
    | LitString text -> stringLit text
    | LitBool true -> "true"
    | LitBool false -> "false"
    | LitNumber value when System.Double.IsInteger value && abs value < 2147483648.0 ->
        string (int value)
    | LitNumber value -> value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

let private xmlEscape (text: string) =
    text.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

let private splitLines (text: string) = text.Replace("\r\n", "\n").Split '\n'

/// JSDoc as XML docs: the comment as `<summary>`, each tag as a `<remarks>` line or block.
/// The tier annotation lands in the manifest, not here.
let private docLines (indent: string) (docs: string) (tags: JSDocTagInfo list) =
    [ let summary = docs.Trim()

      if summary <> "" then
          yield $"{indent}/// <summary>"

          for line in splitLines summary do
              yield $"{indent}/// {xmlEscape line}".TrimEnd()

          yield $"{indent}/// </summary>"

      for tag in tags do
          let text = tag.Text |> ValueOption.defaultValue ""

          match splitLines text with
          | [| single |] ->
              let content = if single = "" then $"@{tag.Name}" else $"@{tag.Name} {single}"
              yield $"{indent}/// <remarks>{xmlEscape content}</remarks>"
          | lines ->
              yield $"{indent}/// <remarks>"
              yield $"{indent}/// @{tag.Name}"

              for line in lines do
                  yield $"{indent}/// {xmlEscape line}".TrimEnd()

              yield $"{indent}/// </remarks>" ]

/// A parameter of a static emission (`Exports` members, `Create` overloads): F# optional
/// syntax, `[<ParamArray>]` on a rest tail.
let private renderParam (parameter: FsParam) =
    if parameter.Rest then
        $"[<ParamArray>] {ident parameter.Name}: {printType parameter.Type}"
    elif parameter.Optional then
        let element =
            match parameter.Type with
            | FsOption inner -> inner
            | other -> other

        $"?{ident parameter.Name}: {printType element}"
    else
        $"{ident parameter.Name}: {printType parameter.Type}"

let private renderParamList (parameters: FsParam list) =
    match parameters with
    | [] -> "()"
    | parameters -> parameters |> List.map renderParam |> String.concat ", " |> sprintf "(%s)"

/// A parameter inside an abstract member's signature, where attribute syntax is unavailable -
/// a rest parameter reads as its plain array.
let private renderAbstractParam (parameter: FsParam) =
    if parameter.Optional && not parameter.Rest then
        let element =
            match parameter.Type with
            | FsOption inner -> inner
            | other -> other

        $"?{ident parameter.Name}: {printType element}"
    else
        $"{ident parameter.Name}: {printType parameter.Type}"

let private renderAbstractSignature (parameters: FsParam list) (returns: FsTypeRef) =
    let left =
        match parameters with
        | [] -> "unit"
        | parameters -> parameters |> List.map renderAbstractParam |> String.concat " * "

    $"{left} -> {printType returns}"

let private renderMember (m: FsMember) =
    match m with
    | FsProperty p ->
        [ yield! docLines "    " p.Docs p.Tags
          let mutability = if p.ReadOnly then "" else " with get, set"
          yield $"    abstract {ident p.Name}: {printType p.Type}{mutability}" ]
    | FsMethod m ->
        [ yield! docLines "    " m.Docs m.Tags
          yield $"    abstract {ident m.Name}: {renderAbstractSignature m.Parameters m.Return}" ]

let private renderInterface (decl: FsInterfaceDecl) =
    [ yield! docLines "" decl.Docs decl.Tags

      // A static Create with a body makes F# infer a class; the attribute keeps the type an
      // interface (and needs default-interface-member runtime support to type-check).
      if not decl.CreateOverloads.IsEmpty then
          yield "[<Interface>]"

      match decl.Inherits, decl.Members, decl.CreateOverloads with
      | [], [], [] ->
          yield $"type {ident decl.Name} ="
          yield "    interface end"
      | inherits, members, creates ->
          yield $"type {ident decl.Name} ="

          for baseRef in inherits do
              yield $"    inherit {printType baseRef}"

          for m in members do
              yield! renderMember m

          // D3/§4.4 construction ergonomics: the ParamObject Create compiles a call into the
          // object literal the TS API expects; `$0` emits the (erased) argument object itself.
          for overload in creates do
              yield "    [<ParamObject; Emit(\"$0\")>]"
              yield $"    static member Create {renderParamList overload} : {ident decl.Name} = jsNative" ]

let private renderStringEnum (decl: FsStringEnumDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
      yield $"type {ident decl.Name} ="

      for case in decl.Cases do
          let attributes =
              [ match case.CompiledName with
                | Some name -> $"CompiledName({stringLit name})"
                | None -> ()
                match case.CompiledValue with
                | Some value -> $"CompiledValue({printLiteral value})"
                | None -> () ]

          match attributes with
          | [] -> yield $"    | {ident case.Name}"
          | attributes -> yield $"""    | [<{String.concat "; " attributes}>] {ident case.Name}""" ]

let private renderEnum (decl: FsEnumDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"type {ident decl.Name} ="

      for name, value in decl.Cases do
          yield $"    | {ident name} = {value}" ]

let private renderAbbrev (decl: FsAbbrevDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"type {ident decl.Name} = {printType decl.Target}" ]

let private renderExports (packageName: string) (members: FsExportMember list) =
    [ yield "/// <summary>The package's value exports, each bound to its import.</summary>"
      yield "[<Erase>]"
      yield "type Exports ="

      for m in members do
          yield! docLines "    " m.Docs m.Tags

          let importName =
              match m.Binding with
              | ImportDefault -> "default"
              | ImportNamed name -> name

          match m.Body with
          | ExportFunction(parameters, returns) ->
              yield $"    [<Import({stringLit importName}, {stringLit packageName})>]"
              yield $"    static member {ident m.Name} {renderParamList parameters} : {printType returns} = jsNative"
          | ExportValue reference ->
              yield $"    [<Import({stringLit importName}, {stringLit packageName})>]"
              yield $"    static member {ident m.Name}: {printType reference} = jsNative"
          | ExportConstructor(parameters, returns) ->
              yield $"    [<Import({stringLit importName}, {stringLit packageName}); EmitConstructor>]"
              yield $"    static member {ident m.Name} {renderParamList parameters} : {printType returns} = jsNative" ]

/// The one `.fs` file of the walking skeleton: header, opens, declarations in the order the
/// shape tier fixed. `module rec` so declaration order never fights reference order.
let renderSource: Pass<RenderModel> =
    Pass.pure' "render-source" (fun _ model ->
        let body =
            model.Decls
            |> List.map (function
                | FsInterface decl -> renderInterface decl
                | FsStringEnum decl -> renderStringEnum decl
                | FsEnum decl -> renderEnum decl
                | FsAbbrev decl -> renderAbbrev decl
                | FsExports members -> renderExports model.PackageName members)
            |> List.map (String.concat "\n")
            |> String.concat "\n\n"

        let source =
            String.concat
                "\n"
                [ "// <auto-generated>"
                  $"//   Generated by Xantham.Generator from {model.PackageName}."
                  "//   Do not edit by hand - regenerate instead."
                  "// </auto-generated>"
                  $"module rec {model.ModuleName}"
                  ""
                  "open System"
                  "open Fable.Core"
                  "open Fable.Core.JsInterop"
                  ""
                  body
                  "" ]

        { model with
            Files = model.Files @ [ $"{model.ModuleName}.fs", source ] })

/// The top-level symbol a finding belongs to: the qualified name cut at the first member or
/// parameter qualifier.
let private ownerOf (findingSymbol: string) =
    match findingSymbol.IndexOfAny [| '.'; '(' |] with
    | -1 -> findingSymbol
    | cut -> findingSymbol.Substring(0, cut)

/// Per-symbol fidelity: every generated declaration in output order, then any finding subjects
/// that produced no declaration (drops, table-level findings), each with its worst tier.
let symbolTiers (model: RenderModel) : (string * Tier * Finding list) list =
    let grouped = model.Findings |> List.groupBy (fun finding -> ownerOf finding.Symbol) |> Map.ofList

    let declared =
        model.Decls
        |> List.collect (function
            | FsInterface decl -> [ decl.Name ]
            | FsStringEnum decl -> [ decl.Name ]
            | FsEnum decl -> [ decl.Name ]
            | FsAbbrev decl -> [ decl.Name ]
            | FsExports members -> members |> List.map _.Name)
        |> List.distinct

    let row name =
        let findings = grouped |> Map.tryFind name |> Option.defaultValue []

        let tier =
            match findings with
            | [] -> Exact
            | findings -> findings |> List.map _.Tier |> List.max

        name, tier, findings

    let undeclared =
        grouped
        |> Map.toList
        |> List.map fst
        |> List.filter (fun name -> not (List.contains name declared))
        |> List.sort

    List.map row (declared @ undeclared)

let counts (rows: (string * Tier * Finding list) list) =
    let count tier =
        rows |> List.filter (fun (_, rowTier, _) -> rowTier = tier) |> List.length

    { Exact = count Exact
      Ergonomic = count Ergonomic
      Widened = count Widened
      Escape = count Escape }

let private tierLabel =
    function
    | Exact -> "exact"
    | Ergonomic -> "ergonomic"
    | Widened -> "widened"
    | Escape -> "escape"

// The manifest's shape, spelled as records so the property order is fixed by declaration.
type ManifestFinding = { pass: string; tier: string; message: string }

type ManifestCounts = { exact: int; ergonomic: int; widened: int; escape: int }

type ManifestSymbol = { name: string; tier: string; findings: ManifestFinding list }

type Manifest =
    { package: string
      ``module``: string
      counts: ManifestCounts
      symbols: ManifestSymbol list }

let private manifestOptions =
    let options = JsonSerializerOptions(WriteIndented = true)
    options.NewLine <- "\n" // byte-identical output whatever the OS
    options

/// The fidelity report: which pass widened what, and why, per exported symbol.
let renderManifest: Pass<RenderModel> =
    Pass.pure' "render-manifest" (fun _ model ->
        let rows = symbolTiers model
        let tallies = counts rows

        let manifest =
            { package = model.PackageName
              ``module`` = model.ModuleName
              counts =
                { exact = tallies.Exact
                  ergonomic = tallies.Ergonomic
                  widened = tallies.Widened
                  escape = tallies.Escape }
              symbols =
                [ for name, tier, findings in rows ->
                      { name = name
                        tier = tierLabel tier
                        findings =
                          [ for finding in findings |> List.sortBy (fun f -> f.Pass, f.Symbol, f.Message) ->
                                { pass = finding.Pass
                                  tier = tierLabel finding.Tier
                                  message = finding.Message } ] } ] }

        let json = JsonSerializer.Serialize(manifest, manifestOptions) + "\n"

        { model with
            Files = model.Files @ [ "manifest.json", json ] })

/// The tier's pass list, in execution order.
let passes: Pass<RenderModel> list = [ renderSource; renderManifest ]
