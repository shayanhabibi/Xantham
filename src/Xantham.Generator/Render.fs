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
          "include"; "method"; "mixin"; "object"; "parallel"; "params"; "process"; "protected"
          "pure"; "sealed"; "tailcall"; "trait"; "virtual"
          // Inherited from OCaml: keywords rather than operators, so they need backticks too.
          "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "sig" ]

let private identifierShaped =
    System.Text.RegularExpressions.Regex @"^[A-Za-z_][A-Za-z0-9_']*$"

/// Source names are kept verbatim (mapping doc §4.14); anything F# rejects as an identifier is
/// backticked rather than renamed.
let ident (name: string) =
    if Set.contains name keywords || not (identifierShaped.IsMatch name) then
        $"``{name}``"
    else
        name

/// A dotted templated name (O7), escaped one segment at a time - the dots are ours, not part
/// of any identifier.
let private qualified (name: string) =
    name.Split '.' |> Array.map ident |> String.concat "."

/// `atomic` is true in a position that binds tighter than `*` - an array element, an option's
/// argument, a generic argument. Only a tuple cares: everything else already prints as one
/// term, while `a * b` reassociates unless it is parenthesised there.
let rec private printTypeIn (atomic: bool) =
    function
    | FsBool -> "bool"
    | FsString -> "string"
    | FsFloat -> "float"
    | FsUnit -> "unit"
    | FsObj -> "obj"
    | FsOption inner -> $"{printTypeIn true inner} option"
    | FsArray element -> $"{printTypeIn true element}[]"
    | FsTuple components ->
        let text = components |> List.map (printTypeIn true) |> String.concat " * "
        if atomic then $"({text})" else text
    // Fable's erased unions (D4): the arity names the type, so `U2`-`U4` need no threshold
    // check here - the shape tier never builds a wider one.
    | FsErasedUnion arms ->
        let text = arms |> List.map (printTypeIn true) |> String.concat ", "
        $"U{arms.Length}<{text}>"
    // Delegates guarantee arity at the Fable boundary (D5): `Action` when nothing is
    // returned, `Func` otherwise.
    | FsDelegate([], FsUnit) -> "Action"
    | FsDelegate(args, FsUnit) ->
        args |> List.map (printTypeIn true) |> String.concat ", " |> sprintf "Action<%s>"
    | FsDelegate(args, ret) ->
        args @ [ ret ] |> List.map (printTypeIn true) |> String.concat ", " |> sprintf "Func<%s>"
    | FsTypeVar name -> $"'{name}"
    // A brand (§4.6, D11): `string<UserId>` for the non-numeric primitives, through the
    // support package's measure-annotated abbreviations, and an ordinary measure application
    // for numbers. Both erase to the primitive, which is all the JavaScript ever sees.
    | FsBranded(primitive, measure) -> $"{printTypeIn true primitive}<{qualified measure}>"
    | FsApp(name, arguments) ->
        let text = arguments |> List.map (printTypeIn true) |> String.concat ", "
        $"{qualified name}<{text}>"
    // A name may be qualified into another group's templated module (O7); each segment
    // escapes on its own.
    | FsNamed name -> qualified name

let printType = printTypeIn false

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

/// A line of doc prose, XML-escaped, with its markdown code spans as `<c>`. A span opens on a
/// run of backticks and closes on a run of the same length - so a span can carry backticks of
/// its own - and a run that never closes is prose, which is what a lone backtick in a sentence
/// nearly always is. Multi-line spans are not recognised; the caller works a line at a time.
let private inlineCode (line: string) =
    let ticksAt index =
        let mutable last = index

        while last < line.Length && line[last] = '`' do
            last <- last + 1

        last - index

    // The closing run must be exactly as long as the opening one, so a shorter or longer run
    // in between is content and the scan carries on past it.
    let closingRun opening index =
        let mutable index = index
        let mutable found = -1

        while found < 0 && index < line.Length do
            match ticksAt index with
            | 0 -> index <- index + 1
            | run when run = opening -> found <- index
            | run -> index <- index + run

        found

    let rendered = System.Text.StringBuilder()
    let mutable index = 0
    let mutable prose = 0

    while index < line.Length do
        match ticksAt index with
        | 0 -> index <- index + 1
        | opening ->
            match closingRun opening (index + opening) with
            | -1 -> index <- index + opening
            | closing ->
                rendered.Append(xmlEscape line[prose .. index - 1]) |> ignore
                rendered.Append($"<c>{xmlEscape line[index + opening .. closing - 1]}</c>") |> ignore
                index <- closing + opening
                prose <- index

    rendered.Append(xmlEscape line[prose ..]).ToString()

/// A markdown fence line: three or more backticks, and whatever info string follows them.
let private (|CodeFence|_|) (line: string) =
    let trimmed = line.Trim()
    let ticks = trimmed |> Seq.takeWhile ((=) '`') |> Seq.length

    if ticks >= 3 then
        Some(ticks, trimmed.Substring(ticks).Trim())
    else
        None

/// The body of a doc comment, XML-escaped, with markdown fences rewritten as `<code>` blocks -
/// JSDoc is markdown, XML docs are not, and a fence left alone reads as three backticks in
/// every tooltip. The info string's first word, where there is one, becomes `lang`. A fence
/// left open by the comment closes at its end, because unbalanced XML breaks the consumers.
let private docBody (indent: string) (lines: string seq) =
    // Inside a block every character is already code, backticks included; outside it a code
    // span becomes `<c>`.
    let escaped (line: string) = $"{indent}/// {xmlEscape line}".TrimEnd()
    let prose (line: string) = $"{indent}/// {inlineCode line}".TrimEnd()

    let opener (info: string) =
        match info.Split([| ' '; '\t' |]) |> Array.head with
        | "" -> "<code>"
        | language -> $"""<code lang="{xmlEscape (language.Replace("\"", ""))}">"""

    let rec walk fence lines =
        match lines with
        | [] -> if fence > 0 then [ $"{indent}/// </code>" ] else []
        | line :: rest ->
            match line, fence with
            | CodeFence(ticks, info), 0 -> $"{indent}/// {opener info}" :: walk ticks rest
            // Markdown closes a block on a bare fence at least as long as the one that opened
            // it; anything else inside the block is code, backticks and all.
            | CodeFence(ticks, ""), _ when ticks >= fence -> $"{indent}/// </code>" :: walk 0 rest
            | line, 0 -> prose line :: walk 0 rest
            | line, _ -> escaped line :: walk fence rest

    walk 0 (List.ofSeq lines)

/// JSDoc as XML docs: the comment as `<summary>`, each tag as a `<remarks>` line or block.
/// The tier annotation lands in the manifest, not here.
let private docLines (indent: string) (docs: string) (tags: JSDocTagInfo list) =
    [ let summary = docs.Trim()

      if summary <> "" then
          yield $"{indent}/// <summary>"
          yield! docBody indent (splitLines summary)
          yield $"{indent}/// </summary>"

      for tag in tags do
          let text = tag.Text |> ValueOption.defaultValue ""

          match splitLines text with
          | [| single |] ->
              let content = if single = "" then $"@{tag.Name}" else $"@{tag.Name} {single}"
              yield $"{indent}/// <remarks>{inlineCode content}</remarks>"
          | lines ->
              yield $"{indent}/// <remarks>"
              yield $"{indent}/// @{tag.Name}"
              yield! docBody indent lines
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

/// A declaration's name with its type parameters and their constraints (§4.9), as written at
/// the point of definition: `Box<'T>`, `Node<'T when 'T :> Element>`. A generic *member*
/// writes its own parameters the same way - `abstract read<'K> : ...`.
let private declHead (name: string) (typeParameters: FsTypeParam list) =
    if typeParameters.IsEmpty then
        ident name
    else
        let parameters =
            typeParameters
            |> List.map (fun p ->
                match p.Constraint with
                | Some bound -> $"'{p.Name} when '{p.Name} :> {printTypeIn true bound}"
                | None -> $"'{p.Name}")
            |> String.concat ", "

        $"{ident name}<{parameters}>"

/// The same declaration written at a reference position, where the parameters appear bare:
/// `Box<'T>`. A constraint belongs to the definition only, so it is not repeated here.
let private declRef (name: string) (typeParameters: FsTypeParam list) =
    if typeParameters.IsEmpty then
        ident name
    else
        let parameters = typeParameters |> List.map (fun p -> $"'{p.Name}") |> String.concat ", "
        $"{ident name}<{parameters}>"

let private renderMember (m: FsMember) =
    match m with
    | FsProperty p ->
        [ yield! docLines "    " p.Docs p.Tags
          let mutability = if p.ReadOnly then "" else " with get, set"
          yield $"    abstract {ident p.Name}: {printType p.Type}{mutability}" ]
    | FsMethod m ->
        [ yield! docLines "    " m.Docs m.Tags
          yield $"    abstract {declHead m.Name m.TypeParameters}: {renderAbstractSignature m.Parameters m.Return}" ]
    | FsIndexer i ->
        // `[<EmitIndexer>]` is what makes this reach JavaScript as `bag[key]` rather than a
        // method call; the member must be named `Item` for F# indexer syntax to bind to it.
        [ yield "    [<EmitIndexer>]"
          let mutability = if i.ReadOnly then "" else " with get, set"
          yield $"    abstract Item: {printType i.Key} -> {printType i.Value}{mutability}" ]

let private renderInterface (decl: FsInterfaceDecl) =
    [ yield! docLines "" decl.Docs decl.Tags

      // A static Create with a body makes F# infer a class; the attribute keeps the type an
      // interface (and needs default-interface-member runtime support to type-check).
      if not decl.CreateOverloads.IsEmpty then
          yield "[<Interface>]"

      match decl.Inherits, decl.Members, decl.CreateOverloads with
      | [], [], [] ->
          yield $"type {declHead decl.Name decl.TypeParameters} ="
          yield "    interface end"
      | inherits, members, creates ->
          yield $"type {declHead decl.Name decl.TypeParameters} ="

          for baseRef in inherits do
              yield $"    inherit {printType baseRef}"

          for m in members do
              yield! renderMember m

          // D3/§4.4 construction ergonomics: the ParamObject Create compiles a call into the
          // object literal the TS API expects; `$0` emits the (erased) argument object itself.
          for overload in creates do
              yield "    [<ParamObject; Emit(\"$0\")>]"
              yield
                  $"    static member Create {renderParamList overload} : {declRef decl.Name decl.TypeParameters} = jsNative" ]

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

/// A tagged union (D4, §4.5(2)). `RequireQualifiedAccess` for the same reason the StringEnum
/// emission takes it: one generated module holds every declaration a package has, and bare
/// case names collide across unions - and here with the member interfaces the cases carry.
let private renderTaggedUnion (decl: FsTaggedUnionDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"[<RequireQualifiedAccess; TypeScriptTaggedUnion({stringLit decl.Tag}, CaseRules.None)>]"
      yield $"type {ident decl.Name} ="

      for case in decl.Cases do
          // Named fields, so the JS keys survive: Fable emits each field under its own name.
          let fields =
              case.Fields
              |> List.map (fun field -> $"{ident field.Name}: {printTypeIn true field.Type}")
              |> String.concat " * "

          let carries = if case.Fields.IsEmpty then "" else $" of {fields}"

          match case.CompiledName with
          | Some tag -> yield $"    | [<CompiledName({stringLit tag})>] {ident case.Name}{carries}"
          | None -> yield $"    | {ident case.Name}{carries}" ]

let private renderEnum (decl: FsEnumDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"type {ident decl.Name} ="

      for name, value in decl.Cases do
          yield $"    | {ident name} = {value}" ]

let private renderAbbrev (decl: FsAbbrevDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"type {declHead decl.Name decl.TypeParameters} = {printType decl.Target}" ]

/// The unit of measure a branding intersection becomes (§4.6, D11). A measure has no body:
/// the name is the whole of it, and what it brands is written at the uses as `string<Name>`.
/// The primitive is recorded in the doc comment because the declaration itself cannot say it.
let private renderMeasure (decl: FsMeasureDecl) =
    [ yield! docLines "" decl.Docs decl.Tags
      yield $"/// <remarks>A brand over <c>{printType decl.Primitive}</c>.</remarks>"
      yield "[<Measure>]"
      yield $"type {ident decl.Name}" ]

/// A declaration whose right-hand side is a type-level computation F# has no way to reproduce -
/// a mapped or conditional type, or a template literal over an operand the checker left open
/// (§4.10, §4.11). The name and the arity survive, so uses of it stay distinct from one another
/// and from `obj`; the single private case means a cast is the only way in or out.
let private renderPhantom (decl: FsPhantomDecl) =
    // Named after the type it carries, so the cases of two phantoms never collide in the one
    // module a package generates into.
    let case = ident (decl.Name + "__")

    [ yield! docLines "" decl.Docs decl.Tags
      yield "[<Erase>]"
      yield $"type {declHead decl.Name decl.TypeParameters} = private {case} of {printType decl.Carrier}" ]

/// The import name a default export binds under - the JavaScript key, not an F# name.
let private defaultExportKey = "default"

let private renderExports (packageName: string) (members: FsExportMember list) =
    [ yield "/// <summary>The package's value exports, each bound to its import.</summary>"
      yield "[<Erase>]"
      yield "type Exports ="

      for m in members do
          yield! docLines "    " m.Docs m.Tags

          // The binding attribute, optionally carrying a second attribute inside the same
          // brackets: a global is named off `globalThis` and imports nothing.
          let attribute (also: string) =
              let package = stringLit packageName

              match m.Binding with
              | ImportDefault -> $"    [<Import({stringLit defaultExportKey}, {package}){also}>]"
              | ImportNamed name -> $"    [<Import({stringLit name}, {package}){also}>]"
              | GlobalName name -> $"    [<Global({stringLit name}){also}>]"

          match m.Body with
          | ExportFunction(parameters, returns) ->
              yield attribute ""
              yield
                  $"    static member {declHead m.Name m.TypeParameters} {renderParamList parameters} : {printType returns} = jsNative"
          | ExportValue reference ->
              yield attribute ""
              yield $"    static member {ident m.Name}: {printType reference} = jsNative"
          | ExportConstructor(parameters, returns) ->
              yield attribute "; EmitConstructor"
              yield
                  $"    static member {declHead m.Name m.TypeParameters} {renderParamList parameters} : {printType returns} = jsNative" ]

/// The one `.fs` file of the walking skeleton: header, opens, declarations in the order the
/// shape tier fixed. `module rec` so declaration order never fights reference order.
let renderSource: Pass<RenderModel> =
    Pass.pure' "render-source" (fun _ model ->
        let body =
            model.Decls
            |> List.map (function
                | FsInterface decl -> renderInterface decl
                | FsStringEnum decl -> renderStringEnum decl
                | FsTaggedUnion decl -> renderTaggedUnion decl
                | FsEnum decl -> renderEnum decl
                | FsAbbrev decl -> renderAbbrev decl
                | FsMeasure decl -> renderMeasure decl
                | FsPhantom decl -> renderPhantom decl
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
                  "open Xantham.Fable.Core"
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
            | FsTaggedUnion decl -> [ decl.Name ]
            | FsEnum decl -> [ decl.Name ]
            | FsAbbrev decl -> [ decl.Name ]
            | FsMeasure decl -> [ decl.Name ]
            | FsPhantom decl -> [ decl.Name ]
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
