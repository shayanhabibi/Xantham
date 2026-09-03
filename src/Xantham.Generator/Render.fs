/// Tier 4 - Render: F# source text plus the fidelity manifest, from the shaped model alone.
/// The printer is generator-owned (decision O2): no formatter dependency, golden stability
/// over delegated style, and the compile gate absorbs the correctness risk. The tier's
/// invariant is byte-identical output for an identical model - nothing here may consult the
/// clock, the environment, or hash order.
module Xantham.Generator.Render

open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open Xantham.TypeScript.Wire.Proto

/// F# keywords and reserved words that force backticks when a JavaScript name collides.
let private keywords =
    Set.ofList
        [
            "abstract"
            "and"
            "as"
            "assert"
            "base"
            "begin"
            "class"
            "default"
            "delegate"
            "do"
            "done"
            "downcast"
            "downto"
            "elif"
            "else"
            "end"
            "exception"
            "extern"
            "false"
            "finally"
            "fixed"
            "for"
            "fun"
            "function"
            "global"
            "if"
            "in"
            "inherit"
            "inline"
            "interface"
            "internal"
            "lazy"
            "let"
            "match"
            "member"
            "module"
            "mutable"
            "namespace"
            "new"
            "not"
            "null"
            "of"
            "open"
            "or"
            "override"
            "private"
            "public"
            "rec"
            "return"
            "select"
            "static"
            "struct"
            "then"
            "to"
            "true"
            "try"
            "type"
            "upcast"
            "use"
            "val"
            "void"
            "when"
            "while"
            "with"
            "yield"
            "atomic"
            "break"
            "checked"
            "component"
            "const"
            "constraint"
            "constructor"
            "continue"
            "eager"
            "event"
            "external"
            "functor"
            "include"
            "method"
            "mixin"
            "object"
            "parallel"
            "params"
            "process"
            "protected"
            "pure"
            "sealed"
            "tailcall"
            "trait"
            "virtual"
            // Inherited from OCaml: keywords rather than operators, so they need backticks too.
            "asr"
            "land"
            "lor"
            "lsl"
            "lsr"
            "lxor"
            "mod"
            "sig"
        ]

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
    // FSharp.Core's abbreviation for `System.Numerics.BigInteger`, which Fable compiles to the
    // native JavaScript `BigInt`; no open is needed for it.
    | FsBigInt -> "bigint"
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
        args
        |> List.map (printTypeIn true)
        |> String.concat ", "
        |> sprintf "Action<%s>"
    | FsDelegate(args, ret) ->
        args @ [ ret ]
        |> List.map (printTypeIn true)
        |> String.concat ", "
        |> sprintf "Func<%s>"
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
        text.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

    $"\"{escaped}\""

/// A literal as attribute-argument source text: `CompiledName`/`CompiledValue` payloads.
let printLiteral =
    function
    | LitString text -> stringLit text
    | LitBool true -> "true"
    | LitBool false -> "false"
    | LitNumber value when System.Double.IsInteger value && abs value < 2147483648.0 -> string (int value)
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

                rendered.Append($"<c>{xmlEscape line[index + opening .. closing - 1]}</c>")
                |> ignore

                index <- closing + opening
                prose <- index

    rendered.Append(xmlEscape line[prose..]).ToString()

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
    let escaped (line: string) =
        $"{indent}/// {xmlEscape line}".TrimEnd()

    let prose (line: string) =
        $"{indent}/// {inlineCode line}".TrimEnd()

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
    [
        let summary = docs.Trim()

        if summary <> "" then
            yield $"{indent}/// <summary>"
            yield! docBody indent (splitLines summary)
            yield $"{indent}/// </summary>"

        for tag in tags do
            let text = tag.Text |> ValueOption.defaultValue ""

            match splitLines text with
            | [| single |] ->
                let content =
                    if single = "" then
                        $"@{tag.Name}"
                    else
                        $"@{tag.Name} {single}"

                yield $"{indent}/// <remarks>{inlineCode content}</remarks>"
            | lines ->
                yield $"{indent}/// <remarks>"
                yield $"{indent}/// @{tag.Name}"
                yield! docBody indent lines
                yield $"{indent}/// </remarks>"
    ]

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

/// A parameter inside an abstract member's signature. A rest tail carries `[<ParamArray>]`
/// here too: F# admits parameter attributes in a slot signature, and without it Fable passes
/// the array as one argument - the run gate's `tween(...values)` arrived as `[[1, 2, 3]]`.
let private renderAbstractParam (parameter: FsParam) =
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

let private renderAbstractSignature (parameters: FsParam list) (returns: FsTypeRef) =
    let left =
        match parameters with
        | [] -> "unit"
        | parameters -> parameters |> List.map renderAbstractParam |> String.concat " * "

    $"{left} -> {printType returns}"

/// A declaration's name with its type parameters and their constraints (§4.9), as written at
/// the point of definition: `Box<'T>`, `Node<'T when 'T :> Element>`. A generic *member*
/// writes its own parameters the same way - `abstract read<'K>: ...`, spaced off its colon by
/// `memberColon` below where the head ends in `>>`. F# admits one `when` clause, after the last
/// parameter, with the constraints joined by `and` - a clause between two parameters
/// (`<'A when 'A :> X, 'B>`) is a syntax error.
let private declHead (name: string) (typeParameters: FsTypeParam list) =
    if typeParameters.IsEmpty then
        ident name
    else
        let parameters =
            typeParameters |> List.map (fun p -> $"'{p.Name}") |> String.concat ", "

        let constraints =
            typeParameters
            |> List.choose (fun p ->
                p.Constraint
                |> Option.map (fun bound -> $"'{p.Name} :> {printTypeIn true bound}"))

        match constraints with
        | [] -> $"{ident name}<{parameters}>"
        | constraints ->
            let joined = String.concat " and " constraints
            $"{ident name}<{parameters} when {joined}>"

/// The separator between a member's head and its signature. F# lexes `>>` as a single token, so
/// a head whose last constraint is itself a generic application - `m<'T when 'T :> Obj<Ev>>` -
/// runs into the member's colon as `>>:` and the whole file fails to parse (`FS0010`). One space
/// before the colon is the entire fix. A head ending in a single `>` (`m<'T>:`, `m<'T when 'T :>
/// Ev>:`) lexes correctly, so the space is spent only where it is needed and no existing golden
/// moves.
let private memberColon (head: string) =
    if head.EndsWith ">>" then " :" else ":"

/// The same declaration written at a reference position, where the parameters appear bare:
/// `Box<'T>`. A constraint belongs to the definition only, so it is not repeated here.
let private declRef (name: string) (typeParameters: FsTypeParam list) =
    if typeParameters.IsEmpty then
        ident name
    else
        let parameters =
            typeParameters |> List.map (fun p -> $"'{p.Name}") |> String.concat ", "

        $"{ident name}<{parameters}>"

let private renderMember (m: FsMember) =
    match m with
    | FsProperty p ->
        [
            yield! docLines "    " p.Docs p.Tags
            let mutability = if p.ReadOnly then "" else " with get, set"
            yield $"    abstract {ident p.Name}: {printType p.Type}{mutability}"
        ]
    | FsMethod m ->
        [
            yield! docLines "    " m.Docs m.Tags
            let head = declHead m.Name m.TypeParameters
            yield $"    abstract {head}{memberColon head} {renderAbstractSignature m.Parameters m.Return}"
        ]
    | FsIndexer i ->
        // `[<EmitIndexer>]` is what makes this reach JavaScript as `bag[key]` rather than a
        // method call; the member must be named `Item` for F# indexer syntax to bind to it.
        [
            yield "    [<EmitIndexer>]"
            let mutability = if i.ReadOnly then "" else " with get, set"
            yield $"    abstract Item: {printType i.Key} -> {printType i.Value}{mutability}"
        ]
    | FsConstructor c ->
        // `[<EmitConstructor>]` is `Emit("new $0($1...)")`, and on an abstract member `$0` is
        // the object the member is read off - so `scope.Request.Create(url)` compiles to
        // `new scope.Request(url)` rather than to a call (§4.4).
        [
            yield! docLines "    " c.Docs c.Tags
            yield "    [<EmitConstructor>]"
            let head = declHead "Create" c.TypeParameters
            yield $"    abstract {head}: {renderAbstractSignature c.Parameters c.Return}"
        ]

/// One binding attribute at `indent`, optionally carrying a second attribute inside the same
/// brackets. A global names its own path off `globalThis`; an import names its specifier - the
/// run's runtime package, or an ambient module's own quoted specifier.
let private bindingAttribute (runtimePackage: string) (indent: string) (also: string) (binding: ImportBinding) =
    let package = stringLit runtimePackage

    match binding with
    | ImportDefault -> $"{indent}[<Import({stringLit Naming.defaultImportKey}, {package}){also}>]"
    | ImportNamed name -> $"{indent}[<Import({stringLit name}, {package}){also}>]"
    | ImportFrom(name, specifier) -> $"{indent}[<Import({stringLit name}, {stringLit specifier}){also}>]"
    | GlobalName name -> $"{indent}[<Global({stringLit name}){also}>]"

/// The binding a declaration writes at the type level, where it holds a settable member. Under a
/// type-level attribute Fable compiles `X.y <- v` to `X.y = v`; under a per-member one it compiles
/// the call `X.y(v)`.
let private hoistedBinding (members: FsExportMember list) =
    members |> List.tryPick (fun m -> if m.Settable then Some m.Binding else None)

/// One bound member - an `Exports` member or a class static - as its attribute line and its
/// signature. Both hold an `ImportBinding` and neither has an F# body, so they render the same.
let private renderBound (runtimePackage: string) (m: FsExportMember) =
    [
        yield! docLines "    " m.Docs m.Tags

        let attribute (also: string) =
            bindingAttribute runtimePackage "    " also m.Binding

        match m.Body with
        | ExportFunction(parameters, returns) ->
            yield attribute ""

            yield
                $"    static member {declHead m.Name m.TypeParameters} {renderParamList parameters} : {printType returns} = jsNative"
        | ExportValue reference when m.Settable ->
            // The declaring type carries the attribute, and the member name is the JavaScript key
            // read off whatever that names.
            let reference = printType reference
            yield $"    static member {ident m.Name}"
            yield $"        with get (): {reference} = jsNative"
            yield $"        and set (_: {reference}): unit = jsNative"
        | ExportValue reference ->
            yield attribute ""
            yield $"    static member {ident m.Name}: {printType reference} = jsNative"
        | ExportConstructor(parameters, returns) ->
            yield attribute "; EmitConstructor"

            yield
                $"    static member {declHead m.Name m.TypeParameters} {renderParamList parameters} : {printType returns} = jsNative"
    ]

/// One member of an entrypoint class. A method stays `abstract`, which is the slot a derived
/// class overrides; a property is a concrete binding onto the instance, because the JavaScript
/// constructor is what assigns it and a derived class reads it as it stands.
let private renderClassMember (m: FsMember) =
    match m with
    | FsProperty p ->
        [
            yield! docLines "    " p.Docs p.Tags
            let reference = printType p.Type

            if p.ReadOnly then
                yield $"    member _.{ident p.Name}: {reference} = jsNative"
            else
                yield $"    member _.{ident p.Name}"
                yield $"        with get (): {reference} = jsNative"
                yield $"        and set (_: {reference}): unit = jsNative"
        ]
    | other -> renderMember other

/// A class an ambient module exports for consumers to derive from (§4.4). `[<AbstractClass>]`
/// under the import that binds the JavaScript constructor: Fable compiles a derived class's
/// `inherit` to `extends` and its constructor to `super(...)`.
let private renderEntrypointClass (runtimePackage: string) (decl: FsInterfaceDecl) (entrypoint: FsEntrypoint) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield bindingAttribute runtimePackage "" "; AbstractClass" entrypoint.Binding

        let head =
            $"type {declHead decl.Name decl.TypeParameters} {renderParamList entrypoint.Parameters}"

        match decl.Members, decl.Statics with
        | [], [] -> yield $"{head} = class end"
        | members, statics ->
            yield $"{head} ="

            for m in members do
                yield! renderClassMember m

            for m in statics do
                yield! renderBound runtimePackage m
    ]

let private renderInterface (runtimePackage: string) (decl: FsInterfaceDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags

        // Any static with a body makes F# infer a class; the attribute keeps the type an
        // interface (and needs default-interface-member runtime support to type-check).
        if not (decl.CreateOverloads.IsEmpty && decl.Statics.IsEmpty) then
            yield "[<Interface>]"

        match hoistedBinding decl.Statics with
        | Some binding -> yield bindingAttribute runtimePackage "" "" binding
        | None -> ()

        match decl.Inherits, decl.Members, decl.CreateOverloads, decl.Statics with
        | [], [], [], [] ->
            yield $"type {declHead decl.Name decl.TypeParameters} ="
            yield "    interface end"
        | inherits, members, creates, statics ->
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
                    $"    static member Create {renderParamList overload} : {declRef decl.Name decl.TypeParameters} = jsNative"

            // Class statics (§4.4), last so that the instance surface reads first and a
            // generated Create keeps the place it has held since phase B.
            for m in statics do
                yield! renderBound runtimePackage m
    ]

let private renderStringEnum (decl: FsStringEnumDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
        yield $"type {ident decl.Name} ="

        for case in decl.Cases do
            let attributes =
                [
                    match case.CompiledName with
                    | Some name -> $"CompiledName({stringLit name})"
                    | None -> ()
                    match case.CompiledValue with
                    | Some value -> $"CompiledValue({printLiteral value})"
                    | None -> ()
                ]

            match attributes with
            | [] -> yield $"    | {ident case.Name}"
            | attributes -> yield $"""    | [<{String.concat "; " attributes}>] {ident case.Name}"""
    ]

/// A tagged union (D4, §4.5(2)). `RequireQualifiedAccess` for the same reason the StringEnum
/// emission takes it: one generated module holds every declaration a package has, and bare
/// case names collide across unions - and here with the member interfaces the cases carry.
let private renderTaggedUnion (decl: FsTaggedUnionDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
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
            | None -> yield $"    | {ident case.Name}{carries}"
    ]

let private renderEnum (decl: FsEnumDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield $"type {ident decl.Name} ="

        for name, value in decl.Cases do
            yield $"    | {ident name} = {value}"
    ]

let private renderAbbrev (decl: FsAbbrevDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield $"type {declHead decl.Name decl.TypeParameters} = {printType decl.Target}"
    ]

/// The unit of measure a branding intersection becomes (§4.6, D11). A measure has no body:
/// the name is the whole of it, and what it brands is written at the uses as `string<Name>`.
/// The primitive is recorded in the doc comment because the declaration itself cannot say it.
let private renderMeasure (decl: FsMeasureDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield $"/// <remarks>A brand over <c>{printType decl.Primitive}</c>.</remarks>"
        yield "[<Measure>]"
        yield $"type {ident decl.Name}"
    ]

/// A declaration whose right-hand side is a type-level computation F# has no way to reproduce -
/// a mapped or conditional type, or a template literal over an operand the checker left open
/// (§4.10, §4.11). The name and the arity survive, so uses of it stay distinct from one another
/// and from `obj`; the single private case means a cast is the only way in or out.
let private renderPhantom (decl: FsPhantomDecl) =
    // Named after the type it carries, so the cases of two phantoms never collide in the one
    // module a package generates into.
    let case = ident (decl.Name + "__")

    [
        yield! docLines "" decl.Docs decl.Tags
        yield "[<Erase>]"
        yield $"type {declHead decl.Name decl.TypeParameters} = private {case} of {printType decl.Carrier}"
    ]

let private renderExports (runtimePackage: string) (members: FsExportMember list) =
    [
        yield "/// <summary>The package's value exports, each bound to its import.</summary>"
        yield "[<Erase>]"

        match hoistedBinding members with
        | Some binding -> yield bindingAttribute runtimePackage "" "" binding
        | None -> ()

        yield "type Exports ="

        for m in members do
            yield! renderBound runtimePackage m
    ]

// ---------------------------------------------------------------------------------------------
// Group emission (O7): a run writes one module per shipped group, and a name crossing a module
// boundary is written qualified - the same spelling the `reference` disposition templates.
// ---------------------------------------------------------------------------------------------

/// One module a run writes: a group's declarations under the module name that group templates
/// to (O7).
type GroupModule =
    {
        /// The npm name the group is addressed by under `xantham.json`'s `groups`; the entry
        /// package's own name for the entry group.
        Group: string
        /// The group the run was asked to generate. Its module is written at the output root;
        /// every other shipped group is written under `groups/`.
        IsEntry: bool
        Module: string
        /// The npm package this module's `[<Import(…)>]` attributes name.
        RuntimePackage: string
        Decls: FsDecl list
    }

/// The name a declaration is written under. `Exports` gathers the module's value exports and
/// carries no name of its own.
let declName =
    function
    | FsInterface decl -> Some decl.Name
    | FsStringEnum decl -> Some decl.Name
    | FsTaggedUnion decl -> Some decl.Name
    | FsEnum decl -> Some decl.Name
    | FsAbbrev decl -> Some decl.Name
    | FsMeasure decl -> Some decl.Name
    | FsPhantom decl -> Some decl.Name
    | FsExports _ -> None

let private qualifyName (foreign: Map<string, string>) (name: string) =
    Map.tryFind name foreign |> Option.defaultValue name

/// Rewrites each reference to a name another module of the run declares into that module's
/// qualified spelling. `foreign` holds only names declared elsewhere, so a reference to a
/// binding somebody else wrote (`JS.Promise`, `Browser.Types.Blob`) passes through untouched.
let rec private qualifyRef (foreign: Map<string, string>) =
    function
    | FsOption inner -> FsOption(qualifyRef foreign inner)
    | FsArray element -> FsArray(qualifyRef foreign element)
    | FsTuple components -> FsTuple(components |> List.map (qualifyRef foreign))
    | FsErasedUnion arms -> FsErasedUnion(arms |> List.map (qualifyRef foreign))
    | FsDelegate(parameters, returns) ->
        FsDelegate(parameters |> List.map (qualifyRef foreign), qualifyRef foreign returns)
    | FsApp(name, arguments) -> FsApp(qualifyName foreign name, arguments |> List.map (qualifyRef foreign))
    | FsBranded(primitive, measure) -> FsBranded(qualifyRef foreign primitive, qualifyName foreign measure)
    | FsNamed name -> FsNamed(qualifyName foreign name)
    | primitive -> primitive

let private qualifyParam foreign (parameter: FsParam) =
    { parameter with
        Type = qualifyRef foreign parameter.Type
    }

let private qualifyTypeParams foreign (parameters: FsTypeParam list) =
    parameters
    |> List.map (fun parameter ->
        { parameter with
            Constraint = parameter.Constraint |> Option.map (qualifyRef foreign)
        })

let private qualifyMember foreign =
    function
    | FsProperty m ->
        FsProperty
            { m with
                Type = qualifyRef foreign m.Type
            }
    | FsMethod m ->
        FsMethod
            { m with
                TypeParameters = qualifyTypeParams foreign m.TypeParameters
                Parameters = m.Parameters |> List.map (qualifyParam foreign)
                Return = qualifyRef foreign m.Return
            }
    | FsIndexer m ->
        FsIndexer
            { m with
                Key = qualifyRef foreign m.Key
                Value = qualifyRef foreign m.Value
            }
    | FsConstructor m ->
        FsConstructor
            { m with
                TypeParameters = qualifyTypeParams foreign m.TypeParameters
                Parameters = m.Parameters |> List.map (qualifyParam foreign)
                Return = qualifyRef foreign m.Return
            }

let private qualifyBound foreign (m: FsExportMember) =
    { m with
        TypeParameters = qualifyTypeParams foreign m.TypeParameters
        Body =
            match m.Body with
            | ExportFunction(parameters, returns) ->
                ExportFunction(parameters |> List.map (qualifyParam foreign), qualifyRef foreign returns)
            | ExportValue reference -> ExportValue(qualifyRef foreign reference)
            | ExportConstructor(parameters, returns) ->
                ExportConstructor(parameters |> List.map (qualifyParam foreign), qualifyRef foreign returns)
    }

let private qualifyDecl foreign =
    function
    | FsInterface decl ->
        FsInterface
            { decl with
                TypeParameters = qualifyTypeParams foreign decl.TypeParameters
                Inherits = decl.Inherits |> List.map (qualifyRef foreign)
                Members = decl.Members |> List.map (qualifyMember foreign)
                Entrypoint =
                    decl.Entrypoint
                    |> Option.map (fun entrypoint ->
                        { entrypoint with
                            Parameters = entrypoint.Parameters |> List.map (qualifyParam foreign)
                        })
                CreateOverloads = decl.CreateOverloads |> List.map (List.map (qualifyParam foreign))
                Statics = decl.Statics |> List.map (qualifyBound foreign)
            }
    | FsTaggedUnion decl ->
        FsTaggedUnion
            { decl with
                Cases =
                    decl.Cases
                    |> List.map (fun case ->
                        { case with
                            Fields =
                                case.Fields
                                |> List.map (fun field ->
                                    { field with
                                        Type = qualifyRef foreign field.Type
                                    })
                        })
            }
    | FsAbbrev decl ->
        FsAbbrev
            { decl with
                TypeParameters = qualifyTypeParams foreign decl.TypeParameters
                Target = qualifyRef foreign decl.Target
            }
    | FsMeasure decl ->
        FsMeasure
            { decl with
                Primitive = qualifyRef foreign decl.Primitive
            }
    | FsPhantom decl ->
        FsPhantom
            { decl with
                TypeParameters = qualifyTypeParams foreign decl.TypeParameters
                Carrier = qualifyRef foreign decl.Carrier
            }
    | FsExports members -> FsExports(members |> List.map (qualifyBound foreign))
    // A string enum and an F# enum are closed over literals.
    | cases -> cases

/// One `.fs` file: header, opens, declarations in the order the shape tier fixed. `module rec`
/// so declaration order never fights reference order.
let private renderModule (group: GroupModule) (foreign: Map<string, string>) =
    let decls =
        if Map.isEmpty foreign then
            group.Decls
        else
            group.Decls |> List.map (qualifyDecl foreign)

    let body =
        decls
        |> List.map (function
            | FsInterface decl ->
                match decl.Entrypoint with
                | Some entrypoint -> renderEntrypointClass group.RuntimePackage decl entrypoint
                | None -> renderInterface group.RuntimePackage decl
            | FsStringEnum decl -> renderStringEnum decl
            | FsTaggedUnion decl -> renderTaggedUnion decl
            | FsEnum decl -> renderEnum decl
            | FsAbbrev decl -> renderAbbrev decl
            | FsMeasure decl -> renderMeasure decl
            | FsPhantom decl -> renderPhantom decl
            | FsExports members -> renderExports group.RuntimePackage members)
        |> List.map (String.concat "\n")
        |> String.concat "\n\n"

    String.concat
        "\n"
        [
            "// <auto-generated>"
            $"//   Generated by Xantham.Generator from {group.Group}."
            "//   Do not edit by hand - regenerate instead."
            "// </auto-generated>"
            $"module rec {group.Module}"
            ""
            "open System"
            "open Fable.Core"
            "open Fable.Core.JsInterop"
            "open Xantham.Fable.Core"
            ""
            body
            ""
        ]

/// The run's source files, one per shipped group (O7), and the record of what each group's
/// emission came to. The entry package's module is written at the output root and every other
/// shipped group under `groups/`, so a consumer compiling the whole output compiles `groups/`
/// first: a module is written before the one naming its types.
///
/// An empty plan writes the entry package alone, from every declaration the model carries.
let renderSources (modules: GroupModule list) : Pass<RenderModel> =
    {
        Name = "render-source"
        Run =
            fun ctx model ->
                async {
                    let planned =
                        match modules with
                        | [] ->
                            [
                                {
                                    Group = model.PackageName
                                    IsEntry = true
                                    Module = model.ModuleName
                                    RuntimePackage = model.RuntimePackage
                                    Decls = model.Decls
                                }
                            ]
                        | modules -> modules

                    // The entry package claims its module name first and the rest claim in
                    // group order, so which group loses a collision is fixed by configuration.
                    let ordered = planned |> List.sortBy (fun group -> not group.IsEntry, group.Group)

                    let _, kept, collided =
                        ordered
                        |> List.fold
                            (fun (claimed, kept, collided) group ->
                                if Set.contains group.Module claimed then
                                    claimed, kept, collided @ [ group ]
                                else
                                    Set.add group.Module claimed, kept @ [ group ], collided)
                            (Set.empty, [], [])

                    // A group that lost the name keeps its declarations in the entry module,
                    // where a run writing one module puts them.
                    let written =
                        kept
                        |> List.map (fun group ->
                            if group.IsEntry then
                                { group with
                                    Decls = group.Decls @ (collided |> List.collect _.Decls)
                                }
                            else
                                group)
                        |> List.filter (fun group -> group.IsEntry || not group.Decls.IsEmpty)

                    let owners =
                        written
                        |> List.collect (fun group ->
                            group.Decls |> List.choose declName |> List.map (fun name -> name, group.Module))
                        |> Map.ofList

                    let foreignTo (group: GroupModule) =
                        owners
                        |> Map.filter (fun _ owner -> owner <> group.Module)
                        |> Map.map (fun name owner -> $"{owner}.{name}")

                    let files =
                        written
                        |> List.sortBy (fun group -> not group.IsEntry, group.Module)
                        |> List.map (fun group ->
                            let file =
                                if group.IsEntry then
                                    $"{group.Module}.fs"
                                else
                                    $"groups/{group.Module}.fs"

                            file, renderModule group (foreignTo group))

                    let reached = written @ collided |> List.map _.Group |> Set.ofList

                    let findings =
                        [
                            for group in written do
                                if not group.IsEntry then
                                    Finding.make group.Group (EmitGroups.GroupShipped(group.Group, group.Decls.Length))

                            for group in collided do
                                Finding.make group.Group (EmitGroups.GroupModuleCollision(group.Group, group.Module))

                            for key, disposition in Map.toList ctx.Config.Groups do
                                if disposition = Ship && not (Set.contains key reached) then
                                    Finding.make key (EmitGroups.ShippedGroupWithoutDeclarations key)
                        ]

                    let model =
                        { model with
                            Files = model.Files @ files
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

/// The top-level symbol a finding belongs to: the qualified name cut at the first member or
/// parameter qualifier.
let private ownerOf (findingSymbol: string) =
    match findingSymbol.IndexOfAny [| '.'; '(' |] with
    | -1 -> findingSymbol
    | cut -> findingSymbol.Substring(0, cut)

/// Per-symbol fidelity: every generated declaration in output order, then any finding subjects
/// that produced no declaration (drops, table-level findings), each with its worst tier.
let symbolTiers (model: RenderModel) : (string * Tier * Finding list) list =
    let grouped =
        model.Findings
        |> List.groupBy (fun finding -> ownerOf finding.Symbol)
        |> Map.ofList

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

    {
        Exact = count Exact
        Ergonomic = count Ergonomic
        Widened = count Widened
        Escape = count Escape
    }

let private tierLabel =
    function
    | Exact -> "exact"
    | Ergonomic -> "ergonomic"
    | Widened -> "widened"
    | Escape -> "escape"

// The manifest's shape, spelled as records so property order is fixed by declaration. A pass
// is labelled with the prefix of the union it owns (`SI - shape-interfaces`). `file`
// is null where a symbol has no declaration to point at (drops, table-level findings) and is
// then omitted from the JSON.
//
// A run writes the report as two files. `manifest.json` holds the aggregate - the package, the
// tier counts and the per-pass tallies - and stays a page long for any package, so a reader
// takes the whole of it. `symbols.jsonl` holds the per-symbol detail, one symbol per line, and
// runs to thousands of lines for a package the size of `@cloudflare/workers-types`: a reader
// greps it or takes the lines it wants.
type ManifestFinding =
    {
        /// The finding's stable name, `TR.NullableHoistedToOption`: what a consumer dispatches
        /// on, since it is fixed by what the case is called.
        name: string
        /// The same finding's numeric code, `TR032`, as prose and `--key` filters cite it.
        key: string
        pass: string
        tier: string
        symbol: string
        /// The case's payload, field by field, for a consumer that dispatches on the detail
        /// rather than reading `message`. Null, and so omitted, for a case without a payload.
        fields: JsonObject
        message: string
    }

type ManifestCounts =
    {
        exact: int
        ergonomic: int
        widened: int
        escape: int
    }

/// A pass's tallies: only the non-zero ones are written, so a pass that raised nothing at any
/// tier is just its label.
type ManifestPass =
    {
        pass: string
        total: Nullable<int>
        exact: Nullable<int>
        ergonomic: Nullable<int>
        widened: Nullable<int>
        escape: Nullable<int>
    }

type ManifestSymbol =
    {
        name: string
        file: string
        tier: string
        findings: ManifestFinding list
    }

type Manifest =
    {
        /// The shape of the pair of files, bumped when a consumer would have to read them
        /// differently.
        schemaVersion: int
        package: string
        ``module``: string
        counts: ManifestCounts
        passes: ManifestPass list
    }

let private manifestOptions =
    let options = JsonSerializerOptions(WriteIndented = true)
    options.NewLine <- "\n" // byte-identical output whatever the OS
    options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    options

/// `symbols.jsonl` is one symbol per line, so a reader takes the symbols it wants and a grep
/// answers with the line it found rather than a position in a file it has to reconstruct.
let private symbolOptions =
    let options = JsonSerializerOptions(WriteIndented = false)
    options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    options

/// A payload field's value as JSON: the number, string or boolean it is, and its `ToString` for
/// any other type a case might later carry.
let private fieldValue (value: obj) : JsonNode =
    match value with
    | null -> null
    | :? string as text -> JsonValue.Create text
    | :? int as number -> JsonValue.Create number
    | :? bool as flag -> JsonValue.Create flag
    | :? float as number -> JsonValue.Create number
    | other -> JsonValue.Create(string other)

/// A finding's payload as a JSON object, field name to value, in declaration order. Null for a
/// case without a payload, so the property is omitted.
let private payloadFields (finding: Finding) : JsonObject =
    match finding.Payload with
    | [||] -> null
    | payload ->
        let fields = JsonObject()

        for name, value in payload do
            fields[name] <- fieldValue value

        fields

/// The declaration file a symbol came from, as the manifest reports it: relative to the package
/// for the package's own files, from `node_modules/` down for anything installed, and the bare
/// pseudo-path for the compiler's bundled libs. Never an absolute path, so the goldens hold.
let private sourceFile (packageDir: string) (order: DeclOrder option) : string =
    match order with
    | None -> null
    | Some order ->
        let path = order.File.Replace('\\', '/')
        let root = packageDir.Replace('\\', '/').TrimEnd '/' + "/"

        if path.StartsWith(root, StringComparison.OrdinalIgnoreCase) then
            path.Substring root.Length
        else
            match path.LastIndexOf "/node_modules/" with
            | -1 -> path.Substring(path.LastIndexOf '/' + 1)
            | at -> path.Substring(at + 1)

/// Declaration name -> the file it was declared in, for every declaration that carries an order.
let private declFiles (model: RenderModel) : Map<string, string> =
    model.Decls
    |> List.choose (function
        | FsInterface decl -> Some(decl.Name, decl.Order)
        | FsStringEnum decl -> Some(decl.Name, decl.Order)
        | FsTaggedUnion decl -> Some(decl.Name, decl.Order)
        | FsEnum decl -> Some(decl.Name, decl.Order)
        | FsAbbrev decl -> Some(decl.Name, decl.Order)
        | FsMeasure decl -> Some(decl.Name, decl.Order)
        | FsPhantom decl -> Some(decl.Name, decl.Order)
        | FsExports _ -> None)
    |> List.choose (fun (name, order) ->
        match sourceFile model.PackageDir order with
        | null -> None
        | file -> Some(name, file))
    |> Map.ofList

/// Per-pass tallies of the findings each pass raised, in execution order.
let private passTallies (findings: Finding list) : ManifestPass list =
    let byPass = findings |> List.groupBy _.Pass |> Map.ofList

    [
        for pass in findings |> List.map _.Pass |> List.distinct ->
            let raised = byPass[pass]

            // Zero is absence: the field is omitted rather than written as 0.
            let nonZero count =
                if count = 0 then Nullable() else Nullable count

            let count tier =
                raised |> List.filter (fun f -> f.Tier = tier) |> List.length |> nonZero

            {
                pass = FindingCatalogue.passLabel pass
                total = nonZero raised.Length
                exact = count Exact
                ergonomic = count Ergonomic
                widened = count Widened
                escape = count Escape
            }
    ]

/// The fidelity report: which pass widened what, and why, per exported symbol.
let renderManifest: Pass<RenderModel> =
    Pass.pure' "render-manifest" (fun _ model ->
        let rows = symbolTiers model
        let tallies = counts rows
        let files = declFiles model

        let manifest =
            {
                schemaVersion = 1
                package = model.PackageName
                ``module`` = model.ModuleName
                counts =
                    {
                        exact = tallies.Exact
                        ergonomic = tallies.Ergonomic
                        widened = tallies.Widened
                        escape = tallies.Escape
                    }
                passes = passTallies model.Findings
            }

        let symbols =
            [
                for name, tier, findings in rows ->
                    {
                        name = name
                        file = files |> Map.tryFind name |> Option.toObj
                        tier = tierLabel tier
                        findings =
                            [
                                for finding in findings |> List.sortBy (fun f -> f.Pass, f.Symbol, f.Key, f.Message) ->
                                    {
                                        name = finding.Name
                                        key = finding.Key
                                        pass = finding.Pass
                                        tier = tierLabel finding.Tier
                                        symbol = finding.Symbol
                                        fields = payloadFields finding
                                        message = finding.Message
                                    }
                            ]
                    }
            ]

        let json = JsonSerializer.Serialize(manifest, manifestOptions) + "\n"

        let lines =
            symbols
            |> List.map (fun symbol -> JsonSerializer.Serialize(symbol, symbolOptions) + "\n")
            |> String.concat ""

        { model with
            Files = model.Files @ [ "manifest.json", json; "symbols.jsonl", lines ]
        })

/// The tier's pass list, in execution order, for a run that writes the entry package alone.
///
/// The pipeline runs the two halves separately, so the manifest reports what group emission
/// found; a caller with no group plan gets the whole tier in one list.
let passes: Pass<RenderModel> list = [ renderSources []; renderManifest ]
