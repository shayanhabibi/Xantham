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
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// F# keywords and reserved words that force backticks when a JavaScript name collides.
let private keywords =
    Set.ofList
        [
            "_"
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
    // Fable's erased unions (D4): the arity names the type. `U2`-`U9` are `Fable.Core`'s own;
    // wider arities resolve against a `U<n>` this file's own footer declares.
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
    // A callback whose arity survives the boundary as an F# function type (D5a). Always
    // parenthesised: `abstract handler: (float -> string) with get, set` is a property, where the
    // bare spelling is a method, and `abstract make: seed: float -> (float -> string)` is a
    // one-parameter method returning a callback, where the bare spelling takes two parameters.
    | FsFunc(argument, returns) -> $"({printTypeIn true argument} -> {printTypeIn true returns})"
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
let stringLit (text: string<_>) =
    let text: string = text / uom<_>

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

let private xmlAttributeEscape (text: string) =
    (xmlEscape text).Replace("\"", "&quot;").Replace("'", "&apos;")

let private splitLines (text: string) = text.Replace("\r\n", "\n").Split '\n'

/// A line of doc prose, XML-escaped, with simple markdown inline elements rewritten as XML docs.
/// Code spans are opaque to the other elements; other element contents may nest. Markup does not
/// span lines, and a marker that never closes remains prose rather than producing unbalanced XML.
let rec private inlineCode (line: string) =
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

    let closingAsterisk index =
        let mutable index = index
        let mutable found = -1

        while found < 0 && index < line.Length do
            if
                line[index] = '*'
                && (index = 0 || line[index - 1] <> '*')
                && (index + 1 = line.Length || line[index + 1] <> '*')
            then
                found <- index
            else
                index <- index + 1

        found

    let rendered = System.Text.StringBuilder()
    let mutable index = 0
    let mutable prose = 0

    let appendElement opening contentStart contentEnd closing tag =
        rendered.Append(xmlEscape line[prose .. opening - 1]) |> ignore

        let content = line[contentStart..contentEnd]
        let content = if tag = "c" then xmlEscape content else inlineCode content

        rendered.Append($"<{tag}>{content}</{tag}>") |> ignore

        index <- closing
        prose <- index

    while index < line.Length do
        if line[index] = '`' then
            let opening = ticksAt index

            match closingRun opening (index + opening) with
            | -1 -> index <- index + opening
            | closing -> appendElement index (index + opening) (closing - 1) (closing + opening) "c"
        elif index + 1 < line.Length && line[index] = '*' && line[index + 1] = '*' then
            let closing = line.IndexOf("**", index + 2, System.StringComparison.Ordinal)

            if closing > index + 2 then
                appendElement index (index + 2) (closing - 1) (closing + 2) "b"
            else
                index <- index + 2
        elif line[index] = '*' then
            let closing = closingAsterisk (index + 1)

            if closing > index + 1 then
                appendElement index (index + 1) (closing - 1) (closing + 1) "i"
            else
                index <- index + 1
        elif line[index] = '[' then
            let separator = line.IndexOf("](", index + 1, System.StringComparison.Ordinal)

            let closing =
                if separator < 0 then
                    -1
                else
                    line.IndexOf(')', separator + 2)

            if separator > index + 1 && closing > separator + 2 then
                rendered.Append(xmlEscape line[prose .. index - 1]) |> ignore

                rendered.Append($"<a href=\"{xmlAttributeEscape line[separator + 2 .. closing - 1]}\">")
                |> ignore

                rendered.Append(inlineCode line[index + 1 .. separator - 1]).Append("</a>")
                |> ignore

                index <- closing + 1
                prose <- index
            else
                index <- index + 1
        else
            index <- index + 1

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

    let br = $"{indent}/// <br /><br />"

    let opener (info: string) =
        match info.Split([| ' '; '\t' |]) |> Array.head with
        | "" -> "<code>"
        | language -> $"""<code lang="{xmlEscape (language.Replace("\"", ""))}">"""

    let rec walk followsEmptyLine fence lines =
        match lines with
        | [] -> if fence > 0 then [ $"{indent}/// </code>" ] else []
        | line :: rest ->
            match line, fence with
            | CodeFence(ticks, info), 0 -> $"{indent}/// {opener info}" :: walk false ticks rest
            // Markdown closes a block on a bare fence at least as long as the one that opened
            // it; anything else inside the block is code, backticks and all.
            | CodeFence(ticks, ""), _ when ticks >= fence -> $"{indent}/// </code>" :: walk false 0 rest
            | line, 0 when String.IsNullOrWhiteSpace line -> walk true 0 rest
            | line, 0 when followsEmptyLine -> br :: prose line :: walk false 0 rest
            | line, 0 -> prose line :: walk false 0 rest
            | line, _ -> escaped line :: walk false fence rest

    walk false 0 (List.ofSeq lines)

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
            match tag.Name with
            | "param" when tag.Text.IsSome ->
                match
                    tag.Text.Value
                        .Trim()
                        .Split(' ', 2, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                with
                | [| name; text |] ->
                    match splitLines text with
                    | [| single |] -> yield $"{indent}/// <param name=\"{name}\">{inlineCode single}</param>"
                    | lines ->
                        yield $"{indent}/// <param name=\"{name}\">"
                        yield! docBody indent lines
                        yield $"{indent}/// </param>"
                | _ -> ()
            | "returns"
            | "return" when tag.Text.IsSome ->
                match splitLines tag.Text.Value with
                | [||] as arr
                | arr when arr |> Array.forall String.IsNullOrEmpty -> ()
                | [| single |] -> yield $"{indent}/// <returns>{inlineCode single}</returns>"
                | lines ->
                    yield $"{indent}/// <returns>"
                    yield! docBody indent lines
                    yield $"{indent}/// </returns>"
            | "example" when tag.Text.IsSome ->
                match splitLines tag.Text.Value with
                | [||] as arr
                | arr when arr |> Array.forall String.IsNullOrEmpty -> ()
                | [| single |] -> yield $"{indent}/// <example><c>{xmlEscape single}</c></example>"
                | lines ->
                    yield $"{indent}/// <example>"
                    yield! docBody indent [| yield "```"; yield! lines; yield "```" |]
                    yield $"{indent}/// </example>"
            | "default"
            | "defaultValue" when tag.Text.IsSome ->
                match splitLines tag.Text.Value with
                | [||] as arr
                | arr when arr |> Array.forall String.IsNullOrEmpty -> ()
                | [| single |] -> yield $"{indent}/// <defaultValue>{inlineCode single}</defaultValue>"
                | lines ->
                    yield $"{indent}/// <defaultValue>"
                    yield! docBody indent lines
                    yield $"{indent}/// </defaultValue>"
            | "default"
            | "defaultValue"
            | "example"
            | "param"
            | "returns"
            | "return" -> ()
            | _ ->
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

let private patternCases =
    Set.ofList
        [
            "None"
            "Some"
            "ValueNone"
            "ValueSome"
            "Ok"
            "Error"
            "Failure"
            "MatchFailureException"
            for arity in 2..7 do
                for arm in 1..arity do
                    $"Choice{arm}Of{arity}"
        ]

let private bindParameters (parameters: FsParam list) =
    let taken = parameters |> List.map _.Name |> Set.ofList

    parameters
    |> List.mapFold
        (fun taken parameter ->
            if Set.contains parameter.Name patternCases then
                let rec available candidate =
                    if Set.contains candidate taken then
                        available ("_" + candidate)
                    else
                        candidate

                let name = available ("_" + parameter.Name)
                { parameter with Name = name }, Set.add name taken
            else
                parameter, taken)
        taken
    |> fst

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
    | parameters ->
        parameters
        |> bindParameters
        |> List.map renderParam
        |> String.concat ", "
        |> sprintf "(%s)"

let private createAttribute (parameters: FsParam list) =
    if bindParameters parameters = parameters then
        "[<ParamObject; Emit(\"$0\")>]"
    else
        let fields =
            parameters
            |> List.mapi (fun index parameter ->
                let field = $"{stringLit parameter.Name}: ${index}"

                if parameter.Optional then
                    $"...(${index} === undefined ? {{}} : {{{field}}})"
                else
                    field)
            |> String.concat ", "

        let expression = "({" + fields + "})"
        $"[<Emit({stringLit expression})>]"

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
            yield $"    abstract {head}{memberColon head} {renderAbstractSignature c.Parameters c.Return}"
        ]
    | FsInvoke c ->
        // `Emit("$0($1...)")` applies the receiver to the arguments, so `x.Invoke(a)` compiles to
        // the call `x(a)` rather than to `x.Invoke(a)` (§4.4's counterpart for the call side).
        [
            yield! docLines "    " c.Docs c.Tags
            yield "    [<Emit(\"$0($1...)\")>]"
            let head = declHead "Invoke" c.TypeParameters
            yield $"    abstract {head}{memberColon head} {renderAbstractSignature c.Parameters c.Return}"
        ]

/// One binding attribute at `indent`, optionally carrying a second attribute inside the same
/// brackets. A global names its own path off `globalThis`; an import names its specifier - the
/// run's runtime package, or an ambient module's own quoted specifier.
let private bindingAttribute
    (runtimePackage: string<importSpecifier>)
    (indent: string)
    (also: string)
    (binding: ImportBinding)
    =
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
let private renderBound (runtimePackage: string<importSpecifier>) (publicName: string option) (m: FsExportMember) =
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

            match publicName with
            | Some name when name <> m.Name -> yield $"    [<CompiledName({stringLit name})>]"
            | _ -> ()

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
///
/// The declaration is erased at its import, so an `inherit exn()` line carries the is-a relation
/// to F# and nothing to JavaScript: the imported constructor is what runs.
let private renderEntrypointClass
    (runtimePackage: string<importSpecifier>)
    (decl: FsInterfaceDecl)
    (entrypoint: FsEntrypoint)
    =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield bindingAttribute runtimePackage "" "; AbstractClass" entrypoint.Binding

        let head =
            $"type {declHead decl.Name decl.TypeParameters} {renderParamList entrypoint.Parameters}"

        let inherits =
            match entrypoint.Inherits with
            | Some baseRef -> [ $"    inherit {printType baseRef}()" ]
            | None -> []

        match inherits, decl.Members, decl.Statics with
        | [], [], [] -> yield $"{head} = class end"
        | inherits, members, statics ->
            yield $"{head} ="
            yield! inherits

            for m in members do
                yield! renderClassMember m

            for m in statics do
                yield! renderBound runtimePackage None m
    ]

let private renderInterface (runtimePackage: string<importSpecifier>) (decl: FsInterfaceDecl) =
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
                yield "    " + createAttribute overload

                yield
                    $"    static member Create {renderParamList overload} : {declRef decl.Name decl.TypeParameters} = jsNative"

            // Class statics (§4.4), last so that the instance surface reads first and a
            // generated Create keeps the place it has held since phase B.
            for m in statics do
                yield! renderBound runtimePackage None m
    ]

/// F# identifiers a bare single-case `[<StringEnum>]` case collides with if written without
/// `RequireQualifiedAccess`: the core library's own single-case union members.
let reservedCaseNames = Xantham.Generator.Shape.Spec.reservedCaseNames

let private renderStringEnum (decl: FsStringEnumDecl) =
    let qualified =
        match decl.Cases with
        | [ case ] -> Set.contains case.Name reservedCaseNames
        | _ -> true

    [
        yield! docLines "" decl.Docs decl.Tags
        yield
            if qualified then
                "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
            else
                "[<StringEnum(CaseRules.None)>]"
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

let private renderAbbrev runtimePackage (decl: FsAbbrevDecl) =
    [
        yield! docLines "" decl.Docs decl.Tags
        yield $"type {declHead decl.Name decl.TypeParameters} = {printType decl.Target}"

        match decl.Value with
        | Some(binding, reference) ->
            yield ""
            yield bindingAttribute runtimePackage "" "" binding
            yield $"let {ident decl.Name}: {printType reference} = jsNative"
        | None -> ()
    ]

/// A callback as a named delegate (D5): `type TickHandler = delegate of x: float * y: float ->
/// string`. It guarantees the arity at the boundary exactly as `System.Func` and `System.Action`
/// do, and the parameter names reach the consumer's tooling, where the positional spelling gave
/// only a count. A nullary callback takes `unit`, which is the only argument list F# admits for
/// one.
let private renderDelegate (decl: FsDelegateDecl) =
    let parameters =
        match decl.Parameters with
        | [] -> "unit"
        | parameters ->
            parameters
            |> List.map (fun p -> $"{ident p.Name}: {printTypeIn true p.Type}")
            |> String.concat " * "

    [
        yield! docLines "" decl.Docs decl.Tags
        yield $"type {declHead decl.Name decl.TypeParameters} = delegate of {parameters} -> {printType decl.Return}"
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

let private renderExports (runtimePackage: string<importSpecifier>) (container: FsExportContainer) =
    let members = container.Members |> List.map _.Member

    [
        yield "/// <summary>The package's value exports, each bound to its import.</summary>"
        yield "[<Erase>]"

        match hoistedBinding members with
        | Some binding -> yield bindingAttribute runtimePackage "" "" binding
        | None -> ()

        yield $"type {ident container.Name} ="

        for owned in container.Members do
            yield! renderBound runtimePackage (Some owned.ExportName) owned.Member
    ]

// ---------------------------------------------------------------------------------------------
// Group emission (O7): a run writes one module per shipped group, and a name crossing a module
// boundary is written qualified - the same spelling the `reference` disposition templates.
// ---------------------------------------------------------------------------------------------

/// The compiler library's two declaration families. This is renderer metadata rather than a
/// general module-path tree: only the compiler library is co-located in one source file.
type CompilerLibFamily =
    | Es
    | Dom

/// One module a run writes: a group's declarations under the module name that group templates
/// to (O7).
type GroupModule =
    {
        /// The npm name the group is addressed by under `xantham.json`'s `groups`; the entry
        /// package's own name for the entry group.
        Group: string<npmDependency>
        /// The group the run was asked to generate. Its module is written at the output root;
        /// every other shipped group is written under `groups/`.
        IsEntry: bool
        Module: string
        /// The `namespace rec` the module is nested in. Every group sharing a namespace is
        /// written into one file under `groups/`, as a sibling module, so the modules may
        /// reference each other in both directions.
        Namespace: string option
        /// The npm package this module's `[<Import(…)>]` attributes name.
        RuntimePackage: string<importSpecifier>
        /// Present only for one of the compiler library's two child modules.
        CompilerLib: CompilerLibFamily option
        Decls: FsDecl list
    }

/// The name a declaration is written under. `Exports` gathers the module's value exports and
/// carries no name of its own.
let declName =
    function
    | FsInterface decl -> decl.Name
    | FsStringEnum decl -> decl.Name
    | FsTaggedUnion decl -> decl.Name
    | FsEnum decl -> decl.Name
    | FsAbbrev decl -> decl.Name
    | FsDelegateType decl -> decl.Name
    | FsMeasure decl -> decl.Name
    | FsPhantom decl -> decl.Name
    | FsExports decl -> decl.Name

/// The modules a declaration is written inside, and the name it takes there. A path-derived
/// name is dotted (`Widget.Options`), so the declaration goes in `module Widget` under the leaf.
let private nestingOf (name: string) =
    let segments = name.Split '.'
    List.ofArray segments[.. segments.Length - 2], segments[segments.Length - 1]

/// The declaration under the name it takes inside the modules it nests in.
let private underLeaf (name: string) =
    function
    | FsInterface decl -> FsInterface { decl with Name = name }
    | FsStringEnum decl -> FsStringEnum { decl with Name = name }
    | FsTaggedUnion decl -> FsTaggedUnion { decl with Name = name }
    | FsEnum decl -> FsEnum { decl with Name = name }
    | FsAbbrev decl -> FsAbbrev { decl with Name = name }
    | FsDelegateType decl -> FsDelegateType { decl with Name = name }
    | FsMeasure decl -> FsMeasure { decl with Name = name }
    | FsPhantom decl -> FsPhantom { decl with Name = name }
    | FsExports decl -> FsExports { decl with Name = name }

let private indented (indent: string) (line: string) = if line = "" then "" else indent + line

/// One module level's blocks of source text. A declaration with no modules left to enter is
/// rendered here; a nested module opens at the first declaration that reaches into it and takes
/// every later one. Order within a level is the order `order-declarations` fixed.
let rec private nestedBlocks
    (render: FsDecl -> string list)
    (indent: string)
    (entries: (string list * FsDecl) list)
    : string list list =
    let opensAt =
        entries
        |> List.indexed
        |> List.fold
            (fun opened (i, (path, _)) ->
                match path with
                | head :: _ when not (Map.containsKey head opened) -> Map.add head i opened
                | _ -> opened)
            Map.empty

    entries
    |> List.indexed
    |> List.collect (fun (i, (path, decl)) ->
        match path with
        | [] -> [ render decl |> List.map (indented indent) ]
        | head :: _ when Map.find head opensAt = i ->
            let children =
                entries
                |> List.choose (fun (path, nested) ->
                    match path with
                    | segment :: rest when segment = head -> Some(rest, nested)
                    | _ -> None)

            match nestedBlocks render (indent + "    ") children with
            | [] -> []
            | first :: rest -> ($"{indent}module {ident head} =" :: first) :: rest
        | _ -> [])

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
    | FsFunc(argument, returns) -> FsFunc(qualifyRef foreign argument, qualifyRef foreign returns)
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
    | FsInvoke m ->
        FsInvoke
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

let internal qualifyDecl foreign =
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
                            Inherits = entrypoint.Inherits |> Option.map (qualifyRef foreign)
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
                Value =
                    decl.Value
                    |> Option.map (fun (binding, reference) -> binding, qualifyRef foreign reference)
            }
    | FsDelegateType decl ->
        FsDelegateType
            { decl with
                TypeParameters = qualifyTypeParams foreign decl.TypeParameters
                Parameters =
                    decl.Parameters
                    |> List.map (fun p ->
                        { p with
                            Type = qualifyRef foreign p.Type
                        })
                Return = qualifyRef foreign decl.Return
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
    | FsExports container ->
        FsExports
            { container with
                Members =
                    container.Members
                    |> List.map (fun owned ->
                        { owned with
                            Member = qualifyBound foreign owned.Member
                        })
            }
    // A string enum and an F# enum are closed over literals.
    | cases -> cases

/// Arm counts of every `FsErasedUnion` a type reference reaches, recursively.
let rec private erasedArities (reference: FsTypeRef) : int list =
    match reference with
    | FsOption inner -> erasedArities inner
    | FsArray element -> erasedArities element
    | FsTuple components -> components |> List.collect erasedArities
    | FsErasedUnion arms -> arms.Length :: (arms |> List.collect erasedArities)
    | FsDelegate(arguments, returns) -> (arguments |> List.collect erasedArities) @ erasedArities returns
    | FsFunc(argument, returns) -> erasedArities argument @ erasedArities returns
    | FsApp(_, arguments) -> arguments |> List.collect erasedArities
    | FsBranded(primitive, _) -> erasedArities primitive
    | FsBool
    | FsString
    | FsFloat
    | FsBigInt
    | FsUnit
    | FsObj
    | FsTypeVar _
    | FsNamed _ -> []

/// Arm counts of every `FsErasedUnion` a declaration reaches, across every type-reference
/// position `Shape.Arity.mapDeclRefs` also rewrites: members, inherits, statics, exports,
/// constructors and type-parameter constraints.
let private declErasedArities (decl: FsDecl) : int list =
    let ofParam (p: FsParam) = erasedArities p.Type

    let ofTypeParam (p: FsTypeParam) =
        p.Constraint |> Option.map erasedArities |> Option.defaultValue []

    let ofMember =
        function
        | FsProperty p -> erasedArities p.Type
        | FsIndexer i -> erasedArities i.Key @ erasedArities i.Value
        | FsMethod m ->
            (m.TypeParameters |> List.collect ofTypeParam)
            @ (m.Parameters |> List.collect ofParam)
            @ erasedArities m.Return
        | FsConstructor c
        | FsInvoke c ->
            (c.TypeParameters |> List.collect ofTypeParam)
            @ (c.Parameters |> List.collect ofParam)
            @ erasedArities c.Return

    let ofExportMember (m: FsExportMember) =
        (m.TypeParameters |> List.collect ofTypeParam)
        @ (match m.Body with
           | ExportFunction(parameters, returns) -> (parameters |> List.collect ofParam) @ erasedArities returns
           | ExportValue returns -> erasedArities returns
           | ExportConstructor(parameters, returns) -> (parameters |> List.collect ofParam) @ erasedArities returns)

    match decl with
    | FsInterface d ->
        (d.TypeParameters |> List.collect ofTypeParam)
        @ (d.Inherits |> List.collect erasedArities)
        @ (d.Members |> List.collect ofMember)
        @ (d.Entrypoint
           |> Option.map (fun e -> e.Parameters |> List.collect ofParam)
           |> Option.defaultValue [])
        @ (d.CreateOverloads |> List.collect (List.collect ofParam))
        @ (d.Statics |> List.collect ofExportMember)
    | FsAbbrev d ->
        (d.TypeParameters |> List.collect ofTypeParam)
        @ erasedArities d.Target
        @ (d.Value |> Option.map (snd >> erasedArities) |> Option.defaultValue [])
    | FsPhantom d -> (d.TypeParameters |> List.collect ofTypeParam) @ erasedArities d.Carrier
    | FsDelegateType d ->
        (d.TypeParameters |> List.collect ofTypeParam)
        @ (d.Parameters |> List.collect (fun p -> erasedArities p.Type))
        @ erasedArities d.Return
    | FsMeasure d -> erasedArities d.Primitive
    | FsTaggedUnion d ->
        d.Cases
        |> List.collect (fun case -> case.Fields |> List.collect (fun f -> erasedArities f.Type))
    | FsExports container -> container.Members |> List.collect (fun owned -> ofExportMember owned.Member)
    | FsStringEnum _
    | FsEnum _ -> []

/// The arities above `Fable.Core`'s own `U9` a file's declarations need, ascending and
/// deduplicated - the file's footer declares exactly these and nothing wider.
let private footerArities (decls: FsDecl list) : int list =
    decls
    |> List.collect declErasedArities
    |> List.filter (fun arity -> arity > ErasedUnionArity)
    |> List.distinct
    |> List.sort

/// One erased union above `Fable.Core`'s shipped arity, modelled on its own `U2`-`U9`: one case
/// per arm and one `op_ErasedCast` overload per arm, so `!^` widens into it the same way it
/// widens into a shipped `U<n>`.
let private renderErasedUnionArity (arity: int) =
    let typeParameters =
        [ 1..arity ] |> List.map (fun i -> { Name = $"t{i}"; Constraint = None })

    let name = $"U{arity}"

    [
        yield "[<Erase>]"
        yield $"type {declHead name typeParameters} ="

        for i in 1..arity do
            yield $"    | Case{i} of 't{i}"

        yield ""

        for i in 1..arity do
            yield $"    static member op_ErasedCast(x: 't{i}) = Case{i} x"
    ]

let private fileHeader (openDom: bool) (source: string) (declaration: string) =
    [
        "// <auto-generated>"
        $"//   Generated by Xantham.Generator from {source}."
        "//   Do not edit by hand - regenerate instead."
        "// </auto-generated>"
        declaration
        ""
        "open System"
        "open Fable.Core"
        "open Fable.Core.JsInterop"
        "open Fable.Core.JS"
        if openDom then
            "open Fable.Core.TS.Dom"
        ""
    ]

/// A group's declarations, each reference to another module's name qualified, rendered at
/// `indent` in the order the shape tier fixed.
let private renderBody (group: GroupModule) (foreign: Map<string, string>) (indent: string) =
    let decls =
        if Map.isEmpty foreign then
            group.Decls
        else
            group.Decls |> List.map (qualifyDecl foreign)
        |> List.filter (function
            | FsExports container -> not container.Members.IsEmpty
            | _ -> true)

    let names = group.Decls |> List.map declName

    let namesByHead =
        names |> List.groupBy (fun name -> name.Split('.')[0]) |> Map.ofList

    let localBindings =
        names
        |> List.collect (fun name ->
            let segments = name.Split '.' |> Array.toList

            [
                for depth in 1 .. segments.Length - 1 do
                    List.take depth segments, segments[depth]
            ])
        |> List.groupBy fst
        |> List.map (fun (scope, bindings) -> scope, bindings |> List.map snd |> Set.ofList)
        |> Map.ofList

    let scopedReferences =
        names
        |> List.map (nestingOf >> fst)
        |> List.distinct
        |> List.map (fun scope ->
            let shadowed =
                [
                    for depth in 1 .. scope.Length do
                        yield!
                            Map.tryFind (List.take depth scope) localBindings
                            |> Option.defaultValue Set.empty
                ]
                |> Set.ofList

            let references =
                shadowed
                |> Set.toList
                |> List.collect (fun head -> Map.tryFind head namesByHead |> Option.defaultValue [])
                |> List.map (fun name -> name, $"{group.Module}.{name}")
                |> Map.ofList

            scope, references)
        |> Map.ofList

    let render =
        function
        | FsInterface decl ->
            match decl.Entrypoint with
            | Some entrypoint -> renderEntrypointClass group.RuntimePackage decl entrypoint
            | None -> renderInterface group.RuntimePackage decl
        | FsStringEnum decl -> renderStringEnum decl
        | FsTaggedUnion decl -> renderTaggedUnion decl
        | FsEnum decl -> renderEnum decl
        | FsAbbrev decl -> renderAbbrev group.RuntimePackage decl
        | FsDelegateType decl -> renderDelegate decl
        | FsMeasure decl -> renderMeasure decl
        | FsPhantom decl -> renderPhantom decl
        | FsExports container -> renderExports group.RuntimePackage container

    let body =
        decls
        |> List.map (fun decl ->
            match declName decl with
            | ""
            | null
            | "global" -> [], decl
            | name ->
                let modules, leaf = nestingOf name
                let scoped = qualifyDecl (Map.find modules scopedReferences) decl
                modules, underLeaf leaf scoped)
        |> nestedBlocks render indent
        |> List.map (String.concat "\n")
        |> String.concat "\n\n"

    body, decls

// Arities past `Fable.Core`'s own `U9`, declared once at the bottom of the file that needs
// them (D4) rather than in the shared support package - a file needing arity twelve pays for
// a `U12` here, a file needing nothing past nine pays for no footer at all.
let private renderFooter (decls: FsDecl list) =
    match footerArities decls with
    | [] -> ""
    | arities ->
        arities
        |> List.map (renderErasedUnionArity >> String.concat "\n")
        |> String.concat "\n\n"
        |> sprintf "\n%s\n"

/// One `.fs` file: header, opens, declarations in the order the shape tier fixed. `module rec`
/// so declaration order never fights reference order.
let private renderModule (group: GroupModule) (foreign: Map<string, string>) =
    let body, decls = renderBody group foreign ""

    String.concat
        "\n"
        (fileHeader true (group.Group / uom<npmDependency>) $"module rec {group.Module}"
         @ [ body; renderFooter decls ])

let private compilerLibModule (layout: CompilerLibLayout) =
    function
    | Es -> layout.EsQualifiedModule
    | Dom -> layout.DomQualifiedModule

let private compilerLibChild (layout: CompilerLibLayout) =
    function
    | Es -> layout.EsModule, layout.AutoOpenEs
    | Dom -> layout.DomModule, layout.AutoOpenDom

/// The compiler library's two families live under one recursive root module. References still
/// use each child's canonical module name, regardless of whether that child is auto-opened.
let private renderCompilerLib
    (layout: CompilerLibLayout)
    (groups: GroupModule list)
    (foreignTo: GroupModule -> Map<string, string>)
    =
    let rendered =
        groups
        |> List.choose (fun group ->
            group.CompilerLib
            |> Option.map (fun family ->
                let moduleName = compilerLibModule layout family

                let body, decls =
                    renderBody { group with Module = moduleName } (foreignTo group) "    "

                family, body, decls))
        |> List.sortBy (fun (family, _, _) -> family)

    let modules =
        rendered
        |> List.map (fun (family, body, _) ->
            let child, autoOpen = compilerLibChild layout family

            let declaration =
                if autoOpen then
                    [ "[<AutoOpen>]"; $"module {ident child} =" ]
                else
                    [ $"module {ident child} =" ]

            String.concat "\n" (declaration @ [ body ]))
        |> String.concat "\n\n"

    let footer = rendered |> List.collect (fun (_, _, decls) -> decls) |> renderFooter

    let sources =
        groups
        |> List.map (_.Group >> (fun x -> x / uom<npmDependency>))
        |> List.distinct
        |> String.concat ", "
    // The producer must compile without referencing the assembly it generates.
    String.concat "\n" (fileHeader false sources $"module rec {layout.RootModule}" @ [ modules; footer ])

/// One `.fs` file holding every group of a namespace, each as a nested module under
/// `namespace rec`, so the modules reference each other's types in both directions.
let private renderNamespace (ns: string) (groups: GroupModule list) (foreignTo: GroupModule -> Map<string, string>) =
    let rendered =
        groups
        |> List.map (fun group -> group, renderBody group (foreignTo group) "    ")

    let modules =
        rendered
        |> List.map (fun (group, (body, _)) ->
            let leaf = group.Module.Substring(group.Module.LastIndexOf '.' + 1)
            String.concat "\n" [ $"module {ident leaf} ="; body ])
        |> String.concat "\n\n"

    let footer = rendered |> List.collect (snd >> snd) |> renderFooter

    let sources =
        groups
        |> List.map (_.Group >> (fun x -> x / uom<npmDependency>))
        |> List.distinct
        |> String.concat ", "

    String.concat "\n" (fileHeader true sources $"namespace rec {ns}" @ [ modules; footer ])

/// The run's source files, one per shipped group (O7) - one per namespace where groups share
/// one - and the record of what each group's emission came to. The entry package's module is
/// written at the output root and every other shipped group under `groups/`, so a consumer
/// compiling the whole output compiles `groups/` first: a module is written before the one
/// naming its types.
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
                                    Namespace = None
                                    RuntimePackage = model.RuntimePackage
                                    CompilerLib = None
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

                    let compilerLibLayout = CompilerLibLayout.create ctx.Config.CompilerLib

                    let effectiveModule group =
                        group.CompilerLib
                        |> Option.map (compilerLibModule compilerLibLayout)
                        |> Option.defaultValue group.Module

                    let owners =
                        written
                        |> List.collect (fun group ->
                            group.Decls
                            |> List.map declName
                            |> List.map (fun name -> name, effectiveModule group))
                        |> Map.ofList

                    let foreignTo (group: GroupModule) =
                        owners
                        |> Map.filter (fun _ owner -> owner <> group.Module)
                        |> Map.map (fun name owner -> $"{owner}.{name}")

                    let ordered = written |> List.sortBy (fun group -> not group.IsEntry, group.Module)

                    let files =
                        ordered
                        |> List.filter (fun group -> group.CompilerLib.IsNone && group.Namespace.IsNone)
                        |> List.map (fun group ->
                            let file =
                                if group.IsEntry then
                                    $"{group.Module}.fs"
                                else
                                    $"groups/{group.Module}.fs"

                            file,
                            renderModule
                                { group with
                                    Module = effectiveModule group
                                }
                                (foreignTo group))

                    let namespaced =
                        ordered
                        |> List.filter (fun group -> group.CompilerLib.IsNone)
                        |> List.choose (fun group -> group.Namespace |> Option.map (fun ns -> ns, group))
                        |> List.groupBy fst
                        |> List.map (fun (ns, groups) ->
                            $"groups/{ns}.fs", renderNamespace ns (List.map snd groups) foreignTo)

                    let compilerLib = ordered |> List.filter (fun group -> group.CompilerLib.IsSome)

                    let compilerLibFile =
                        match compilerLib with
                        | [] -> []
                        | groups ->
                            [
                                $"groups/{compilerLibLayout.RootModule}.fs",
                                renderCompilerLib compilerLibLayout groups foreignTo
                            ]

                    let files = files @ namespaced @ compilerLibFile

                    let reached = written @ collided |> List.map _.Group |> Set.ofList

                    let findings =
                        [
                            for group in written do
                                for decl in group.Decls do
                                    let bound owner (parameters: FsParam list) =
                                        List.zip parameters (bindParameters parameters)
                                        |> List.choose (fun (original, bound) ->
                                            if original.Name = bound.Name then
                                                None
                                            else
                                                Some(
                                                    Finding.make
                                                        owner
                                                        (EmitGroups.ParameterNameEscaped(original.Name, bound.Name))
                                                ))

                                    let exports owner members =
                                        members
                                        |> List.collect (fun (member_: FsExportMember) ->
                                            match member_.Body with
                                            | ExportFunction(parameters, _)
                                            | ExportConstructor(parameters, _) ->
                                                bound (owner + member_.Name) parameters
                                            | ExportValue _ -> [])

                                    match decl with
                                    | FsInterface interface_ ->
                                        for overload in interface_.CreateOverloads do
                                            yield! bound (interface_.Name + ".Create") overload

                                        for entrypoint in Option.toList interface_.Entrypoint do
                                            yield! bound interface_.Name entrypoint.Parameters

                                        yield! exports (interface_.Name + ".") interface_.Statics
                                    | FsExports container ->
                                        yield! exports (container.Name + ".") (container.Members |> List.map _.Member)
                                    | _ -> ()

                            for group in written do
                                if not group.IsEntry then
                                    Finding.make
                                        (group.Group / uom<npmDependency>)
                                        (EmitGroups.GroupShipped(group.Group / uom<npmDependency>, group.Decls.Length))

                            for group in collided do
                                Finding.make
                                    (group.Group / uom<npmDependency>)
                                    (EmitGroups.GroupModuleCollision(group.Group / uom<npmDependency>, group.Module))

                            for key, disposition in Map.toList ctx.Config.Groups do
                                if disposition = Ship && not (Set.contains key reached) then
                                    Finding.make
                                        (key / uom<npmDependency>)
                                        (EmitGroups.ShippedGroupWithoutDeclarations(key / uom<npmDependency>))
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

/// The declaration a finding is reported under: the longest prefix of the finding's symbol path
/// that this run declares, with the rest of the path reading as a member within it. Where the
/// symbol names no declaration - a drop, a table-level finding - the first segment stands.
let private ownerIn (declared: Set<string>) (findingSymbol: string) =
    let head =
        match findingSymbol.IndexOf '(' with
        | -1 -> findingSymbol
        | cut -> findingSymbol.Substring(0, cut)

    head.Split '.'
    |> Array.scan (fun prefix segment -> if prefix = "" then segment else $"{prefix}.{segment}") ""
    |> Array.filter (fun prefix -> prefix <> "" && Set.contains prefix declared)
    |> Array.tryLast
    |> Option.defaultValue (ownerOf findingSymbol)

/// Per-symbol fidelity: every generated declaration in output order, then any finding subjects
/// that produced no declaration (drops, table-level findings), each with its worst tier.
let symbolTiers (model: RenderModel) : (string * Tier * Finding list) list =
    let declared =
        model.Decls
        |> List.collect (function
            | FsInterface decl -> [ decl.Name ]
            | FsStringEnum decl -> [ decl.Name ]
            | FsTaggedUnion decl -> [ decl.Name ]
            | FsEnum decl -> [ decl.Name ]
            | FsAbbrev decl -> [ decl.Name ]
            | FsDelegateType decl -> [ decl.Name ]
            | FsMeasure decl -> [ decl.Name ]
            | FsPhantom decl -> [ decl.Name ]
            | FsExports container ->
                container.Members
                |> List.map (fun owned -> container.Name + "." + owned.Member.Name))
        |> List.distinct

    let declaredSet = Set.ofList declared

    let grouped =
        model.Findings
        |> List.groupBy (fun finding -> ownerIn declaredSet finding.Symbol)
        |> Map.ofList

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
        let path = (order.File / uom<declFile>).Replace('\\', '/')
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
        | FsDelegateType decl -> Some(decl.Name, decl.Order)
        | FsMeasure decl -> Some(decl.Name, decl.Order)
        | FsPhantom decl -> Some(decl.Name, decl.Order)
        | FsExports _ -> None)
    |> List.choose (fun (name, order) ->
        match sourceFile (model.PackageDir / uom<dirPath>) order with
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
                package = model.PackageName / uom<npmDependency>
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
