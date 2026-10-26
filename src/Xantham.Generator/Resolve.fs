/// Tier 2 - Resolve: the type table. Breadth-first from the harvested exports' types, batched
/// per generation frontier through the mailbox, memoized on `TypeResponse.Id`. The tier's
/// invariant is closure: every type id a `TypeFacts` refers to is in the table or recorded in
/// `NotFollowed` with its reason.
module Xantham.Generator.Resolve

open System.Collections.Concurrent
open Measure

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Namespace ancestry of a source symbol; file/ambient modules and the global augmentation
/// wrapper end the path. Complete symbol keys guard against malformed cyclic parent chains.
let private namespacePathOf (ctx: Context) (symbol: SymbolResponse voption) =
    let rec walk seen symbol =
        async {
            match symbol with
            | ValueSome(current: SymbolResponse) when current.Parent.IsSome && not (Set.contains current.SymbolId seen) ->
                let! parent = ctx.Session.Sessionless.getParentOfSymbol current.Reference

                match parent with
                | ValueSome owner when
                    uint32 (owner.Flags &&& SymbolFlags.Module) <> 0u
                    && Naming.isWritableTypeName owner.Name
                    && owner.Name <> "__global"
                    ->
                    let! outer = walk (Set.add current.SymbolId seen) parent
                    return outer @ [ owner.Name ]
                | _ -> return []
            | _ -> return []
        }

    walk Set.empty symbol

/// The dump path `XANTHAM_FRONTIER_DUMP` names, read once per process. `None` leaves every hook
/// below inert and the default corpus output untouched.
let private frontierDump =
    lazy
        (match System.Environment.GetEnvironmentVariable "XANTHAM_FRONTIER_DUMP" with
         | null
         | "" -> None
         | path -> Some path)

/// Whether `XANTHAM_RESOLVE_COUNTERS` is set, read once per process. Gates the type-table
/// walk's cost counters below so a normal run pays nothing for them.
let private resolveCountersEnabled =
    lazy
        (match System.Environment.GetEnvironmentVariable "XANTHAM_RESOLVE_COUNTERS" with
         | null
         | "" -> false
         | _ -> true)

/// One walk's record of where the types it reached came from: how often each derivation channel
/// discovered a type id, and the generation each id reached the frontier at. A checker id is
/// assigned in the order answers arrive, so an id means nothing outside the walk that saw it and
/// every walk carries its own trace, under a tag that separates its files from a concurrent
/// walk's.
type private Trace =
    {
        Channels: ConcurrentDictionary<int<typeId>, ConcurrentDictionary<string, int>>
        Generations: ConcurrentDictionary<int<typeId>, int>
        Tag: string
    }

module private Trace =
    /// A trace for one walk, or `None` where the dump path is unset.
    let start () =
        frontierDump.Value
        |> Option.map (fun _ ->
            {
                Channels = ConcurrentDictionary()
                Generations = ConcurrentDictionary()
                Tag = System.Guid.NewGuid().ToString("N").Substring(0, 8)
            })

/// Tags the types one derivation channel discovered and hands them back unchanged, so a
/// discovery site is wrapped rather than restructured.
let private channel (trace: Trace option) (name: string) (types: TypeResponse list) =
    match trace with
    | None -> ()
    | Some trace ->
        for ty in types do
            let counts = trace.Channels.GetOrAdd(ty.TypeId, (fun _ -> ConcurrentDictionary()))
            counts.AddOrUpdate(name, 1, (fun _ count -> count + 1)) |> ignore

    types

/// Generations the breadth-first walk follows before recording the rest as deliberately not
/// followed. The frontier doubles as the cycle boundary - ids already derived are never
/// re-entered - so this bounds runaway utility-type expansion, not recursion.
///
/// This value carries nine generations of headroom. The corpus's deepest rendered type
/// reference sits at generation 11, in `@cloudflare/workers-types`; a rendered reference past
/// the cutoff widens to `obj` and reports `TR002`.
[<Literal>]
let private FollowDepth = 20

/// Distinct types one generation of the frontier may hold before the whole generation is
/// recorded as not followed rather than derived - not a per-type admission cutoff, since a
/// checker id is assigned in the order answers arrived and a prefix chosen by id would differ
/// run to run. A safety valve: the widest generation seen live is 10,218 (`lib.dom` shipped
/// whole, generation 1, `XANTHAM_RESOLVE_COUNTERS` measured), and this value carries three
/// times that as headroom.
[<Literal>]
let private FollowWidth = 32768

/// Nested wrapping applications whose members an oversized frontier still follows below a
/// source declaration. A wrapping application is a generic interface, class or alias
/// instantiation with an argument other than a type parameter, a primitive, a literal or a
/// union of those. An application past this depth is kept, and its members are subject to the
/// width cutoff.
[<Literal>]
let private ContractDepth = 1

/// Primitive, literal and top/bottom types: a type with one of these flags has no members.
let private leafTypeFlags =
    TypeFlags.Any
    ||| TypeFlags.Unknown
    ||| TypeFlags.String
    ||| TypeFlags.StringLiteral
    ||| TypeFlags.Number
    ||| TypeFlags.NumberLiteral
    ||| TypeFlags.Boolean
    ||| TypeFlags.BooleanLiteral
    ||| TypeFlags.BigInt
    ||| TypeFlags.BigIntLiteral
    ||| TypeFlags.NonPrimitive
    ||| TypeFlags.ESSymbol
    ||| TypeFlags.UniqueESSymbol
    ||| TypeFlags.Void
    ||| TypeFlags.Undefined
    ||| TypeFlags.Null
    ||| TypeFlags.Never

/// Type ids written during a frontier generation and visible to readers from the next
/// generation on: `Contains` answers from the state as of the last `Commit`, whatever the
/// generation has written since.
type private Register() =
    let committed = System.Collections.Generic.HashSet<int<typeId>>()
    let staged = ConcurrentDictionary<int<typeId>, bool>()
    let mutable stagedReads = 0

    member _.Contains id =
        if
            resolveCountersEnabled.Value
            && staged.ContainsKey id
            && not (committed.Contains id)
        then
            System.Threading.Interlocked.Increment &stagedReads |> ignore

        committed.Contains id

    member _.Add id = staged.TryAdd(id, true) |> ignore

    /// Removes the id from the committed state. Call only between generations.
    member _.Remove(id: int<typeId>) =
        committed.Remove id |> ignore
        staged.TryRemove id |> ignore

    /// Publishes the staged writes and returns how many reads since the last commit missed an id
    /// staged at the time. Call only between generations.
    member _.Commit() =
        for id in staged.Keys do
            committed.Add id |> ignore

        staged.Clear()
        let reads = stagedReads
        stagedReads <- 0
        reads

/// One walk's registers, filled as derivation meets the ids and read by the next generation.
type private Registers =
    {
        /// Type parameters a signature declared. A signature's parameter is cloned afresh every
        /// time the checker instantiates the signature, so an instantiation written over one is
        /// never the same type twice.
        SignatureParameters: Register
        /// Types the shape tier reads for their structure: the seeds, and every operand of a
        /// union or an intersection.
        Structural: Register
        /// Instantiations derived as identity alone. One a later generation reads as an
        /// operand is re-derived in full.
        IdentityOnly: Register
    }

module private Registers =
    let start () =
        {
            SignatureParameters = Register()
            Structural = Register()
            IdentityOnly = Register()
        }

    let structural (registers: Registers) (types: TypeResponse list) =
        for ty in types do
            registers.Structural.Add ty.TypeId

        types

    /// Publishes every register's staged writes; returns the reads that missed a staged id.
    let commit (registers: Registers) =
        registers.SignatureParameters.Commit()
        + registers.Structural.Commit()
        + registers.IdentityOnly.Commit()

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The server's complaint without the Go stack it arrives with: the first line, minus the
/// `panic: ` its recovery handler prefixes. The one refusal seen live is restated in fixed
/// words rather than quoted: Go's encoder says "cannot marshal" on one run and "unable to
/// marshal" on the next for the same value, and a finding is golden text.
let private complaint (message: string) =
    let line = message.Split('\n').[0].Trim()
    let line = if line.StartsWith "panic: " then line.Substring 7 else line
    let marker = "unsupported value: "

    match line.IndexOf marker with
    | -1 -> line
    | at -> $"its answer holds a value JSON cannot carry: {line.Substring(at + marker.Length)}"

/// Runs one request, turning the server's refusal into a reason rather than a raised
/// exception. Only `TsGoError` is caught: that is the server declining to answer *this*
/// request while the channel lives on. The one case seen live is a result it cannot encode -
/// a number literal type whose value is `1e999` is `+Inf` to Go's JSON encoder, and the
/// refusal is the whole answer (`type-fest`'s `PositiveInfinity`). Anything else still
/// raises: a dead channel has nothing trustworthy left to say.
let private attempt (work: Async<'T>) : Async<Result<'T, string>> =
    async {
        try
            let! value = work
            return Ok value
        with TsGoError(method, message) ->
            return Error $"the compiler could not answer {method} ({complaint message})"
    }

/// Writes what the depth cutoff discarded, under the walk's own tag: `<dump>.<tag>.jsonl` holds
/// the frontier's `TypeResponse` values, one per line. `<dump>.<tag>.prov` holds one `ID` line
/// per type id the walk reached - its generation, whether the cutoff stranded it, and the
/// channels that discovered it - and one `TARGET` line per distinct `Target` on the frontier,
/// naming the declaration the group instantiates. Naming a target costs two round trips, so the
/// whole function runs only where a dump path is named.
let private dumpFrontier (ctx: Context) (trace: Trace option) (frontier: TypeResponse list) =
    async {
        match frontierDump.Value, trace with
        | None, _
        | _, None -> ()
        | Some path, Some trace ->
            let stuck = frontier |> List.map _.TypeId |> Set.ofList

            let idLines =
                [
                    for entry in trace.Generations do
                        let channels =
                            match trace.Channels.TryGetValue entry.Key with
                            | true, counts ->
                                counts
                                |> Seq.map (fun kv -> $"{kv.Key}={kv.Value}")
                                |> Seq.sort
                                |> String.concat ";"
                            | _ -> "seed"

                        let onFrontier = if Set.contains entry.Key stuck then 1 else 0
                        $"ID\t{entry.Key}\t{entry.Value}\t{onFrontier}\t{channels}"
                ]

            let byTarget =
                frontier
                |> List.choose (fun ty ->
                    match ty.Target with
                    | ValueSome target -> Some(target, ty)
                    | ValueNone -> None)
                |> List.groupBy fst
                |> List.sortByDescending (snd >> List.length)

            let! targetLines =
                byTarget
                |> List.map (fun (target, entries) ->
                    async {
                        let representative = (entries |> List.head |> snd).Id

                        // The id on `Target` is a field of a response rather than a handle the
                        // compiler registered, so it is re-requested through the reference that
                        // carries it before anything else names it.
                        let! named =
                            attempt (
                                async {
                                    let! resolved = ctx.Session.getTargetOfType representative
                                    let! symbol = ctx.Session.getSymbolOfType resolved.Id
                                    return resolved, symbol
                                }
                            )

                        let description =
                            match named with
                            | Error reason -> $"\t\t{reason}"
                            | Ok(resolved, symbol) ->
                                let name =
                                    symbol |> ValueOption.map _.Name |> ValueOption.defaultValue "<anonymous>"

                                let declaration =
                                    symbol
                                    |> ValueOption.bind _.Declarations
                                    |> ValueOption.bind (Array.tryHead >> ValueOption.ofOption)
                                    |> ValueOption.defaultValue "<none>"

                                $"{resolved.Id}\t{name}\t{declaration}"

                        return $"TARGET\t{target}\t{List.length entries}\t{description}"
                    })
                |> Async.Sequential

            System.IO.File.WriteAllLines(
                $"{path}.{trace.Tag}.jsonl",
                frontier
                |> List.map (fun ty -> System.Text.Encoding.UTF8.GetString(ProtoJson.serialize ty))
            )

            System.IO.File.WriteAllLines($"{path}.{trace.Tag}.prov", idLines @ List.ofArray targetLines)
    }

/// The type ids each export resolves to: the declared type for type-like symbols, the value
/// type for value-like ones, both for symbols that are both (a class). The responses land in
/// the table shallow; `resolveTypeTable` derives them.
let resolveExportTypes: Pass<ResolveModel> =
    {
        Name = "resolve-export-types"
        Run =
            fun ctx model ->
                async {
                    let! resolved =
                        model.Harvest.Exports
                        |> List.map (fun export ->
                            async {
                                let ask relevant (request: SymbolReference -> Async<TypeResponse>) =
                                    if relevant then
                                        async {
                                            let! ty = attempt (request export.Symbol.Reference)
                                            return Some ty
                                        }
                                    else
                                        async.Return None

                                let! declared =
                                    ask
                                        (hasAny SymbolFlags.Type export.Symbol.Flags)
                                        ctx.Session.getDeclaredTypeOfSymbol

                                let! value =
                                    ask (hasAny SymbolFlags.Value export.Symbol.Flags) ctx.Session.getTypeOfSymbol

                                return export, declared, value
                            })
                        // Sequential, unlike every other fan-out in this tier: asking for a
                        // declared type is what *creates* it in the checker, and a type alias
                        // stamps its name on the type it creates. Two aliases with the same right
                        // side (`type A = X & Y; type B = X & Y`) therefore race - whichever is
                        // asked for first owns the intersection, and the other either aliases it
                        // or widens. Under Async.Parallel that ordering came out of the thread
                        // pool, so the same package generated two different files.
                        |> Async.Sequential

                    let answered (response: Result<TypeResponse, string> option) =
                        response |> Option.bind Result.toOption

                    let exportTypes =
                        resolved
                        |> Array.fold
                            (fun map (export, declared, value) ->
                                Map.add
                                    export.Symbol.SymbolId
                                    {
                                        Declared = answered declared |> Option.map _.TypeId
                                        Value = answered value |> Option.map _.TypeId
                                        AliasParameters = None
                                    }
                                    map)
                            model.ExportTypes

                    let types =
                        resolved
                        |> Array.collect (fun (_, declared, value) ->
                            [|
                                yield! Option.toArray (answered declared)
                                yield! Option.toArray (answered value)
                            |])
                        |> Array.fold (fun map ty -> Map.add ty.TypeId (TypeFacts.shallow ty) map) model.Types

                    // An export whose type the compiler would not hand over has nothing to shape.
                    // It drops here, loudly: the finding is what tells `audit-coverage` that the
                    // absence is owned.
                    let findings =
                        [
                            for export, declared, value in resolved do
                                for facet, response in [ "declared type", declared; "value type", value ] do
                                    match response with
                                    | Some(Error reason) ->
                                        Finding.make
                                            export.ExportName
                                            (ResolveExportTypes.FacetNotResolved(facet, reason))
                                    | _ -> ()
                        ]

                    let model =
                        { model with
                            ExportTypes = exportTypes
                            Types = types
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

/// The source-file blobs one connection has already been asked for, by snapshot, project and
/// path - the scope a node handle is meaningful in. Held against the connection itself, so a
/// finished generation drops its blobs with the compiler process that served them.
let private blobs =
    System.Runtime.CompilerServices.ConditionalWeakTable<
        TscMailbox,
        System.Collections.Concurrent.ConcurrentDictionary<
            struct (int * string * string),
            Lazy<System.Threading.Tasks.Task<Ast.SourceFile voption>>
         >
     >()

let private sourceFile (ctx: Context) (path: string) : Async<Ast.SourceFile voption> =
    blobs
        .GetOrCreateValue(ctx.Session.Transport)
        .GetOrAdd(
            struct (ctx.Session.Snapshot, ctx.Session.Project, path),
            fun (struct (_, _, path)) ->
                lazy
                    (ctx.Session.getSourceFile (DocumentIdentifier.FileName path)
                     |> Async.StartAsTask)
        )
        .Value
    |> Async.AwaitTask

/// The description of a `/** */` block, up to its first tag line, with the comment markers and
/// each line's leading `*` removed.
let private jsDocDescription (block: string) =
    block.Substring(3, max 0 (block.Length - 5)).Replace("\r\n", "\n").Split '\n'
    |> Array.map (fun line ->
        let line = line.Trim()

        if line.StartsWith "*" then
            line.Substring(1).Trim()
        else
            line)
    |> Array.takeWhile (fun line -> not (line.StartsWith "@"))
    |> String.concat "\n"
    |> _.Trim()

/// A signature declaration's text as the description of the JSDoc written above it and the
/// declaration on one line, comments outside string literals removed:
/// `/** Closed. */ on(\n  event: "close",\n): this;` reads `"Closed."` and
/// `on(event: "close"): this`.
let internal signatureText (text: string) : string * string =
    let at (index: int) (token: string) =
        System.String.CompareOrdinal(text, index, token, 0, token.Length) = 0

    let skipComment (index: int) =
        if at index "//" then
            match text.IndexOf('\n', index) with
            | -1 -> text.Length
            | newline -> newline + 1
        else
            match text.IndexOf("*/", index + 2, System.StringComparison.Ordinal) with
            | -1 -> text.Length
            | close -> close + 2

    let mutable index = 0
    let mutable comment = ""
    let mutable leading = true

    while leading && index < text.Length do
        if System.Char.IsWhiteSpace text[index] then
            index <- index + 1
        elif at index "//" || at index "/*" then
            let next = skipComment index

            if at index "/**" && next - index >= 5 then
                comment <- jsDocDescription (text.Substring(index, next - index))

            index <- next
        else
            leading <- false

    let body = System.Text.StringBuilder()

    let space () =
        if body.Length > 0 && body[body.Length - 1] <> ' ' then
            body.Append ' ' |> ignore

    let mutable quote = None

    while index < text.Length do
        let c = text[index]

        match quote with
        | Some q ->
            body.Append c |> ignore

            if c = '\\' && index + 1 < text.Length then
                body.Append text[index + 1] |> ignore
                index <- index + 2
            else
                if c = q then
                    quote <- None

                index <- index + 1
        | None when c = '"' || c = '\'' || c = '`' ->
            quote <- Some c
            body.Append c |> ignore
            index <- index + 1
        | None when at index "//" || at index "/*" ->
            space ()
            index <- skipComment index
        | None when System.Char.IsWhiteSpace c ->
            space ()
            index <- index + 1
        | None ->
            body.Append c |> ignore
            index <- index + 1

    let spelling =
        body.ToString().Trim().Replace("( ", "(").Replace(" )", ")").Replace(",)", ")").TrimEnd(';').Trim()

    comment, spelling

/// The source of a signature declaration, where its node handle resolves to source text.
let private signatureSource (ctx: Context) (declaration: string voption) : Async<SignatureSource option> =
    async {
        match declaration |> ValueOption.bind NodeHandle.parse with
        | ValueNone -> return None
        | ValueSome handle ->
            match! attempt (sourceFile ctx handle.Path) with
            | Ok(ValueSome source) ->
                let node = Node.ofIndex<AnyNode> source handle.Index
                let text = Ast.sourceText source

                if node.Pos < 0 || node.End > text.Length || node.End <= node.Pos then
                    return None
                else
                    let comment, spelling =
                        signatureText (text.Substring(node.Pos, node.End - node.Pos))

                    return
                        Some
                            {
                                Spelling = spelling
                                Comment = comment
                            }
            | _ -> return None
    }

/// The identifier a declaration node at `handle` is named by: a labelled tuple element, or a
/// parameter whose name is an identifier.
let private declaredIdentifier (ctx: Context) (handle: string) : Async<string option> =
    async {
        match NodeHandle.parse handle with
        | ValueNone -> return None
        | ValueSome handle ->
            match! attempt (sourceFile ctx handle.Path) with
            | Ok(ValueSome source) ->
                let node = Node.ofIndex<AnyNode> source handle.Index

                let name =
                    match node.Kind with
                    | SyntaxKind.NamedTupleMember ->
                        Node.retag<AnyNode, NamedTupleMember> node
                        |> NamedTupleMember.name
                        |> ValueOption.bind _.Text
                    | SyntaxKind.Parameter ->
                        Node.retag<AnyNode, ParameterDeclaration> node
                        |> ParameterDeclaration.name
                        |> ValueOption.filter (fun name -> name.Kind = SyntaxKind.Identifier)
                        |> ValueOption.bind _.Text
                    | _ -> ValueNone

                return ValueOption.toOption name
            | _ -> return None
    }

/// The names an array binding pattern on a rest parameter writes, by position.
let private restPatternNames (ctx: Context) (parameter: SymbolResponse) : Async<string option list> =
    async {
        match parameter.Declarations |> ValueOption.defaultValue [||] with
        | [| declaration |] ->
            match NodeHandle.parse declaration with
            | ValueNone -> return []
            | ValueSome handle ->
                match! attempt (sourceFile ctx handle.Path) with
                | Ok(ValueSome source) ->
                    let node = Node.ofIndex<AnyNode> source handle.Index

                    if node.Kind <> SyntaxKind.Parameter then
                        return []
                    else
                        match Node.retag<AnyNode, ParameterDeclaration> node |> ParameterDeclaration.name with
                        | ValueSome name when name.Kind = SyntaxKind.ArrayBindingPattern ->
                            return
                                Node.retag<BindingName, BindingPattern> name
                                |> BindingPattern.elements
                                |> Seq.map (fun element ->
                                    if element.Kind <> SyntaxKind.BindingElement then
                                        None
                                    else
                                        match BindingElement.dotDotDotToken element, BindingElement.name element with
                                        | ValueNone, ValueSome name when name.Kind = SyntaxKind.Identifier ->
                                            ValueOption.toOption name.Text
                                        | _ -> None)
                                |> Seq.toList
                        | _ -> return []
                | _ -> return []
        | _ -> return []
    }

/// A type alias can bind unused parameters that disappear from its resolved type. Read the
/// declaring alias rather than attaching that fact to a type shared with nongeneric aliases.
let internal declarationHasTypeParameters (ctx: Context) (symbol: SymbolResponse) =
    async {
        if not (symbol.Flags.HasFlag SymbolFlags.TypeAlias) then
            return Ok false
        else
            let handles = symbol.Declarations |> ValueOption.defaultValue [||]

            let! declarations =
                handles
                |> Array.map (fun declaration ->
                    async {
                        match NodeHandle.parse declaration with
                        | ValueNone -> return Error "The alias has a malformed declaration handle"
                        | ValueSome handle ->
                            let! source = attempt (sourceFile ctx handle.Path)

                            match source with
                            | Error reason -> return Error reason
                            | Ok ValueNone -> return Error "The alias declaration source is unavailable"
                            | Ok(ValueSome source) ->
                                let node = Node.ofIndex<AnyNode> source handle.Index

                                if node.Kind <> SyntaxKind.TypeAliasDeclaration then
                                    return Error "The alias handle does not identify a type alias declaration"
                                else
                                    return
                                        Node.retag<AnyNode, TypeAliasDeclaration> node
                                        |> TypeAliasDeclaration.typeParameters
                                        |> Seq.isEmpty
                                        |> not
                                        |> Ok
                    })
                |> Async.Sequential

            if Array.isEmpty declarations then
                return Error "The alias has no declaration handles"
            else
                match
                    declarations
                    |> Array.tryPick (function
                        | Error reason -> Some reason
                        | Ok _ -> None)
                with
                | Some reason -> return Error reason
                | None -> return Ok(declarations |> Array.exists ((=) (Ok true)))
    }

let internal declarationMemberName (ctx: Context) (symbol: SymbolResponse) =
    async {
        let declarations = symbol.Declarations |> ValueOption.defaultValue [||]

        let! names =
            declarations
            |> Array.map (fun declaration ->
                async {
                    match NodeHandle.parse declaration with
                    | ValueNone -> return None
                    | ValueSome handle ->
                        let! source = sourceFile ctx handle.Path

                        match source with
                        | ValueNone -> return None
                        | ValueSome source ->
                            let node = Node.ofIndex<AnyNode> source handle.Index

                            let name =
                                match node.Kind with
                                | SyntaxKind.PropertySignature ->
                                    Node.retag<AnyNode, PropertySignatureDeclaration> node
                                    |> PropertySignatureDeclaration.name
                                | SyntaxKind.PropertyDeclaration ->
                                    Node.retag<AnyNode, PropertyDeclaration> node |> PropertyDeclaration.name
                                | SyntaxKind.MethodSignature ->
                                    Node.retag<AnyNode, MethodSignatureDeclaration> node
                                    |> MethodSignatureDeclaration.name
                                | SyntaxKind.MethodDeclaration ->
                                    Node.retag<AnyNode, MethodDeclaration> node |> MethodDeclaration.name
                                | _ -> ValueNone

                            return
                                name
                                |> ValueOption.bind (fun name ->
                                    match name.Kind with
                                    | SyntaxKind.Identifier
                                    | SyntaxKind.StringLiteral -> name.Text
                                    | SyntaxKind.NumericLiteral when name.Text = ValueSome symbol.Name -> name.Text
                                    | _ -> ValueNone)
                                |> ValueOption.toOption
                })
            |> Async.Sequential

        return
            match names |> Array.distinct with
            | [| Some name |] -> Some name
            | _ -> None
    }

/// Declaration sources referenced by selected type annotations, including imported aliases.
let internal declarationTypeClosure (ctx: Context) (roots: string<declHandle> list) =
    async {
        let mutable pending = roots
        let mutable visited = Set.empty
        let mutable generic = false
        let mutable failure = None

        while not pending.IsEmpty && failure.IsNone do
            let declaration = pending.Head
            pending <- pending.Tail

            if not (Set.contains declaration visited) then
                visited <- Set.add declaration visited

                if visited.Count > 512 then
                    failure <- Some "Selected declaration source closure exceeds 512 declarations"
                else
                    match NodeHandle.parse (declaration / uom<_>) with
                    | ValueNone -> failure <- Some "A selected type has a malformed declaration handle"
                    | ValueSome handle ->
                        let! source = attempt (sourceFile ctx handle.Path)

                        match source with
                        | Error reason -> failure <- Some reason
                        | Ok ValueNone -> failure <- Some "A selected declaration source is unavailable"
                        | Ok(ValueSome source) ->
                            let node = Node.ofIndex<AnyNode> source handle.Index

                            if node.Kind = SyntaxKind.TypeAliasDeclaration then
                                generic <-
                                    generic
                                    || (Node.retag<AnyNode, TypeAliasDeclaration> node
                                        |> TypeAliasDeclaration.typeParameters
                                        |> Seq.isEmpty
                                        |> not)

                            for child in Seq.append [ node ] (Node.descendants node) do
                                if child.Kind = SyntaxKind.ImportType || child.Kind = SyntaxKind.TypeQuery then
                                    failure <-
                                        Some
                                            "Import types and type queries are outside the authenticated source closure contract"
                                elif child.Kind = SyntaxKind.TypeReference then
                                    match TypeReferenceNode.typeName (Node.retag<AnyNode, TypeReferenceNode> child) with
                                    | ValueNone -> failure <- Some "A selected type reference has no name"
                                    | ValueSome name ->
                                        let location =
                                            NodeHandle.format
                                                {
                                                    Index = Node.index name
                                                    Kind = name.Kind
                                                    Path = handle.Path
                                                }

                                        let! symbol = ctx.Session.getSymbolAtLocation location

                                        match symbol with
                                        | ValueNone -> failure <- Some "A selected type reference has no symbol"
                                        | ValueSome symbol ->
                                            let! symbol =
                                                if symbol.Flags.HasFlag SymbolFlags.Alias then
                                                    ctx.Session.getAliasedSymbol symbol.Reference
                                                else
                                                    async.Return symbol

                                            if Grouping.classify ctx.PackageDir (ValueSome symbol) <> CompilerLib then
                                                let declarations =
                                                    symbol.DeclarationHandles
                                                    |> ValueOption.defaultValue [||]
                                                    |> Array.toList

                                                if declarations.IsEmpty then
                                                    failure <- Some "A selected type reference has no declaration"
                                                else
                                                    pending <- pending @ declarations

        return
            match failure with
            | Some reason -> Error reason
            | None -> Ok(Set.toList visited, generic)
    }

/// The instantiated interface contracts explicitly declared by a class's `implements` clauses.
let private implementedTypes (ctx: Context) (symbol: SymbolResponse voption) : Async<TypeResponse list> =
    async {
        let declarations =
            symbol
            |> ValueOption.bind _.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.choose (NodeHandle.parse >> ValueOption.toOption)

        let! contracts =
            declarations
            |> Array.map (fun handle ->
                async {
                    let! source = sourceFile ctx handle.Path

                    match source with
                    | ValueSome source ->
                        let node = Node.ofIndex<AnyNode> source handle.Index

                        if node.Kind = SyntaxKind.ClassDeclaration then
                            return!
                                Node.retag<AnyNode, ClassDeclaration> node
                                |> ClassDeclaration.heritageClauses
                                |> Seq.filter (fun clause ->
                                    HeritageClause.token clause = ValueSome SyntaxKind.ImplementsKeyword)
                                |> Seq.collect HeritageClause.types
                                |> Seq.map (fun contract ->
                                    let location =
                                        NodeHandle.format
                                            {
                                                Index = Node.index contract
                                                Kind = contract.Kind
                                                Path = handle.Path
                                            }

                                    ctx.Session.getTypeFromTypeNode location)
                                |> Async.Parallel
                        else
                            return [||]
                    | ValueNone -> return [||]
                })
            |> Async.Parallel

        return contracts |> Array.concat |> Array.distinctBy _.TypeId |> Array.toList
    }

/// The operand `M` of a type parameter declared `K extends keyof M`, read from the constraint
/// node.
let private keyOfOperandOf (ctx: Context) (parameter: SymbolResponse) : Async<TypeResponse voption> =
    async {
        match
            parameter.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.tryHead
            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)
        with
        | None -> return ValueNone
        | Some handle ->
            let! file = sourceFile ctx handle.Path

            let operand =
                match file with
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    if node.Kind = SyntaxKind.TypeParameter then
                        Node.retag<AnyNode, TypeParameterDeclaration> node
                        |> TypeParameterDeclaration.``constraint``
                        |> ValueOption.filter (fun bound -> bound.Kind = SyntaxKind.TypeOperator)
                        |> ValueOption.map Node.retag<TypeNode, TypeOperatorNode>
                        |> ValueOption.filter (fun bound ->
                            TypeOperatorNode.operator bound = ValueSome SyntaxKind.KeyOfKeyword)
                        |> ValueOption.bind TypeOperatorNode.``type``
                    else
                        ValueNone
                | ValueNone -> ValueNone

            match operand with
            | ValueSome operand ->
                let location =
                    NodeHandle.format
                        {
                            Index = Node.index operand
                            Kind = operand.Kind
                            Path = handle.Path
                        }

                let! resolved = ctx.Session.getTypeFromTypeNode location
                return ValueSome resolved
            | ValueNone -> return ValueNone
    }

/// Each source file's text, decoded once per blob.
let private sourceTexts =
    System.Runtime.CompilerServices.ConditionalWeakTable<byte[], string>()

let private textOf (file: Ast.SourceFile) : string =
    sourceTexts.GetValue(file.Data, fun _ -> Ast.sourceText file)

/// The node's leading trivia: the whitespace and comments between its full start and its first
/// token.
let private leadingTrivia (file: Ast.SourceFile) (index: int) =
    let text = textOf file
    let start = Ast.pos file index
    let finish = Ast.endPos file index

    if start < 0 || finish > text.Length || finish < start then
        ""
    else
        let span = text.Substring(start, finish - start)

        System.Text.RegularExpressions.Regex.Match(span, @"\A(?:\s+|/\*[\s\S]*?\*/|//[^\n]*)*").Value

/// The JSDoc a signature's own declaration writes, or `None` where the declaration is
/// unavailable.
let private ownDocOf (ctx: Context) (signature: SignatureResponse) : Async<OverloadDoc option> =
    async {
        match signature.Declaration |> ValueOption.bind NodeHandle.parse with
        | ValueNone -> return None
        | ValueSome handle ->
            let! file = sourceFile ctx handle.Path

            match file with
            | ValueNone -> return None
            | ValueSome file ->
                let blocks =
                    System.Text.RegularExpressions.Regex.Matches(leadingTrivia file handle.Index, @"/\*\*[\s\S]*?\*/")

                if blocks.Count = 0 then
                    return Some Undocumented
                else
                    return
                        blocks
                        |> Seq.collect (fun block ->
                            System.Text.RegularExpressions.Regex.Matches(block.Value, @"(?:^|[\s*])@(\w+)")
                            |> Seq.map _.Groups[1].Value)
                        |> Set.ofSeq
                        |> Documented
                        |> Some
    }

/// The operand `T` and value node of a mapped type declared `{ [P in keyof T]: V }`, where `V`
/// is independent of `P` and the mapping carries no modifiers or `as` clause.
let private mappedRecordOf (ctx: Context) (symbol: SymbolResponse) : Async<(TypeResponse * TypeResponse) voption> =
    async {
        match
            symbol.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.tryHead
            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)
        with
        | None -> return ValueNone
        | Some handle ->
            let! file = sourceFile ctx handle.Path

            let nodes =
                match file with
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    if node.Kind <> SyntaxKind.MappedType then
                        ValueNone
                    else
                        let mapped = Node.retag<AnyNode, MappedTypeNode> node

                        let unmodified =
                            (MappedTypeNode.readonlyToken mapped).IsNone
                            && (MappedTypeNode.questionToken mapped).IsNone
                            && (MappedTypeNode.nameType mapped).IsNone

                        match MappedTypeNode.typeParameter mapped, MappedTypeNode.``type`` mapped with
                        | ValueSome parameter, ValueSome value when unmodified ->
                            let key =
                                TypeParameterDeclaration.name parameter |> ValueOption.bind Identifier.text

                            let operand =
                                TypeParameterDeclaration.``constraint`` parameter
                                |> ValueOption.filter (fun bound -> bound.Kind = SyntaxKind.TypeOperator)
                                |> ValueOption.map Node.retag<TypeNode, TypeOperatorNode>
                                |> ValueOption.filter (fun bound ->
                                    TypeOperatorNode.operator bound = ValueSome SyntaxKind.KeyOfKeyword)
                                |> ValueOption.bind TypeOperatorNode.``type``

                            let valueText =
                                let text = textOf file
                                let start = Ast.pos file (Node.index value)
                                text.Substring(start, Ast.endPos file (Node.index value) - start)

                            match key, operand with
                            | ValueSome key, ValueSome operand when
                                not (
                                    System.Text.RegularExpressions.Regex.IsMatch(
                                        valueText,
                                        $@"(?<![\w$]){System.Text.RegularExpressions.Regex.Escape key}(?![\w$])"
                                    )
                                )
                                ->
                                ValueSome(Node.index operand, operand.Kind, Node.index value, value.Kind)
                            | _ -> ValueNone
                        | _ -> ValueNone
                | ValueNone -> ValueNone

            match nodes with
            | ValueSome(operandIndex, operandKind, valueIndex, valueKind) ->
                let location index kind =
                    NodeHandle.format
                        {
                            Index = index
                            Kind = kind
                            Path = handle.Path
                        }

                let! operand = ctx.Session.getTypeFromTypeNode (location operandIndex operandKind)
                let! value = ctx.Session.getTypeFromTypeNode (location valueIndex valueKind)
                return ValueSome(operand, value)
            | ValueNone -> return ValueNone
    }

/// Whether a parameter declares the `?` that makes it omittable.
///
/// For a parameter the marker lives on the declaration node alone, reached through the symbol's
/// handle. `SymbolFlags.Optional` states it for a property, and `CheckFlags.OptionalParameter`
/// for the synthetic parameters of a combined signature.
let private declaresQuestionToken (ctx: Context) (parameter: SymbolResponse) : Async<bool> =
    async {
        match
            parameter.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.tryHead
            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)
        with
        | None -> return false
        | Some handle ->
            let! file = sourceFile ctx handle.Path

            return
                match file with
                | ValueNone -> false
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    node.Kind = SyntaxKind.Parameter
                    && Node.retag<AnyNode, ParameterDeclaration> node
                       |> ParameterDeclaration.questionToken
                       |> ValueOption.isSome
    }

/// Whether a type parameter is declared by an `infer` clause: its declaration is a type parameter
/// node under an `InferType` node.
let private declaredByInfer (ctx: Context) (parameter: TypeResponse) : Async<bool> =
    async {
        let! symbol = ctx.Session.getSymbolOfType parameter.Id

        match
            symbol
            |> ValueOption.bind _.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.tryHead
            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)
        with
        | None -> return false
        | Some handle ->
            let! file = sourceFile ctx handle.Path

            return
                match file with
                | ValueNone -> false
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    node.Kind = SyntaxKind.TypeParameter
                    && Node.parent node
                       |> ValueOption.exists (fun parent -> parent.Kind = SyntaxKind.InferType)
    }

let private isLibFile (declaration: string) =
    match NodeHandle.parse declaration with
    | ValueSome declaration ->
        let file = System.IO.Path.GetFileName declaration.Path

        file.StartsWith("lib.", System.StringComparison.Ordinal)
        && file.EndsWith(".d.ts", System.StringComparison.Ordinal)
    | ValueNone -> false

/// A program over the compiler's complete ECMAScript lib (`esnext`), rooted at the lib file
/// declaring the program's global `Symbol`, one per connection and created on first use. `None`
/// when the program declares no lib `Symbol` to root it at.
let private compilerLibPrograms =
    System.Runtime.CompilerServices.ConditionalWeakTable<
        TscMailbox,
        Lazy<System.Threading.Tasks.Task<Session<TscMailbox> option>>
     >()

let private compilerLibProgram (ctx: Context) : Async<Session<TscMailbox> option> =
    compilerLibPrograms
        .GetValue(
            ctx.Session.Transport,
            fun mailbox ->
                lazy
                    (async {
                        let! symbol = ctx.Session.resolveName ("Symbol", SymbolFlags.Value)

                        let root =
                            symbol
                            |> ValueOption.bind _.Declarations
                            |> ValueOption.defaultValue [||]
                            |> Array.tryFind isLibFile
                            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)

                        match root with
                        | None -> return None
                        | Some root ->
                            let! snapshot =
                                mailbox.createSnapshot (
                                    createPrograms =
                                        [|
                                            { CreateSnapshotProgramParams.Default with
                                                RootFiles = ValueSome [| DocumentIdentifier.FileName root.Path |]
                                                CompilerOptions =
                                                    { RawCompilerOptions.Default with
                                                        Lib = ValueSome [| "esnext" |]
                                                    }
                                            }
                                        |]
                                )

                            return Some(mailbox.Session snapshot)
                     }
                     |> Async.StartAsTask)
        )
        .Value
    |> Async.AwaitTask

/// Whether the compiler's complete ECMAScript lib declares `Symbol.<key>`, independent of the
/// program's `lib` option.
let private compilerLibDeclaresSymbol (ctx: Context) (key: string) : Async<bool> =
    async {
        match! compilerLibProgram ctx with
        | None -> return false
        | Some session ->
            match! session.resolveName ("Symbol", SymbolFlags.Value) with
            | ValueNone -> return false
            | ValueSome symbol ->
                let! constructor = session.getTypeOfSymbol symbol.Reference

                match! session.getPropertyOfType (constructor.Id, key) with
                | ValueNone -> return false
                | ValueSome property ->
                    return property.Declarations |> ValueOption.defaultValue [||] |> Array.exists isLibFile
    }

/// Whether a class member declaration is TypeScript-private: it carries the `private` modifier,
/// or its name is a private identifier (`#name`). A `protected` member and a public member named
/// `_name` both read `false`.
let private declarationPrivate (ctx: Context) (declaration: string) : Async<bool> =
    async {
        match NodeHandle.parse declaration with
        | ValueNone -> return false
        | ValueSome handle ->
            let! file = sourceFile ctx handle.Path

            return
                match file with
                | ValueNone -> false
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    let modifiers, name =
                        match node.Kind with
                        | SyntaxKind.PropertyDeclaration ->
                            let node = Node.retag<AnyNode, PropertyDeclaration> node
                            PropertyDeclaration.modifiers node, PropertyDeclaration.name node
                        | SyntaxKind.MethodDeclaration ->
                            let node = Node.retag<AnyNode, MethodDeclaration> node
                            MethodDeclaration.modifiers node, MethodDeclaration.name node
                        | SyntaxKind.GetAccessor ->
                            let node = Node.retag<AnyNode, GetAccessorDeclaration> node
                            GetAccessorDeclaration.modifiers node, GetAccessorDeclaration.name node
                        | SyntaxKind.SetAccessor ->
                            let node = Node.retag<AnyNode, SetAccessorDeclaration> node
                            SetAccessorDeclaration.modifiers node, SetAccessorDeclaration.name node
                        | SyntaxKind.Constructor ->
                            ConstructorDeclaration.modifiers (Node.retag<AnyNode, ConstructorDeclaration> node),
                            ValueNone
                        | SyntaxKind.Parameter ->
                            ParameterDeclaration.modifiers (Node.retag<AnyNode, ParameterDeclaration> node), ValueNone
                        | _ -> Seq.empty, ValueNone

                    modifiers
                    |> Seq.exists (fun modifier -> modifier.Kind = SyntaxKind.PrivateKeyword)
                    || name
                       |> ValueOption.exists (fun name -> name.Kind = SyntaxKind.PrivateIdentifier)
    }

/// Whether every declaration of a member is TypeScript-private (`declarationPrivate`). A member
/// with no declaration reads `false`, as does an accessor pair whose getter is public.
let private declaredPrivate (ctx: Context) (declarations: string array voption) : Async<bool> =
    async {
        match declarations |> ValueOption.defaultValue [||] with
        | [||] -> return false
        | declarations ->
            let! verdicts = declarations |> Array.map (declarationPrivate ctx) |> Async.Parallel
            return Array.forall id verdicts
    }

/// Whether an accessor property's setter is TypeScript-private behind a public getter: every
/// `set` declaration is private and some declaration is not. Outside the class the property
/// reads as `readonly`.
let private privateSetter (ctx: Context) (declarations: string array voption) : Async<bool> =
    async {
        let declarations = declarations |> ValueOption.defaultValue [||]

        let! facts =
            declarations
            |> Array.map (fun declaration ->
                async {
                    let! isPrivate = declarationPrivate ctx declaration

                    let isSetter =
                        NodeHandle.parse declaration
                        |> ValueOption.exists (fun handle -> handle.Kind = SyntaxKind.SetAccessor)

                    return isSetter, isPrivate
                })
            |> Async.Parallel

        let setters = facts |> Array.filter fst

        return
            setters.Length > 0
            && setters |> Array.forall snd
            && facts |> Array.exists (snd >> not)
    }

/// The properties of a type that a binding carries, every property except a TypeScript-private
/// one (`declaredPrivate`), and whether the type has a private property.
let private publicPropertiesOfType (ctx: Context) (ty: TypeResponse) : Async<SymbolResponse array * bool> =
    async {
        let! properties = ctx.Session.getPropertiesOfType ty.Id
        let properties = properties |> ValueOption.defaultValue [||]

        let! privacy =
            properties
            |> Array.map (fun property -> declaredPrivate ctx property.Declarations)
            |> Async.Parallel

        return Array.zip properties privacy |> Array.filter (snd >> not) |> Array.map fst, Array.contains true privacy
    }

/// The well-known symbol referenced by a member's computed key: `Some "iterator"` for
/// `[Symbol.iterator]`. A well-known symbol is a `SymbolConstructor` property declared by a
/// `lib.*.d.ts` file of the program, or, when a package's augmentation is the program's only
/// declaration (`Symbol.asyncDispose` under `lib: ["es2020"]`), by the compiler's complete
/// ECMAScript lib; the key then resolves to a property of the program's global
/// `SymbolConstructor` through the global `Symbol`. A key referencing a package's own
/// `unique symbol` reads `None`, including one escaped as `__@iterator@<id>` or keyed through a
/// namespace-local `Symbol`, as does a `SymbolConstructor` property absent from the compiler's lib
/// (`Symbol.observable`).
let private wellKnownSymbol (ctx: Context) (property: SymbolResponse) : Async<string option> =
    async {
        match
            property.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.tryHead
            |> Option.bind (NodeHandle.parse >> ValueOption.toOption)
        with
        | _ when not (property.Name.StartsWith "__@") -> return None
        | None -> return None
        | Some handle ->
            let! file = sourceFile ctx handle.Path

            let candidate =
                match file with
                | ValueNone -> None
                | ValueSome file ->
                    let node = Node.ofIndex<AnyNode> file handle.Index

                    let name =
                        match node.Kind with
                        | SyntaxKind.PropertySignature ->
                            Node.retag<AnyNode, PropertySignatureDeclaration> node
                            |> PropertySignatureDeclaration.name
                        | SyntaxKind.PropertyDeclaration ->
                            Node.retag<AnyNode, PropertyDeclaration> node |> PropertyDeclaration.name
                        | SyntaxKind.MethodSignature ->
                            Node.retag<AnyNode, MethodSignatureDeclaration> node
                            |> MethodSignatureDeclaration.name
                        | SyntaxKind.MethodDeclaration ->
                            Node.retag<AnyNode, MethodDeclaration> node |> MethodDeclaration.name
                        | SyntaxKind.GetAccessor ->
                            Node.retag<AnyNode, GetAccessorDeclaration> node |> GetAccessorDeclaration.name
                        | SyntaxKind.SetAccessor ->
                            Node.retag<AnyNode, SetAccessorDeclaration> node |> SetAccessorDeclaration.name
                        | _ -> ValueNone

                    match name with
                    | ValueSome name when name.Kind = SyntaxKind.ComputedPropertyName ->
                        match
                            Node.retag<PropertyName, ComputedPropertyName> name
                            |> ComputedPropertyName.expression
                        with
                        | ValueSome access when access.Kind = SyntaxKind.PropertyAccessExpression ->
                            let access = Node.retag<Expression, PropertyAccessExpression> access

                            match PropertyAccessExpression.expression access, PropertyAccessExpression.name access with
                            | ValueSome receiver, ValueSome key when
                                receiver.Kind = SyntaxKind.Identifier
                                && receiver.Text = ValueSome "Symbol"
                                && key.Kind = SyntaxKind.Identifier
                                ->
                                key.Text
                                |> ValueOption.toOption
                                |> Option.map (fun text ->
                                    text, (Node.index key, key.Kind), (Node.index receiver, receiver.Kind))
                            | _ -> None
                        | _ -> None
                    | _ -> None

            match candidate with
            | None -> return None
            | Some(text, (keyIndex, keyKind), (receiverIndex, receiverKind)) ->
                let symbolAt index kind =
                    ctx.Session.getSymbolAtLocation (
                        NodeHandle.format
                            {
                                Index = index
                                Kind = kind
                                Path = handle.Path
                            }
                    )

                let! symbol = symbolAt keyIndex keyKind

                let declarations =
                    symbol |> ValueOption.bind _.Declarations |> ValueOption.defaultValue [||]

                let! declaredByLib =
                    if Array.exists isLibFile declarations then
                        async.Return true
                    elif Array.isEmpty declarations then
                        async.Return false
                    else
                        async {
                            let! receiver = symbolAt receiverIndex receiverKind
                            let! globalSymbol = ctx.Session.resolveName ("Symbol", SymbolFlags.Value)

                            match receiver, globalSymbol, symbol with
                            | ValueSome receiver, ValueSome globalSymbol, ValueSome symbol when
                                receiver.SymbolId = globalSymbol.SymbolId
                                ->
                                let! constructor = ctx.Session.getTypeOfSymbol globalSymbol.Reference
                                let! property = ctx.Session.getPropertyOfType (constructor.Id, text)

                                match property with
                                | ValueSome property when property.SymbolId = symbol.SymbolId ->
                                    return! compilerLibDeclaresSymbol ctx text
                                | _ -> return false
                            | _ -> return false
                        }

                return if declaredByLib then Some text else None
    }

let private declaresMergedFunction (ctx: Context) (property: SymbolResponse) =
    async {
        let! declarations =
            property.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.map (fun declaration ->
                async {
                    match NodeHandle.parse declaration with
                    | ValueNone -> return None
                    | ValueSome handle ->
                        let! file = sourceFile ctx handle.Path

                        match file with
                        | ValueNone -> return None
                        | ValueSome file ->
                            let node = Node.ofIndex<AnyNode> file handle.Index

                            match node.Kind with
                            | SyntaxKind.FunctionDeclaration -> return Some true
                            | SyntaxKind.MethodSignature
                            | SyntaxKind.MethodDeclaration -> return Some false
                            | SyntaxKind.ExportSpecifier ->
                                let name = Node.retag<AnyNode, ExportSpecifier> node |> ExportSpecifier.name

                                match name with
                                | ValueNone -> return None
                                | ValueSome name ->
                                    let! symbol =
                                        ctx.Session.getSymbolAtLocation (
                                            NodeHandle.format
                                                {
                                                    Index = Node.index name
                                                    Kind = name.Kind
                                                    Path = handle.Path
                                                }
                                        )

                                    match symbol with
                                    | ValueSome symbol when symbol.Flags.HasFlag SymbolFlags.Alias ->
                                        let! target = ctx.Session.getAliasedSymbol symbol.Reference

                                        return
                                            if target.Flags.HasFlag SymbolFlags.Function then
                                                Some true
                                            else
                                                None
                                    | _ -> return None
                            | _ -> return None
                })
            |> Async.Parallel

        return
            Array.contains (Some true) declarations
            && Array.forall Option.isSome declarations
    }

let private resolveMember (ctx: Context) isProperty (property: SymbolResponse) =
    async {
        let! propertyType = ctx.Session.getTypeOfSymbol property.Reference
        let! docs = ctx.Session.getDocumentationComment property.Reference
        let! tags = ctx.Session.getJsDocTags property.Reference

        // `CheckFlags.Readonly` only marks transient symbols; a declared
        // `readonly` modifier is the checker's to see, so ask it.
        let! readOnly =
            if isProperty then
                async {
                    let! declared = ctx.Session.isReadonlySymbol property.Reference

                    if declared then
                        return true
                    else
                        return! privateSetter ctx property.Declarations
                }
            else
                async.Return false

        let! questionToken =
            if isProperty then
                async.Return false
            else
                declaresQuestionToken ctx property

        let! symbol =
            if isProperty then
                wellKnownSymbol ctx property
            else
                async.Return None

        let! resolvesToFunction =
            if not isProperty then
                async.Return false
            elif property.Flags.HasFlag SymbolFlags.Function then
                async.Return true
            elif property.Flags.HasFlag SymbolFlags.Alias then
                async {
                    let! target = ctx.Session.getAliasedSymbol property.Reference
                    return target.Flags.HasFlag SymbolFlags.Function
                }
            elif property.Flags.HasFlag SymbolFlags.Transient then
                declaresMergedFunction ctx property
            else
                async.Return false

        return
            {
                Symbol = property
                Docs = docs
                Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                Optional =
                    property.Flags.HasFlag SymbolFlags.Optional
                    || property.CheckFlags.HasFlag CheckFlags.OptionalParameter
                    || questionToken
                ReadOnly = readOnly
                TypeId = propertyType.TypeId
                WellKnownSymbol = symbol
                ResolvesToFunction = resolvesToFunction
                SourceStringAnnotation = None
            },
            propertyType
    }

let private resolveSignatures (ctx: Context) (trace: Trace option) (registers: Registers) (ty: TypeResponse) kind =
    let label =
        if kind = SignatureKind.Call then
            "call-signature"
        else
            "construct-signature"

    async {
        let! signatures = ctx.Session.getSignaturesOfType (ty.Id, kind)

        return!
            signatures
            |> Array.map (fun signature ->
                async {
                    let! parameters = ctx.Session.getParametersOfSignature signature.Id
                    let parameters = parameters |> ValueOption.defaultValue [||]

                    let! isPrivate =
                        match signature.Declaration with
                        | ValueSome declaration when kind = SignatureKind.Construct ->
                            declarationPrivate ctx declaration
                        | _ -> async.Return false

                    let! parameterFacts =
                        parameters |> Array.map (resolveMember ctx false) |> Async.Parallel

                    let! returnType = ctx.Session.getReturnTypeOfSignature signature.Id

                    // A generic callback alias spells its parameters on the
                    // signature, not the type, so `Mapper<T> = (t: T) => T` has
                    // nothing to bind without this call.
                    let! typeParameters = ctx.Session.getTypeParametersOfSignature signature.Id
                    let typeParameters = typeParameters |> ValueOption.defaultValue [||]

                    for parameter in typeParameters do
                        registers.SignatureParameters.Add parameter.TypeId

                    // The spelling documents a literal overload's method of its own (W5).
                    let! source =
                        if signatures.Length >= 2 then
                            signatureSource ctx signature.Declaration
                        else
                            async.Return None

                    // Per overload: the symbol's tags merge every declaration's.
                    let! ownDoc =
                        if signatures.Length > 1 then
                            ownDocOf ctx signature
                        else
                            async.Return None

                    let hasRest = signature.Flags.HasFlag SignatureFlags.HasRestParameter

                    let! restPattern =
                        match Array.tryLast parameters with
                        | Some rest when hasRest -> restPatternNames ctx rest
                        | _ -> async.Return []

                    return
                        {
                            Parameters = parameterFacts |> Array.map fst |> Array.toList
                            HasThisParameter = ValueOption.isSome signature.ThisParameter
                            HasRest = hasRest
                            TypeParameters = typeParameters |> Array.map _.TypeId |> Array.toList
                            IsAbstract = signature.Flags.HasFlag SignatureFlags.Abstract
                            IsPrivate = isPrivate
                            ReturnTypeId = returnType.TypeId
                            Source = source
                            OwnDoc = ownDoc
                            RestPatternNames = restPattern
                        },
                        [
                            yield! channel trace $"{label}-parameter" (parameterFacts |> Array.map snd |> Array.toList)
                            yield! channel trace $"{label}-type-parameter" (typeParameters |> Array.toList)
                            yield! channel trace $"{label}-return" [ returnType ]
                        ]
                })
            |> Async.Parallel
    }

/// The structure of a type that has members: properties, call and construct signatures, index
/// signatures, each with the responses it discovered. Object types and the intersections of
/// them share it, because the checker answers the same questions about both: the properties of
/// `A & B` are both sets, a property both declare typed as the intersection of its two types.
let private deriveStructure (ctx: Context) (trace: Trace option) (registers: Registers) (ty: TypeResponse) =
    async {
        let! properties, privateMembers = publicPropertiesOfType ctx ty

        let! members = properties |> Array.map (resolveMember ctx true) |> Async.Parallel

        let! callSignatures = resolveSignatures ctx trace registers ty SignatureKind.Call

        let! constructSignatures =
            resolveSignatures ctx trace registers ty SignatureKind.Construct

        // An index signature is not a property: `getPropertiesOfType` returns nothing at
        // all for `interface Bag { [key: string]: number }`, so without this the type
        // reaches the shape tier looking empty and is never declared. Key and value are
        // followed into the table like any other referenced type.
        let! indexInfos = ctx.Session.getIndexInfosOfType ty.Id

        let indexInfos = indexInfos |> ValueOption.defaultValue [||] |> Array.toList

        let discovered =
            [
                yield! channel trace "member-type" (members |> Array.map snd |> Array.toList)
                yield! callSignatures |> Array.collect (snd >> List.toArray)
                yield! constructSignatures |> Array.collect (snd >> List.toArray)
                yield!
                    channel
                        trace
                        "index-info"
                        [
                            for info in indexInfos do
                                info.KeyType
                                info.ValueType
                        ]
            ]

        return
            {|
                Members = members |> Array.map fst |> Array.toList
                PrivateMembers = privateMembers
                IndexInfos =
                    indexInfos
                    |> List.map (fun info ->
                        {
                            KeyTypeId = info.KeyType.TypeId
                            ValueTypeId = info.ValueType.TypeId
                            IsReadonly = info.IsReadonly = ValueSome true
                        })
                CallSignatures = callSignatures |> Array.map fst |> Array.toList
                ConstructSignatures = constructSignatures |> Array.map fst |> Array.toList
                Discovered = discovered
            |}
    }

/// Whether a type argument applies a type parameter a signature declares, directly or under a
/// union, intersection or nested reference up to `depth` levels down.
let rec private appliesSignatureParameter
    (ctx: Context)
    (registers: Registers)
    (depth: int)
    (argument: TypeResponse)
    : Async<bool> =
    async {
        let has flag = argument.Flags.HasFlag(flag: TypeFlags)

        let isReference =
            argument.ObjectFlags
            |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
            |> ValueOption.defaultValue false

        let rec any (candidates: TypeResponse list) =
            async {
                match candidates with
                | [] -> return false
                | candidate :: rest ->
                    let! applies = appliesSignatureParameter ctx registers (depth - 1) candidate
                    if applies then return true else return! any rest
            }

        if has TypeFlags.TypeParameter then
            return registers.SignatureParameters.Contains argument.TypeId
        elif depth = 0 then
            return false
        elif has TypeFlags.Union || has TypeFlags.Intersection then
            let! operands = ctx.Session.getTypesOfType argument.Id
            return! any (operands |> ValueOption.defaultValue [||] |> Array.toList)
        elif has TypeFlags.Object && isReference then
            let! arguments = ctx.Session.getTypeArguments argument.Id
            return! any (arguments |> ValueOption.defaultValue [||] |> Array.toList)
        else
            return false
    }

/// Derives one type's facts and reports the responses it discovered, for the next frontier.
let private deriveFacts
    (ctx: Context)
    (trace: Trace option)
    (registers: Registers)
    (ambientClasses: Map<SymbolKey, HarvestedExport>)
    (ty: TypeResponse)
    : Async<TypeFacts * TypeResponse list> =
    async {
        let has flag = ty.Flags.HasFlag(flag: TypeFlags)

        // `boolean` is a union of `true | false` wearing the Boolean flag; it maps as a
        // primitive, so its members are not worth a round trip.
        if has TypeFlags.Union && not (has TypeFlags.Boolean) then
            let! members = ctx.Session.getTypesOfType ty.Id

            let members =
                members
                |> ValueOption.map Array.toList
                |> ValueOption.defaultValue []
                |> Registers.structural registers

            // A generic union alias binds its parameter on the alias, exactly as an object or
            // conditional alias does: `type Ref<T> = T | ((value: T) => void)` has nowhere
            // else to put `T`, and without it the shape tier reads the arms under no scope
            // and widens every one to obj.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let unionAliasArguments = aliasTypeArguments |> ValueOption.defaultValue [||]

            let aliasTypeArguments =
                unionAliasArguments
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            let isNullish (member_: TypeResponse) =
                member_.Flags.HasFlag TypeFlags.Null
                || member_.Flags.HasFlag TypeFlags.Undefined

            let literals = members |> List.filter (isNullish >> not)

            let plainLiteralUnion =
                not (List.isEmpty literals)
                && literals
                   |> List.forall (fun member_ ->
                       let flags = member_.Flags

                       not (flags.HasFlag TypeFlags.Enum || flags.HasFlag TypeFlags.EnumLiteral)
                       && (flags.HasFlag TypeFlags.StringLiteral
                           || flags.HasFlag TypeFlags.NumberLiteral
                           || flags.HasFlag TypeFlags.BigIntLiteral
                           || flags.HasFlag TypeFlags.BooleanLiteral))

            let recoverAlias =
                (plainLiteralUnion || Array.isEmpty unionAliasArguments)
                && (ctx.Config.DeclarationCatalog
                    || not (List.isEmpty ctx.Config.DeclarationReferences))

            let! nonNullable =
                async {
                    if recoverAlias && members |> List.exists isNullish then
                        let! result = ctx.Session.getNonNullableType ty.Id

                        if result.Id <> ty.Id && result.AliasSymbol.IsSome then
                            let! arguments = ctx.Session.getAliasTypeArgumentsOfType result.Id

                            let! canonical =
                                async {
                                    if plainLiteralUnion then
                                        return true
                                    else
                                        let! symbol = ctx.Session.getAliasSymbolOfType result.Id

                                        match symbol with
                                        | ValueSome symbol ->
                                            let! declared = ctx.Session.getDeclaredTypeOfSymbol symbol.Reference
                                            return declared.Id = result.Id
                                        | ValueNone -> return false
                                }

                            return
                                if
                                    canonical
                                    && (plainLiteralUnion
                                        || (arguments |> ValueOption.defaultValue [||] |> Array.isEmpty))
                                then
                                    Some result
                                else
                                    None
                        else
                            return None
                    else
                        return None
                }

            // `getNonNullableType` returns the application a nullable union was written over
            // (`Job<T> | undefined` -> `Job<T>`) where the union's origin retains it. Its arguments
            // are what a reference to the application writes.
            let! nonNullableApplication =
                async {
                    let others = members |> List.filter (isNullish >> not)

                    if
                        nonNullable.IsNone
                        && others.Length >= 2
                        && others.Length < members.Length
                        && others
                           |> List.exists (fun member_ ->
                               member_.Flags.HasFlag TypeFlags.Object
                               || member_.Flags.HasFlag TypeFlags.Intersection)
                    then
                        let! result = ctx.Session.getNonNullableType ty.Id

                        if result.Id <> ty.Id && result.AliasSymbol.IsSome then
                            let! arguments = ctx.Session.getAliasTypeArgumentsOfType result.Id

                            match arguments with
                            | ValueSome arguments when arguments.Length > 0 ->
                                return Some(result, Array.toList arguments)
                            | _ -> return None
                        else
                            return None
                    else
                        return None
                }

            let! alias =
                async {
                    if recoverAlias && ty.AliasSymbol.IsSome then
                        let! symbol = ctx.Session.getAliasSymbolOfType ty.Id

                        match symbol with
                        | ValueSome symbol when not plainLiteralUnion ->
                            let! declared = ctx.Session.getDeclaredTypeOfSymbol symbol.Reference
                            return if declared.Id = ty.Id then ValueSome symbol else ValueNone
                        | symbol -> return symbol
                    else
                        return ValueNone
                }

            return
                { TypeFacts.shallow ty with
                    UnionMembers = members |> List.map _.TypeId
                    NonNullableAlias = nonNullable |> Option.map _.TypeId
                    NonNullableApplication = nonNullableApplication |> Option.map (fst >> _.TypeId)
                    AliasTypeArguments = aliasTypeArguments |> List.map _.TypeId
                    SymbolName = alias |> ValueOption.map _.SymbolName |> ValueOption.toOption
                    SymbolParent = alias |> ValueOption.bind _.ParentSymbolId |> ValueOption.toOption
                    Origin = Grouping.classify ctx.PackageDir alias
                    DeclFile =
                        Grouping.declFile alias
                        |> Option.map (fun value -> value * uom<Measure.declFile>)
                },
                channel trace "union-members" members
                @ channel trace "alias-type-arguments" aliasTypeArguments
                @ channel trace "nonnullable-alias" (Option.toList nonNullable)
                @ channel
                    trace
                    "nonnullable-application"
                    (nonNullableApplication
                     |> Option.map (fun (application, arguments) -> application :: arguments)
                     |> Option.defaultValue [])
        elif has TypeFlags.Intersection then
            // The constituents, followed into the table. A branding intersection (§4.6) is
            // decided by what its object operands *contain* - a marker property or a real
            // one - so the operands are resolved in full rather than identified.
            let! members = ctx.Session.getTypesOfType ty.Id

            let members =
                members
                |> ValueOption.map Array.toList
                |> ValueOption.defaultValue []
                |> Registers.structural registers

            // The alias's arguments, for the same reason an object alias needs them: a
            // flattened intersection is declared over them (§4.6), and a phantom is worth
            // nothing without the arity.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // An intersection of object types is a shape (§4.6): the checker's `getPropertiesOfType`
            // hands over both member sets flattened, a property both operands declare typed as the
            // intersection of its two types, so the shape tier declares it as one interface. Only
            // when every operand is an object: a primitive operand makes it a brand or nothing, and
            // a type-parameter operand (`T & { id: number }`) has no members to read until it is
            // instantiated. Asking would also drag the primitive's apparent members (`String`'s
            // whole prototype) into the table for nothing.
            let! structure =
                if
                    not members.IsEmpty
                    && members |> List.forall (fun m -> m.Flags.HasFlag TypeFlags.Object)
                then
                    async {
                        let! structure = deriveStructure ctx trace registers ty
                        return Some structure
                    }
                else
                    async.Return None

            return
                { TypeFacts.shallow ty with
                    Members = structure |> Option.map _.Members |> Option.defaultValue []
                    PrivateMembers = structure |> Option.exists _.PrivateMembers
                    IndexInfos = structure |> Option.map _.IndexInfos |> Option.defaultValue []
                    CallSignatures = structure |> Option.map _.CallSignatures |> Option.defaultValue []
                    ConstructSignatures = structure |> Option.map _.ConstructSignatures |> Option.defaultValue []
                    IntersectionMembers = members |> List.map _.TypeId
                    AliasTypeArguments = aliasTypeArguments |> List.map _.TypeId
                },
                channel trace "intersection-members" members
                @ channel trace "alias-type-arguments" aliasTypeArguments
                @ (structure |> Option.map _.Discovered |> Option.defaultValue [])
        elif has TypeFlags.ESSymbol || has TypeFlags.UniqueESSymbol then
            // The apparent type of `symbol` is the compiler lib's global `Symbol` interface,
            // followed as a nominal edge so a reference is written through the lib owner's name.
            let! apparent = ctx.Session.getApparentType ty.Id

            let apparent =
                if apparent.Id <> ty.Id && apparent.Flags.HasFlag TypeFlags.Object then
                    Some apparent
                else
                    None

            return
                { TypeFacts.shallow ty with
                    SymbolApparentType = apparent |> Option.map _.TypeId
                },
                channel trace "symbol-apparent" (Option.toList apparent)
        elif has TypeFlags.EnumLiteral then
            // An enum member: its symbol names the F# enum case (§4.7); the value is already
            // on the response.
            let! symbol = ctx.Session.getSymbolOfType ty.Id

            return
                { TypeFacts.shallow ty with
                    Origin = Grouping.classify ctx.PackageDir symbol
                    SymbolName = symbol |> ValueOption.map _.SymbolName |> ValueOption.toOption
                    DeclFile =
                        Grouping.declFile symbol
                        |> Option.map (fun value -> value * uom<Measure.declFile>)
                },
                []
        elif has TypeFlags.Object then
            let! symbol = ctx.Session.getSymbolOfType ty.Id
            let firstOrigin = Grouping.classify ctx.PackageDir symbol

            let origin =
                if firstOrigin <> CompilerLib then
                    firstOrigin
                else
                    symbol
                    |> ValueOption.bind _.Declarations
                    |> ValueOption.defaultValue [||]
                    |> Array.choose (NodeHandle.parse >> ValueOption.toOption)
                    |> Array.map (fun handle -> handle.Path * uom<filePath>)
                    |> Array.filter (fun file -> Grouping.classifyFile ctx.PackageDir file <> CompilerLib)
                    |> Array.sortBy (Grouping.sourceOrderKey ctx.PackageDir)
                    |> Array.tryHead
                    |> Option.map (Grouping.classifyFile ctx.PackageDir)
                    |> Option.defaultValue firstOrigin

            // Type arguments resolve for every group (O7 note): an external `Array<T>` or
            // `Promise<T>` carries entry-package types the walk must still reach.
            let isReference =
                ty.ObjectFlags
                |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
                |> ValueOption.defaultValue false

            let! typeArguments =
                if isReference then
                    ctx.Session.getTypeArguments ty.Id
                else
                    async.Return ValueNone

            let typeArguments =
                typeArguments |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            // A tuple's per-element flags live on its *target*, not on the reference the
            // checker hands back (verified live: the reference reports `elementFlags: null`).
            // The target is read for its flags and then dropped rather than followed - it is
            // the generic tuple type, so deriving it re-derives all of `Array.prototype` for
            // every distinct tuple shape.
            let! tupleElements =
                if ty.IsTupleType = ValueSome true then
                    async {
                        let! target = ctx.Session.getTargetOfType ty.Id

                        return
                            target.ElementFlags
                            |> ValueOption.map Array.toList
                            |> ValueOption.defaultValue []
                    }
                else
                    async.Return []

            // A generic alias hangs its parameters off the alias, not the type: the function
            // type behind `type Mapper<T> = (t: T) => T` reports none of its own. Only the
            // arguments that *are* type parameters bind anything - an instantiated alias
            // reports concrete arguments here, and those are already substituted in.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            // A reference-only alias still needs every application argument: its body is not
            // emitted here, so the substitution cannot be recovered from expanded members.
            let aliasArguments =
                aliasTypeArguments |> ValueOption.defaultValue [||] |> Array.toList

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // The O7 shortcut below rests on the group's types having names to be referenced
            // by. A mapped type has none: `Partial<Options>` is written in lib.es5.d.ts, so it
            // groups as the compiler lib, but it is a type-level *function* with no runtime
            // identity - what it stands for is the entry package's own operand, transformed.
            // Deferring to a name that does not exist widens the whole expansion to obj and
            // loses the operand with it (D6), so an anonymous shape is resolved by content
            // whatever group it was written in.
            let objectFlags = ty.ObjectFlags |> ValueOption.defaultValue ObjectFlags.None

            // Function and member values resolve their signatures by content.
            let isMemberType =
                match symbol with
                | ValueSome s ->
                    hasAny
                        (SymbolFlags.Function
                         ||| SymbolFlags.Method
                         ||| SymbolFlags.Property
                         ||| SymbolFlags.Signature)
                        s.Flags
                    && not (
                        hasAny
                            (SymbolFlags.Interface
                             ||| SymbolFlags.Class
                             ||| SymbolFlags.TypeAlias
                             ||| SymbolFlags.RegularEnum
                             ||| SymbolFlags.ConstEnum)
                            s.Flags
                    )
                | ValueNone -> false

            // The name a symbol supplies a declaration with. The checker's placeholders -
            // `__type`, `__object`, `__@iterator@194` - all carry the prefix.
            let named (candidate: SymbolResponse voption) =
                match candidate with
                | ValueSome s when not (s.Name.StartsWith "__") -> Some s.Name
                | _ -> None

            // The name a `ship` run of the group declares the shape under. An alias body carries
            // `__type` on the type's own symbol and its declared name on the alias symbol -
            // `type Pair = { left: Widget }` is the whole family - so a shape the type's own
            // symbol leaves unnamed takes the name on its alias. A shape both symbols leave
            // unnamed resolves by content, into a declaration of its own in the entry package.
            //
            // The round trip is spent only where the answer can be used: the entry group and the
            // two kinds above resolve by content whatever either symbol says.
            let! shapeName =
                if
                    GeneratorConfig.disposition ctx.Config origin = Ship
                    || objectFlags.HasFlag ObjectFlags.Mapped
                    || isMemberType
                then
                    async.Return None
                else
                    match named symbol with
                    | Some name -> async.Return(Some name)
                    | None ->
                        async {
                            let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                            return named alias
                        }

            let! namespacePath =
                async {
                    match shapeName, named symbol with
                    | Some _, None ->
                        let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                        return! namespacePathOf ctx alias
                    | _ -> return! namespacePathOf ctx symbol
                }

            if shapeName.IsSome then
                // Identity only (O7): the shape tier renders references to this group by
                // templated name or widens them, and either way nothing reads its members.
                // Core.TS preserves compiler-library generic bounds and heritage. Read that
                // declaration metadata without walking members, so a reference written as a
                // Core.TS name satisfies the same bounds.
                let readsNominal =
                    origin = CompilerLib
                    && (match Grouping.declFile symbol with
                        | Some file when Grouping.libFamily file = "Dom" -> true
                        | Some file ->
                            Grouping.isLibFile file
                            && shapeName
                               |> Option.exists (fun name ->
                                   (Naming.LibBindings.tryFind name).IsNone
                                   && (GeneratorConfig.disposition ctx.Config CompilerLib = Widen
                                       || Naming.LibBindings.isPinnedToCoreTs name))
                        | None -> false)

                let! nominal =
                    if readsNominal then
                        async {
                            let! target =
                                match ty.Target with
                                | ValueSome target when target <> ty.Id ->
                                    async {
                                        let! declaration = ctx.Session.getTargetOfType ty.Id
                                        return [ declaration ]
                                    }
                                | _ -> async.Return []

                            let! bases = ctx.Session.getBaseTypes ty.Id
                            return target, bases |> ValueOption.defaultValue [||] |> Array.toList
                        }
                    else
                        async.Return([], [])

                let targets, bases = nominal

                let referenceArguments =
                    if (named symbol).IsNone then
                        aliasArguments
                    else
                        typeArguments

                return
                    { TypeFacts.shallow ty with
                        Origin = origin
                        SymbolName = shapeName |> Option.map (fun value -> value * uom<Measure.symbolName>)
                        NamespacePath = namespacePath
                        DeclFile =
                            Grouping.declFile symbol
                            |> Option.map (fun value -> value * uom<Measure.declFile>)
                        TypeArguments = referenceArguments |> List.map _.TypeId
                        TupleElements = tupleElements
                        AliasTypeArguments = aliasTypeArguments |> List.map _.TypeId
                        BaseTypes = bases |> List.map _.TypeId
                    },
                    channel trace "type-arguments" referenceArguments
                    @ channel trace "alias-type-arguments" aliasTypeArguments
                    @ channel trace "target" targets
                    @ channel trace "base-types" bases
            else

                // The generic declaration behind an instantiation (§4.9). `Ready<T>` reached only
                // through `Resource<T> = Ready<T> | ...` is a reference whose target is the
                // declaration itself; with only the reference in the table the shape tier has
                // nothing to name but the instantiation, and its members read a parameter that
                // is not theirs. Entry group only: a lib or dependency target is identity at
                // most, and the reference already carries that. A tuple's target is its generic
                // carrier and is deliberately read for its flags and dropped, above.
                let isInstantiation =
                    isReference
                    && ty.IsTupleType <> ValueSome true
                    && (match ty.Target with
                        | ValueSome target -> target <> ty.Id
                        | ValueNone -> false)

                let! target =
                    if isInstantiation then
                        async {
                            let! target = ctx.Session.getTargetOfType ty.Id
                            return [ target ]
                        }
                    else
                        async.Return []

                // An instantiation at a reference position is identity: the declaration
                // applied to the arguments, which is how the shape tier writes it
                // (`instantiationOf`). Its members are read only where it stands as an operand
                // of a union or an intersection (`Ready<T>` inside `type Resource<T> = Ready<T>
                // | ...`, which the tagged-union and exclusive-arm passes read) or as a seed;
                // every other instantiation carries the declaration's members under
                // substitution, and deriving them is what widens the frontier: `lib.dom` applies
                // `Array<T>` to hundreds of element types, each dragging Array's methods and
                // their closure in, and `Array<T>.map<U>(...): Array<U>` is a distinct
                // instantiation every time the checker answers, each carrying the same methods
                // under a fresh parameter of its own - the latter is identity even as an
                // operand.
                let! isOpenOverSignature =
                    if isInstantiation then
                        async {
                            let! applies =
                                typeArguments
                                |> List.map (appliesSignatureParameter ctx registers 3)
                                |> Async.Sequential

                            return Array.exists id applies
                        }
                    else
                        async.Return false

                let asIdentity =
                    isInstantiation
                    && (isOpenOverSignature
                        || registers.IdentityOnly.Contains ty.TypeId
                        || not (registers.Structural.Contains ty.TypeId))

                if asIdentity then
                    if not isOpenOverSignature then
                        registers.IdentityOnly.Add ty.TypeId

                    return
                        { TypeFacts.shallow ty with
                            Origin = origin
                            SymbolName = symbol |> ValueOption.map _.SymbolName |> ValueOption.toOption
                            NamespacePath = namespacePath
                            DeclFile =
                                Grouping.declFile symbol
                                |> Option.map (fun value -> value * uom<Measure.declFile>)
                            SymbolParent = symbol |> ValueOption.bind _.ParentSymbolId |> ValueOption.toOption
                            TypeArguments = typeArguments |> List.map _.TypeId
                            AliasTypeArguments = aliasTypeArguments |> List.map _.TypeId
                        },
                        [
                            yield! channel trace "type-arguments" typeArguments
                            yield! channel trace "alias-type-arguments" aliasTypeArguments
                            yield! channel trace "target" target
                        ]
                else

                    let! structure = deriveStructure ctx trace registers ty
                    let! baseTypes = ctx.Session.getBaseTypes ty.Id

                    let! implemented =
                        if objectFlags.HasFlag ObjectFlags.Class then
                            implementedTypes ctx symbol
                        else
                            async.Return []

                    let! ambientClass, constructorDependencies =
                        async {
                            match
                                symbol
                                |> ValueOption.toOption
                                |> Option.bind (fun symbol -> Map.tryFind symbol.SymbolId ambientClasses)
                            with
                            | Some export when objectFlags.HasFlag ObjectFlags.Class ->
                                let! value = ctx.Session.getTypeOfSymbol export.Symbol.Reference
                                let! signatures = resolveSignatures ctx trace registers value SignatureKind.Construct

                                return
                                    Some(export, signatures |> Array.map fst |> Array.toList),
                                    signatures |> Array.toList |> List.collect snd
                            | _ -> return None, []
                        }

                    let baseTypes =
                        baseTypes |> ValueOption.map Array.toList |> ValueOption.defaultValue []

                    let! mappedRecord =
                        match symbol with
                        | ValueSome symbol when
                            objectFlags.HasFlag ObjectFlags.Mapped
                            && structure.Members.IsEmpty
                            && structure.IndexInfos.IsEmpty
                            ->
                            mappedRecordOf ctx symbol
                        | _ -> async.Return ValueNone

                    let discovered =
                        [
                            yield! structure.Discovered
                            yield!
                                channel
                                    trace
                                    "mapped-record"
                                    (match mappedRecord with
                                     | ValueSome(operand, value) -> [ operand; value ]
                                     | ValueNone -> [])
                            yield! constructorDependencies
                            yield! channel trace "base-types" baseTypes
                            yield! channel trace "implemented-types" implemented
                            yield! channel trace "type-arguments" typeArguments
                            yield! channel trace "alias-type-arguments" aliasTypeArguments
                            yield! channel trace "target" target
                        ]

                    return
                        {
                            Response = ty
                            Origin = origin
                            SymbolName = symbol |> ValueOption.map _.SymbolName |> ValueOption.toOption
                            DeclFile =
                                Grouping.declFile symbol
                                |> Option.map (fun value -> value * uom<Measure.declFile>)
                            SymbolParent = symbol |> ValueOption.bind _.ParentSymbolId |> ValueOption.toOption
                            SymbolFlags = symbol |> ValueOption.map _.Flags |> ValueOption.defaultValue SymbolFlags.None
                            NamespacePath = namespacePath
                            Members = structure.Members
                            PrivateMembers = structure.PrivateMembers
                            Declarations = []
                            DeclarationArguments = []
                            AliasDeclarations = []
                            IndexInfos = structure.IndexInfos
                            CallSignatures = structure.CallSignatures
                            ConstructSignatures = structure.ConstructSignatures
                            BaseTypes = baseTypes |> List.map _.TypeId
                            ImplementedTypes = implemented |> List.map _.TypeId
                            AmbientClass = ambientClass
                            TypeArguments = typeArguments |> List.map _.TypeId
                            TupleElements = tupleElements
                            TupleLabels = []
                            AliasTypeArguments = aliasTypeArguments |> List.map _.TypeId
                            IntersectionMembers = []
                            Constraint = None
                            Default = None
                            KeyOfOperand = None
                            KeyOfValues = []
                            MappedRecord =
                                mappedRecord
                                |> ValueOption.map (fun (operand, value) -> operand.TypeId, value.TypeId)
                                |> ValueOption.toOption
                            Conditional = None
                            UnionMembers = []
                            NonNullableAlias = None
                            NonNullableApplication = None
                            AliasIdentity = None
                            UnresolvedName = None
                            SymbolApparentType = None
                        },
                        discovered
        elif has TypeFlags.TypeParameter && ty.IsThisType <> ValueSome true then
            // A type parameter is named by its own symbol - `T`, not the declaration's - and
            // the response carries only the symbol's id, so the name costs a round trip. The
            // bound and the default are followed into the table: an unresolved constraint
            // cannot be rendered, and dropping one silently is the bug the manifest exists to
            // catch.
            let! symbol = ctx.Session.getSymbolOfType ty.Id
            let! bound = ctx.Session.getConstraintOfTypeParameter ty.Id
            let! fallback = ctx.Session.getDefaultFromTypeParameter ty.Id

            let! keyOfOperand =
                match bound, symbol with
                | ValueSome bound, ValueSome symbol when
                    bound.Flags.HasFlag TypeFlags.Union
                    || bound.Flags.HasFlag TypeFlags.StringLiteral
                    ->
                    keyOfOperandOf ctx symbol
                | _ -> async { return ValueNone }

            let! keyOfValues =
                match keyOfOperand with
                | ValueSome operand when operand.Flags.HasFlag TypeFlags.Object ->
                    async {
                        let! properties, _ = publicPropertiesOfType ctx operand

                        return!
                            properties
                            |> Array.map (fun property -> ctx.Session.getTypeOfSymbol property.Reference)
                            |> Async.Parallel
                    }
                | _ -> async.Return [||]

            let keyOfValues = Array.toList keyOfValues

            return
                { TypeFacts.shallow ty with
                    SymbolName = symbol |> ValueOption.map _.SymbolName |> ValueOption.toOption
                    DeclFile =
                        Grouping.declFile symbol
                        |> Option.map (fun value -> value * uom<Measure.declFile>)
                    Constraint = bound |> ValueOption.map _.TypeId |> ValueOption.toOption
                    Default = fallback |> ValueOption.map _.TypeId |> ValueOption.toOption
                    KeyOfOperand = keyOfOperand |> ValueOption.map _.TypeId |> ValueOption.toOption
                    KeyOfValues = keyOfValues |> List.map _.TypeId
                },
                [
                    yield! channel trace "constraint" (ValueOption.toList bound)
                    yield! channel trace "default" (ValueOption.toList fallback)
                    yield! channel trace "keyof-operand" (ValueOption.toList keyOfOperand)
                    yield! channel trace "keyof-values" keyOfValues
                ]
        elif has TypeFlags.Index then
            // `keyof T` at an operand the checker could not finish (§4.10). The operand is the
            // index type's target, and it is what the whole idiom is about - `keyof<'T>` needs
            // a `'T` - so it is followed rather than left dangling.
            let! operand = ctx.Session.getTargetOfType ty.Id
            return TypeFacts.shallow ty, channel trace "index-operand" [ operand ]
        elif has TypeFlags.IndexedAccess then
            // `T[K]`. Both halves are followed: the index is usually a key variable whose
            // constraint names the object, and reading one without the other cannot tell
            // `T[K]` apart from `T[keyof T]`.
            let! objectType = ctx.Session.getObjectTypeOfType ty.Id
            let! indexType = ctx.Session.getIndexTypeOfType ty.Id

            // The alias a single-argument reference is written through (§4.11's `NoInfer`):
            // the checker expands it to an indexed access before the shaper ever sees the
            // flags, so only the alias identity says which reference it was.
            let! alias = ctx.Session.getAliasSymbolOfType ty.Id
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id
            let aliasArguments = aliasTypeArguments |> ValueOption.defaultValue [||]

            let identity =
                match alias, aliasArguments with
                | ValueSome symbol, [| argument |] -> Some(symbol.SymbolName, argument.TypeId)
                | _ -> None

            return
                { TypeFacts.shallow ty with
                    AliasIdentity = identity
                },
                channel trace "indexed-access" ([ objectType; indexType ] @ Array.toList aliasArguments)
        else
            // A conditional, a template literal or an intrinsic string mapping. None of these
            // has a structure to read - each is a type-level computation over an argument the
            // checker could not supply - but the *arguments* are what the declaration binds,
            // and without them `type Unwrap<T> = ...` reaches the shape tier looking like a
            // plain alias with nothing generic about it.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // A template literal type is interned by its texts and operands, so it carries no
            // alias of its own: `` type Prefixed<T extends string> = `x-${T}` `` reports no
            // alias arguments at all. Its operands are the parameters it binds, and those it
            // does report.
            let! operands =
                if has TypeFlags.TemplateLiteral && List.isEmpty aliasTypeArguments then
                    ctx.Session.getTypesOfType ty.Id
                else
                    async.Return ValueNone

            let operands =
                operands
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun operand -> operand.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            let bound =
                channel trace "alias-type-arguments" aliasTypeArguments
                @ channel trace "template-operands" operands

            // A conditional's two branches (§4.11). Both responses are read - choosing between
            // them is what they are for - and only the branch the mapping takes is followed
            // into the table, so a pair that stays deferred leaves the frontier alone.
            let! conditional =
                if has TypeFlags.Conditional then
                    async {
                        let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                        let! whenTrue = ctx.Session.getTrueTypeOfConditionalType ty.Id
                        let! whenFalse = ctx.Session.getFalseTypeOfConditionalType ty.Id

                        // The condition's two sides. They are requested rather than read off
                        // `ty.CheckType`/`ty.ExtendsType`: the ids on a response are handles the
                        // compiler registers when it hands the type over, and an id nothing has
                        // asked for is rejected by the next request that names it.
                        let! check = ctx.Session.getCheckTypeOfType ty.Id
                        let! against = ctx.Session.getExtendsTypeOfType ty.Id

                        // `isTypeAssignableTo` reads a type parameter through its bound, so this
                        // asks whether every argument the head admits satisfies the condition.
                        // The checker defers a conditional it cannot answer for the parameter
                        // *without* its bound, so one it deferred can still be decided here.
                        // The reading holds while the parameter is the source. As the target a
                        // bound answers for a type an argument need only be assignable to, so
                        // `undefined extends T ? [] : [value: T]` and its kind stay deferred.
                        let! decided =
                            if against.Flags.HasFlag TypeFlags.TypeParameter then
                                async.Return false
                            else
                                ctx.Session.isTypeAssignableTo (check.Id, against.Id)

                        let uninhabited (branch: TypeResponse) = branch.Flags.HasFlag TypeFlags.Never

                        let branch =
                            if decided then
                                // A proven condition over an uninhabited true branch names an
                                // uninhabited type, which F# writes in no reference position.
                                if uninhabited whenTrue then
                                    None
                                else
                                    Some("true", whenTrue)
                            elif uninhabited whenTrue && not (uninhabited whenFalse) then
                                Some("sole inhabited", whenFalse)
                            elif uninhabited whenFalse && not (uninhabited whenTrue) then
                                Some("sole inhabited", whenTrue)
                            else
                                None

                        // The true branch reaches back into the condition: a `T` on that side
                        // arrives as a substitution type, `T` refined by what the condition
                        // proved of it. F# writes the parameter, so the refinement is peeled
                        // off - it is the same knowledge the head already carries.
                        let rec peeled (branch: TypeResponse) =
                            async {
                                if branch.Flags.HasFlag TypeFlags.Substitution then
                                    let! bare = ctx.Session.getBaseTypeOfType branch.Id
                                    return! peeled bare
                                else
                                    return branch
                            }

                        let! taken =
                            match branch with
                            | Some(side, whichever) ->
                                async {
                                    let! bare = peeled whichever
                                    return Some(side, bare)
                                }
                            | None -> async.Return None

                        // The conditional instantiated at its check type's bound (W9): read where no
                        // branch is taken, or where the taken branch is an `infer` variable, over a
                        // check type that is a bounded type parameter. An alias's own declaration
                        // keeps its deferred body.
                        let! wanted =
                            if not ctx.Config.ResolveConditionalConstraints then
                                async.Return false
                            else
                                match taken with
                                | None -> async.Return true
                                | Some(_, bare) when bare.Flags.HasFlag TypeFlags.TypeParameter ->
                                    declaredByInfer ctx bare
                                | Some _ -> async.Return false

                        let! constrained =
                            if not wanted then
                                async.Return None
                            else
                                async {
                                    let! checkBare = peeled check

                                    let! bound =
                                        if checkBare.Flags.HasFlag TypeFlags.TypeParameter then
                                            ctx.Session.getConstraintOfTypeParameter checkBare.Id
                                        else
                                            async.Return ValueNone

                                    // Unbounded, or the alias's own declaration.
                                    let! refused =
                                        match bound, alias with
                                        | ValueNone, _ -> async.Return true
                                        | ValueSome _, ValueNone -> async.Return false
                                        | ValueSome _, ValueSome alias ->
                                            async {
                                                let! declared = ctx.Session.getDeclaredTypeOfSymbol alias.Reference
                                                return declared.Id = ty.Id
                                            }

                                    if refused then
                                        return None
                                    else
                                        let! resolved = ctx.Session.getBaseConstraintOfType ty.Id

                                        // The checker's distributive constraint (footguns: not
                                        // `getConstraintOfType`). A union constraint is the
                                        // branch union §4.11 rejects, and is refused whole.
                                        match resolved with
                                        | ValueSome c when
                                            c.Id <> ty.Id
                                            && c.Id <> whenTrue.Id
                                            && c.Id <> whenFalse.Id
                                            && not (
                                                c.Flags.HasFlag TypeFlags.Any
                                                || c.Flags.HasFlag TypeFlags.Unknown
                                                || c.Flags.HasFlag TypeFlags.Never
                                                || c.Flags.HasFlag TypeFlags.Conditional
                                                || (c.Flags.HasFlag TypeFlags.Union
                                                    && not (c.Flags.HasFlag TypeFlags.Boolean))
                                            )
                                            ->
                                            return Some c
                                        | _ -> return None
                                }

                        return
                            Some
                                {
                                    Name = alias |> ValueOption.map _.Name |> ValueOption.toOption
                                    Branch = taken |> Option.map (fun (side, bare) -> side, bare.TypeId)
                                    Constraint = constrained |> Option.map _.TypeId
                                },
                            channel trace "conditional-branch" (taken |> Option.map snd |> Option.toList)
                            @ channel trace "conditional-constraint" (Option.toList constrained)
                    }
                else
                    async.Return(None, [])

            // An error type built for an unresolved reference carries the written name on its alias
            // symbol (`isErrorType`: Any with an alias): the rightmost identifier, under one
            // unresolved parent per qualifier.
            let! unresolvedName =
                if has TypeFlags.Any && ty.AliasSymbol.IsSome then
                    async {
                        let rec qualified (symbol: SymbolResponse) (name: string) =
                            async {
                                if symbol.CheckFlags.HasFlag CheckFlags.Unresolved && symbol.Parent.IsSome then
                                    let! parent = ctx.Session.Sessionless.getParentOfSymbol symbol.Reference

                                    match parent with
                                    | ValueSome parent -> return! qualified parent $"{parent.Name}.{name}"
                                    | ValueNone -> return name
                                else
                                    return name
                            }

                        let! alias = ctx.Session.getAliasSymbolOfType ty.Id

                        match alias with
                        | ValueSome alias ->
                            let! name = qualified alias alias.Name
                            return Some(name * uom<symbolName>)
                        | ValueNone -> return None
                    }
                else
                    async.Return None

            return
                { TypeFacts.shallow ty with
                    AliasTypeArguments = bound |> List.map _.TypeId
                    Conditional = fst conditional
                    UnresolvedName = unresolvedName
                },
                bound @ snd conditional
    }

/// Builds the closed type table: derive the current frontier (sorted by id, purely for a
/// readable trace - a checker id is assigned in the order answers arrived, so it names no
/// subset of a generation, only the generation as a whole), collect what derivation discovered,
/// and recurse until the frontier is exhausted or a cutoff records the remainder.
/// `observe` receives each source-contract walk's visit count and table size.
let internal resolveTypeTableObserved followWidth (observe: int -> int -> unit) : Pass<ResolveModel> =
    {
        Name = "resolve-type-table"
        Run =
            fun ctx model ->
                async {
                    let trace = Trace.start ()

                    // Counts every distinct id one generation of the frontier newly reaches,
                    // including entries deferred by the width cutoff -
                    // gated by `XANTHAM_RESOLVE_COUNTERS` so it costs nothing on a normal run.
                    // The final table size is read off `table` once the walk closes; `maxWidth`
                    // is the widest single generation the walk demanded, before the cutoff.
                    let mutable expansions = 0
                    let mutable maxWidth = 0

                    let registers = Registers.start ()

                    let ambientClasses =
                        model.Harvest.AmbientClasses
                        |> List.map (fun export -> export.Symbol.SymbolId, export)
                        |> Map.ofList

                    let declarationTypes =
                        ConcurrentDictionary<string, Lazy<System.Threading.Tasks.Task<int>>>()

                    let sourceDeclarations =
                        ConcurrentDictionary<int, Lazy<System.Threading.Tasks.Task<string list>>>()

                    let sourceDeclarationHandles (ty: TypeResponse) =
                        sourceDeclarations
                            .GetOrAdd(
                                ty.Id,
                                fun _ ->
                                    lazy
                                        (async {
                                            let! symbols =
                                                [
                                                    ctx.Session.getSymbolOfType ty.Id
                                                    ctx.Session.getAliasSymbolOfType ty.Id
                                                ]
                                                |> Async.Parallel

                                            return
                                                symbols
                                                |> Array.collect (
                                                    ValueOption.bind _.Declarations >> ValueOption.defaultValue [||]
                                                )
                                                |> Array.distinct
                                                |> Array.sort
                                                |> Array.toList
                                         }
                                         |> Async.StartAsTask)
                            )
                            .Value
                        |> Async.AwaitTask

                    let canonicalSourceType (ty: TypeResponse) =
                        async {
                            let! handles = sourceDeclarationHandles ty

                            let mutable canonical = false

                            for handle in handles do
                                if not canonical then
                                    let lookup =
                                        declarationTypes.GetOrAdd(
                                            handle,
                                            fun location ->
                                                lazy
                                                    (async {
                                                        let! result = attempt (ctx.Session.getTypeAtLocation location)

                                                        return
                                                            match result with
                                                            | Ok value -> value.Id
                                                            | Error _ -> -1
                                                     }
                                                     |> Async.StartAsTask)
                                        )

                                    let! declared = lookup.Value |> Async.AwaitTask
                                    canonical <- declared = ty.Id

                            return canonical
                        }

                    let! seeds =
                        model.Types
                        |> Map.toList
                        |> List.filter (fun (_, facts) ->
                            facts.Response.AliasTypeArguments |> ValueOption.exists (Array.isEmpty >> not))
                        |> List.map (fun (id, facts) ->
                            async {
                                let! canonical = canonicalSourceType facts.Response
                                return id, canonical
                            })
                        |> Async.Parallel

                    let canonicalSeedAliases =
                        seeds
                        |> Array.choose (fun (id, canonical) -> if canonical then Some id else None)
                        |> Set.ofArray

                    let declaredAliasTypes =
                        ConcurrentDictionary<SymbolKey, Lazy<System.Threading.Tasks.Task<int>>>()

                    let canonicalAliases =
                        ConcurrentDictionary<int, Lazy<System.Threading.Tasks.Task<bool>>>()

                    // A catalogue can discover a non-exported generic alias after the seeds.
                    // Its declared form owns a finite written operand closure. Signature-parameter
                    // applications can look identical here; only the checker's exact declared ID
                    // authenticates the form, and unavailable source evidence grants no exemption.
                    let canonicalGenericAlias (ty: TypeResponse) =
                        canonicalAliases
                            .GetOrAdd(
                                ty.Id,
                                fun _ ->
                                    lazy
                                        (async {
                                            let! result =
                                                attempt (
                                                    async {
                                                        let! alias = ctx.Session.getAliasSymbolOfType ty.Id

                                                        match alias with
                                                        | ValueNone -> return false
                                                        | ValueSome alias ->
                                                            let! declarations =
                                                                alias.Declarations
                                                                |> ValueOption.defaultValue [||]
                                                                |> Array.map (fun declaration ->
                                                                    async {
                                                                        match NodeHandle.parse declaration with
                                                                        | ValueNone -> return false
                                                                        | ValueSome handle ->
                                                                            let! source = sourceFile ctx handle.Path

                                                                            return
                                                                                source
                                                                                |> ValueOption.exists (fun source ->
                                                                                    let node =
                                                                                        Node.ofIndex<AnyNode>
                                                                                            source
                                                                                            handle.Index

                                                                                    node.Kind =
                                                                                        SyntaxKind
                                                                                            .TypeAliasDeclaration
                                                                                    && (Node.retag<
                                                                                            AnyNode,
                                                                                            TypeAliasDeclaration
                                                                                         >
                                                                                            node
                                                                                        |> TypeAliasDeclaration
                                                                                            .typeParameters
                                                                                        |> Seq.isEmpty
                                                                                        |> not))
                                                                    })
                                                                |> Async.Sequential

                                                            if
                                                                Array.isEmpty declarations
                                                                || not (Array.forall id declarations)
                                                            then
                                                                return false
                                                            else
                                                                let lookup =
                                                                    declaredAliasTypes.GetOrAdd(
                                                                        alias.SymbolId,
                                                                        fun _ ->
                                                                            lazy
                                                                                (async {
                                                                                    let! declared =
                                                                                        ctx.Session
                                                                                            .getDeclaredTypeOfSymbol
                                                                                            alias.Reference

                                                                                    return declared.Id
                                                                                 }
                                                                                 |> Async.StartAsTask)
                                                                    )

                                                                let! declared = lookup.Value |> Async.AwaitTask
                                                                return declared = ty.Id
                                                    }
                                                )

                                            return result |> Result.defaultValue false
                                         }
                                         |> Async.StartAsTask)
                            )
                            .Value
                        |> Async.AwaitTask

                    let sourceContractIds (table: Map<int<typeId>, TypeFacts>) =
                        async {
                            let! handles =
                                table
                                |> Map.toList
                                |> List.chunkBySize 128
                                |> List.map (fun batch ->
                                    batch
                                    |> List.map (fun (id, facts: TypeFacts) ->
                                        async {
                                            let! handles = sourceDeclarationHandles facts.Response

                                            let! canonical =
                                                if
                                                    facts.Response.AliasTypeArguments
                                                    |> ValueOption.exists (Array.isEmpty >> not)
                                                then
                                                    canonicalGenericAlias facts.Response
                                                else
                                                    async.Return false

                                            return id, Set.ofList handles, canonical
                                        })
                                    |> Async.Parallel)
                                |> Async.Sequential

                            let declarationsOf =
                                handles
                                |> Array.concat
                                |> Array.map (fun (id, handles, _) -> id, handles)
                                |> dict

                            let canonicalDeclaredAliases =
                                handles
                                |> Array.concat
                                |> Array.choose (fun (id, _, canonical) -> if canonical then Some id else None)
                                |> Set.ofArray

                            let nominalDeclaration (facts: TypeFacts) =
                                facts.Response.ObjectFlags
                                |> ValueOption.exists (fun flags ->
                                    (flags.HasFlag ObjectFlags.Class || flags.HasFlag ObjectFlags.Interface)
                                    && not (flags.HasFlag ObjectFlags.Instantiated))
                                && facts.Response.Target |> ValueOption.forall ((=) facts.Response.Id)
                                && facts.Response.IsTupleType <> ValueSome true

                            let contracts = System.Collections.Generic.HashSet<int<typeId>>()
                            // The greatest remaining application depth each type's members were followed at.
                            let expanded = System.Collections.Generic.Dictionary<int<typeId>, int>()
                            let shallow = System.Collections.Generic.HashSet<int<typeId>>()
                            let visits = ref 0

                            let pending =
                                System.Collections.Generic.Stack<Set<string> * int * bool * int<typeId>>()

                            // An argument outside the table counts as wrapping.
                            let plainArguments = System.Collections.Generic.Dictionary<int<typeId>, bool>()

                            let rec plainArgument id =
                                match plainArguments.TryGetValue id with
                                | true, plain -> plain
                                | _ ->
                                    plainArguments[id] <- false

                                    let plain =
                                        match Map.tryFind id table with
                                        | None -> false
                                        | Some facts ->
                                            let flags = facts.Response.Flags

                                            flags.HasFlag TypeFlags.TypeParameter
                                            || uint32 (leafTypeFlags &&& flags) <> 0u
                                            || (flags.HasFlag TypeFlags.Union
                                                && List.forall plainArgument facts.UnionMembers)

                                    plainArguments[id] <- plain
                                    plain

                            // A nominal declaration restarts the ancestry and the depth. Below it, an
                            // application stops expanding on re-entering a declaration of its ancestry
                            // or past `ContractDepth` nested wrapping applications. Each type is
                            // expanded at most once per remaining depth.
                            let visit (ancestry: Set<string>) depth expandMembers id =
                                visits.Value <- visits.Value + 1
                                contracts.Add id |> ignore

                                match Map.tryFind id table with
                                | None -> ()
                                | Some facts ->
                                    let response = facts.Response

                                    let declarations =
                                        match declarationsOf.TryGetValue id with
                                        | true, declarations -> declarations
                                        | _ -> Set.empty

                                    let nominal = nominalDeclaration facts

                                    let aliasApplication =
                                        response.AliasTypeArguments |> ValueOption.exists (Array.isEmpty >> not)
                                        && not (Set.contains id canonicalSeedAliases)
                                        && not (Set.contains id canonicalDeclaredAliases)

                                    let instantiated =
                                        aliasApplication || response.Target |> ValueOption.exists ((<>) response.Id)

                                    let wrapping =
                                        (aliasApplication
                                         && not (
                                             response.AliasTypeArguments
                                             |> ValueOption.defaultValue [||]
                                             |> Array.forall (fun argument -> plainArgument (argument * uom<typeId>))
                                         ))
                                        || (response.Target |> ValueOption.exists ((<>) response.Id)
                                            && response.ObjectFlags
                                               |> ValueOption.exists (fun flags -> flags.HasFlag ObjectFlags.Reference)
                                            && response.IsTupleType <> ValueSome true
                                            && not (List.forall plainArgument facts.TypeArguments))

                                    let ancestry, depth =
                                        if nominal then
                                            declarations, ContractDepth
                                        else
                                            ancestry, depth

                                    let repeated =
                                        not nominal && not (Set.isEmpty (Set.intersect ancestry declarations))

                                    let expand =
                                        expandMembers
                                        && not repeated
                                        && (not instantiated
                                            || (not (Set.isEmpty declarations) && (not wrapping || depth > 0)))

                                    let depth = if expand && wrapping then depth - 1 else depth

                                    let first =
                                        if expand then
                                            match expanded.TryGetValue id with
                                            | true, prior when prior >= depth -> false
                                            | _ ->
                                                expanded[id] <- depth
                                                true
                                        else
                                            not (expanded.ContainsKey id) && shallow.Add id

                                    if first then
                                        let ancestry = Set.union ancestry declarations

                                        let push expandMembers id =
                                            pending.Push(ancestry, depth, expandMembers, id)

                                        for argument in facts.TypeArguments @ facts.AliasTypeArguments do
                                            push false argument
                                        // Union and intersection operands keep the ancestry and depth.
                                        for operand in facts.UnionMembers @ facts.IntersectionMembers do
                                            push expand operand

                                        if expand then
                                            // Resolve already chose this mapping edge. Retain its
                                            // target like a signature parameter, without opening
                                            // either unselected conditional branch.
                                            for branch in
                                                facts.Conditional
                                                |> Option.bind _.Branch
                                                |> Option.map snd
                                                |> Option.toList do
                                                push true branch

                                            for bound in Option.toList facts.Constraint @ Option.toList facts.Default do
                                                push true bound

                                            let mapped =
                                                response.ObjectFlags
                                                |> ValueOption.exists (fun flags -> flags.HasFlag ObjectFlags.Mapped)

                                            if not mapped then
                                                for member_ in facts.Members do
                                                    push true member_.TypeId

                                                for signature in facts.CallSignatures @ facts.ConstructSignatures do
                                                    push true signature.ReturnTypeId

                                                    for parameter in signature.Parameters do
                                                        push true parameter.TypeId

                                                    for parameter in signature.TypeParameters do
                                                        push true parameter

                            for KeyValue(id, facts) in table do
                                if
                                    nominalDeclaration facts
                                    || Map.containsKey id model.Types
                                    || Set.contains id canonicalDeclaredAliases
                                then
                                    pending.Push(Set.empty, ContractDepth, true, id)

                                    while pending.Count > 0 do
                                        let ancestry, depth, expandMembers, id = pending.Pop()
                                        visit ancestry depth expandMembers id

                            observe visits.Value table.Count
                            return contracts
                        }

                    let deferredTypes =
                        System.Collections.Generic.Dictionary<int<typeId>, TypeResponse>()

                    let boundedFrontier (sourceContracts: System.Collections.Generic.HashSet<int<typeId>>) fresh =
                        async {

                            let! admitted =
                                fresh
                                |> List.chunkBySize 128
                                |> List.map (fun batch ->
                                    batch
                                    |> List.map (fun (ty: TypeResponse) ->
                                        async {
                                            let leaf = uint32 (leafTypeFlags &&& ty.Flags) <> 0u

                                            let identityReference =
                                                ty.Target |> ValueOption.exists ((<>) ty.Id)
                                                && ty.ObjectFlags
                                                   |> ValueOption.exists (fun flags ->
                                                       flags.HasFlag ObjectFlags.Reference)
                                                && ty.IsTupleType <> ValueSome true
                                                && not (registers.Structural.Contains ty.TypeId)
                                                && not (sourceContracts.Contains ty.TypeId)

                                            if identityReference then
                                                registers.IdentityOnly.Add ty.TypeId

                                            let! canonical =
                                                if
                                                    leaf
                                                    || identityReference
                                                    || Map.containsKey ty.TypeId model.Types
                                                    || ty.Flags.HasFlag TypeFlags.Union
                                                    || sourceContracts.Contains ty.TypeId
                                                then
                                                    async.Return true
                                                else
                                                    canonicalSourceType ty

                                            return ty, canonical
                                        })
                                    |> Async.Parallel)
                                |> Async.Sequential

                            return admitted |> Array.collect id |> Array.toList |> List.partition snd
                        }

                    let rec walk table derived notFollowed findings frontier depth =
                        async {
                            // A source owner can arrive after the burst that deferred its member.
                            // Retry only newly proven contract edges, not the discarded expansion.
                            // One contract walk per generation, shared by the retry and the cutoff.
                            let stepContracts = lazy (sourceContractIds table |> Async.StartAsTask)
                            let contracts () = stepContracts.Value |> Async.AwaitTask

                            let! resumed =
                                if deferredTypes.Count = 0 || depth > FollowDepth then
                                    async.Return []
                                else
                                    async {
                                        let! contracts = contracts ()

                                        return
                                            deferredTypes.Values
                                            |> Seq.filter (fun ty -> contracts.Contains ty.TypeId)
                                            |> Seq.toList
                                    }

                            for ty in resumed do
                                deferredTypes.Remove ty.TypeId |> ignore

                            let frontier = frontier @ resumed

                            let stagedReads = Registers.commit registers

                            if resolveCountersEnabled.Value then
                                eprintfn "[resolve-counters] generation=%d staged-reads=%d" (depth - 1) stagedReads

                            // An instantiation derived as identity that a union or an
                            // intersection has since read as an operand is derived again, in
                            // full; its table entry is replaced.
                            let promoted =
                                frontier
                                |> List.filter (fun (ty: TypeResponse) ->
                                    registers.IdentityOnly.Contains ty.TypeId
                                    && registers.Structural.Contains ty.TypeId)

                            for ty in promoted do
                                registers.IdentityOnly.Remove ty.TypeId

                            let derived = promoted |> List.fold (fun set ty -> Set.remove ty.TypeId set) derived

                            let fresh =
                                frontier
                                |> List.distinctBy (fun (ty: TypeResponse) -> ty.Id)
                                |> List.filter (fun ty -> not (Set.contains ty.TypeId derived))
                                |> List.sortBy _.Id

                            if resolveCountersEnabled.Value then
                                expansions <- expansions + fresh.Length
                                maxWidth <- max maxWidth fresh.Length
                                eprintfn "[resolve-counters] depth=%d width=%d" depth fresh.Length

                            match trace with
                            | None -> ()
                            | Some trace ->
                                for ty in fresh do
                                    trace.Generations.TryAdd(ty.TypeId, depth) |> ignore

                            let! selected, deferred =
                                if fresh.Length > followWidth && depth <= FollowDepth then
                                    async {
                                        let! sourceContracts = contracts ()
                                        let! admitted, deferred = boundedFrontier sourceContracts fresh
                                        Registers.commit registers |> ignore
                                        return List.map fst admitted, List.map fst deferred
                                    }
                                else
                                    async.Return(fresh, [])

                            for ty in deferred do
                                deferredTypes[ty.TypeId] <- ty

                            let notFollowed =
                                deferred
                                |> List.fold
                                    (fun map ty ->
                                        Map.add ty.TypeId $"beyond the frontier width cutoff ({followWidth})" map)
                                    notFollowed

                            let findings =
                                if deferred.IsEmpty then
                                    findings
                                else
                                    findings
                                    @ [
                                        Finding.make
                                            "<type-table>"
                                            (ResolveTypeTable.FrontierTooWide(fresh.Length, followWidth))
                                    ]

                            match selected with
                            | [] -> return table, notFollowed, findings
                            | fresh when depth > FollowDepth ->
                                do! dumpFrontier ctx trace fresh

                                let notFollowed =
                                    fresh
                                    |> List.fold
                                        (fun map ty -> Map.add ty.TypeId $"beyond the depth cutoff ({FollowDepth})" map)
                                        notFollowed

                                // One finding for the frontier, not one per type: a type here has
                                // no name to report under, and its checker id is assigned in the
                                // order answers arrived, so a finding keyed by it differs run to
                                // run. The count is stable; the reference that reads one of these
                                // widens under its own owner's name (`typeRefOnPath`).
                                let findings =
                                    findings
                                    @ [
                                        Finding.make
                                            "<type-table>"
                                            (ResolveTypeTable.FrontierNotResolved(fresh.Length, FollowDepth))
                                    ]

                                return table, notFollowed, findings
                            | fresh ->
                                let! aliasDeclarations =
                                    if
                                        ctx.Config.DeclarationCatalog
                                        || not (List.isEmpty ctx.Config.DeclarationReferences)
                                    then
                                        async {
                                            let! symbols =
                                                fresh
                                                |> List.filter (fun ty -> ty.AliasSymbol.IsSome)
                                                |> List.map (fun ty -> ctx.Session.getAliasSymbolOfType ty.Id)
                                                |> Async.Parallel

                                            return!
                                                symbols
                                                |> Array.choose ValueOption.toOption
                                                |> Array.distinctBy _.SymbolId
                                                |> Array.sortBy (fun symbol -> symbol.Declarations, symbol.Name)
                                                |> Array.map (fun symbol ->
                                                    ctx.Session.getDeclaredTypeOfSymbol symbol.Reference)
                                                |> Async.Sequential
                                        }
                                    else
                                        async.Return [||]

                                let! results =
                                    fresh
                                    |> List.map (fun ty ->
                                        async {
                                            let! result = attempt (deriveFacts ctx trace registers ambientClasses ty)
                                            return ty, result
                                        })
                                    |> Async.Parallel

                                // A type the compiler would not describe is recorded beside the
                                // depth cutoff's: not in the table, but with its reason, so the
                                // shape tier widens a reference to it and says why rather than
                                // reporting a hole in the closure.
                                let table =
                                    results
                                    |> Array.fold
                                        (fun map (_, result) ->
                                            match result with
                                            | Ok(facts, _) -> Map.add facts.Response.TypeId facts map
                                            | Error _ -> map)
                                        table

                                let notFollowed =
                                    results
                                    |> Array.fold
                                        (fun map (ty, result) ->
                                            match result with
                                            | Error reason -> Map.add ty.TypeId reason map
                                            | Ok _ ->
                                                deferredTypes.Remove ty.TypeId |> ignore
                                                Map.remove ty.TypeId map)
                                        notFollowed

                                let findings =
                                    findings
                                    @ [
                                        for ty, result in results do
                                            match result with
                                            | Error reason ->
                                                Finding.make "<type-table>" (ResolveTypeTable.TypeNotResolved reason)
                                            | Ok _ -> ()
                                    ]

                                let derived = fresh |> List.fold (fun set ty -> Set.add ty.TypeId set) derived

                                let discovered =
                                    [
                                        for _, result in results do
                                            match result with
                                            | Ok(_, discovered) -> yield! discovered
                                            | Error _ -> ()
                                        yield! aliasDeclarations
                                    ]

                                return! walk table derived notFollowed findings discovered (depth + 1)
                        }

                    let seeds =
                        model.Types
                        |> Map.toList
                        |> List.map (fun (_, facts) -> facts.Response)
                        |> Registers.structural registers

                    let! table, notFollowed, findings =
                        walk model.Types Set.empty model.NotFollowed [] seeds 0

                    if resolveCountersEnabled.Value then
                        eprintfn
                            "[resolve-counters] frontier-expansions=%d table-size=%d max-width=%d"
                            expansions
                            (Map.count table)
                            maxWidth

                    let model =
                        { model with
                            Types = table
                            NotFollowed = notFollowed
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

let internal resolveTypeTableWithWidth followWidth =
    resolveTypeTableObserved followWidth (fun _ _ -> ())

let resolveTypeTable = resolveTypeTableWithWidth FollowWidth

/// The tier's pass list, in execution order.
let private sourceStringAliasOf (ctx: Context) typeId (property: SymbolResponse) =
    async {
        let literalUnion (node: Node<TypeNode>) =
            node.Kind = SyntaxKind.UnionType
            && (Node.retag<TypeNode, UnionTypeNode> node
                |> UnionTypeNode.types
                |> Seq.forall (fun item ->
                    item.Kind = SyntaxKind.LiteralType
                    && (Node.retag<TypeNode, LiteralTypeNode> item
                        |> LiteralTypeNode.literal
                        |> ValueOption.exists (fun literal -> literal.Kind = SyntaxKind.StringLiteral))))

        let referencedSymbol path reference =
            async {
                match TypeReferenceNode.typeName reference with
                | ValueSome name when TypeReferenceNode.typeArguments reference |> Seq.isEmpty ->
                    let! symbol =
                        ctx.Session.getSymbolAtLocation (
                            NodeHandle.format
                                {
                                    Index = Node.index name
                                    Kind = name.Kind
                                    Path = path
                                }
                        )

                    match symbol with
                    | ValueSome symbol when symbol.Flags.HasFlag SymbolFlags.Alias ->
                        let! actual = ctx.Session.getAliasedSymbol symbol.Reference
                        return ValueSome actual
                    | other -> return other
                | _ -> return ValueNone
            }

        let rec aliasChain visited (symbol: SymbolResponse) =
            async {
                if
                    Set.contains symbol.SymbolId visited
                    || not (symbol.Flags.HasFlag SymbolFlags.TypeAlias)
                then
                    return None
                else
                    let! declared = ctx.Session.getDeclaredTypeOfSymbol symbol.Reference

                    if declared.TypeId <> typeId then
                        return None
                    else
                        let! forms =
                            symbol.DeclarationHandles
                            |> ValueOption.defaultValue [||]
                            |> Array.map (fun raw ->
                                async {
                                    match NodeHandle.parse (raw / uom<_>) with
                                    | ValueSome source when source.Kind = SyntaxKind.TypeAliasDeclaration ->
                                        let! file = sourceFile ctx source.Path

                                        return
                                            file
                                            |> ValueOption.toOption
                                            |> Option.bind (fun file ->
                                                let alias = Node.ofIndex<TypeAliasDeclaration> file source.Index

                                                if TypeAliasDeclaration.typeParameters alias |> Seq.isEmpty then
                                                    TypeAliasDeclaration.``type`` alias
                                                    |> ValueOption.toOption
                                                    |> Option.map (fun body -> source.Path, body)
                                                else
                                                    None)
                                    | _ -> return None
                                })
                            |> Async.Parallel

                        match Array.choose id forms with
                        | [| path, body |] when literalUnion body -> return Some [ symbol ]
                        | [| path, body |] when body.Kind = SyntaxKind.TypeOperator ->
                            let operation = Node.retag<TypeNode, TypeOperatorNode> body

                            return
                                if
                                    TypeOperatorNode.operator operation = ValueSome SyntaxKind.KeyOfKeyword
                                    && (TypeOperatorNode.``type`` operation
                                        |> ValueOption.exists (fun operand -> operand.Kind = SyntaxKind.TypeQuery))
                                then
                                    Some [ symbol ]
                                else
                                    None
                        | [| path, body |] when
                            body.Kind = SyntaxKind.TypeReference
                            && uint32 (symbol.Flags &&& SymbolFlags.Value) = 0u
                            ->
                            let! target = referencedSymbol path (Node.retag<TypeNode, TypeReferenceNode> body)

                            match target with
                            | ValueSome target ->
                                let! remaining = aliasChain (Set.add symbol.SymbolId visited) target
                                return remaining |> Option.map (fun remaining -> symbol :: remaining)
                            | _ -> return None
                        | _ -> return None
            }

        let! aliases =
            property.DeclarationHandles
            |> ValueOption.defaultValue [||]
            |> Array.map (fun declaration ->
                async {
                    match NodeHandle.parse (declaration / uom<_>) with
                    | ValueNone -> return None
                    | ValueSome handle ->
                        let! file = sourceFile ctx handle.Path

                        match file with
                        | ValueNone -> return None
                        | ValueSome file ->
                            let node = Node.ofIndex<AnyNode> file handle.Index

                            let annotation =
                                if node.Kind = SyntaxKind.PropertySignature then
                                    Node.retag<AnyNode, PropertySignatureDeclaration> node
                                    |> PropertySignatureDeclaration.``type``
                                else
                                    ValueNone

                            match annotation with
                            | ValueSome annotation when literalUnion annotation -> return Some InlineStringLiterals
                            | ValueSome annotation when annotation.Kind = SyntaxKind.TypeReference ->
                                let reference = Node.retag<TypeNode, TypeReferenceNode> annotation
                                let! symbol = referencedSymbol handle.Path reference

                                match symbol with
                                | ValueSome symbol ->
                                    let! chain = aliasChain Set.empty symbol
                                    return chain |> Option.map NamedStringAlias
                                | _ -> return None
                            | _ -> return None
                })
            |> Async.Parallel

        return
            if aliases.Length = 0 || Array.exists Option.isNone aliases then
                None
            else
                match
                    aliases
                    |> Array.choose id
                    |> Array.distinctBy (function
                        | NamedStringAlias symbols ->
                            Some(List.map (fun (symbol: SymbolResponse) -> symbol.SymbolId) symbols)
                        | InlineStringLiterals -> None)
                with
                | [| alias |] -> Some alias
                | _ -> None
    }

let resolveDeclarationIdentities: Pass<ResolveModel> =
    {
        Name = "resolve-declaration-identities"
        Run =
            fun ctx model ->
                async {
                    if
                        not ctx.Config.DeclarationCatalog
                        && List.isEmpty ctx.Config.DeclarationReferences
                    then
                        return Advanced model
                    else
                        let bounds =
                            model.Types
                            |> Map.toList
                            |> List.collect (fun (_, facts) ->
                                if facts.Response.Flags.HasFlag TypeFlags.TypeParameter then
                                    Option.toList facts.Constraint @ Option.toList facts.Default
                                else
                                    [])
                            |> Set.ofList

                        let! types =
                            model.Types
                            |> Map.toArray
                            |> Array.map (fun (typeId, facts) ->
                                async {
                                    let! actual = ctx.Session.getSymbolOfType (typeId / uom<_>)

                                    let! symbol =
                                        match actual with
                                        | ValueSome _ -> async.Return actual
                                        | ValueNone -> ctx.Session.getAliasSymbolOfType (typeId / uom<_>)

                                    let declarations =
                                        symbol
                                        |> ValueOption.bind _.DeclarationHandles
                                        |> ValueOption.defaultValue [||]
                                        |> Array.toList

                                    let! arguments = ctx.Session.getAliasTypeArgumentsOfType (typeId / uom<_>)
                                    let! alias = ctx.Session.getAliasSymbolOfType (typeId / uom<_>)

                                    let! boundAlias =
                                        match alias with
                                        | ValueSome alias when
                                            Set.contains typeId bounds
                                            && facts.Response.Flags.HasFlag TypeFlags.Object
                                            && not declarations.IsEmpty
                                            && declarations
                                               |> List.forall (fun declaration ->
                                                   NodeHandle.parse (declaration / uom<_>)
                                                   |> ValueOption.exists (fun handle ->
                                                       handle.Kind = SyntaxKind.TypeLiteral))
                                            && alias.Flags.HasFlag SymbolFlags.TypeAlias
                                            && (alias.DeclarationHandles
                                                |> ValueOption.exists (fun handles ->
                                                    handles.Length > 0
                                                    && handles
                                                       |> Array.forall (fun declaration ->
                                                           NodeHandle.parse (declaration / uom<_>)
                                                           |> ValueOption.exists (fun handle ->
                                                               handle.Kind = SyntaxKind.TypeAliasDeclaration))))
                                            ->
                                            async {
                                                let! declared =
                                                    attempt (ctx.Session.getDeclaredTypeOfSymbol alias.Reference)

                                                return
                                                    match declared with
                                                    | Ok declared when declared.TypeId = typeId -> Some alias
                                                    | _ -> None
                                            }
                                        | _ -> async.Return None

                                    return
                                        typeId,
                                        { facts with
                                            SymbolName =
                                                boundAlias |> Option.map _.SymbolName |> Option.orElse facts.SymbolName
                                            SymbolParent =
                                                boundAlias
                                                |> Option.bind (fun symbol ->
                                                    symbol.ParentSymbolId |> ValueOption.toOption)
                                                |> Option.orElse facts.SymbolParent
                                            Declarations = declarations
                                            DeclarationArguments =
                                                arguments |> ValueOption.defaultValue [||] |> Array.toList
                                            AliasDeclarations =
                                                alias
                                                |> ValueOption.bind _.DeclarationHandles
                                                |> ValueOption.defaultValue [||]
                                                |> Array.toList
                                        }
                                })
                            |> Async.Parallel

                        let types = Map.ofArray types

                        let closedStrings id =
                            match Map.tryFind id types with
                            | Some facts when
                                facts.Response.Flags.HasFlag TypeFlags.Union && facts.UnionMembers.Length >= 2
                                ->
                                facts.UnionMembers
                                |> List.forall (fun id ->
                                    Map.tryFind id types
                                    |> Option.exists (fun item -> item.Response.Flags.HasFlag TypeFlags.StringLiteral))
                            | _ -> false

                        let! annotations =
                            types
                            |> Map.toArray
                            |> Array.collect (snd >> _.Members >> List.toArray)
                            |> Array.filter (fun member_ -> closedStrings member_.TypeId)
                            |> Array.distinctBy (fun member_ -> member_.Symbol.SymbolId, member_.TypeId)
                            |> Array.map (fun member_ ->
                                async {
                                    let! alias = sourceStringAliasOf ctx member_.TypeId member_.Symbol
                                    return (member_.Symbol.SymbolId, member_.TypeId), alias
                                })
                            |> Async.Parallel

                        let annotations = Map.ofArray annotations

                        let types =
                            types
                            |> Map.map (fun _ facts ->
                                let members =
                                    facts.Members
                                    |> List.map (fun member_ ->
                                        { member_ with
                                            SourceStringAnnotation =
                                                Map.tryFind (member_.Symbol.SymbolId, member_.TypeId) annotations
                                                |> Option.flatten
                                        })

                                { facts with Members = members })

                        return Advanced { model with Types = types }
                }
    }

let private compilerNoInferDeclaration (ctx: Context) (symbol: SymbolResponse) =
    async {
        if symbol.Name <> "NoInfer" then
            return false
        else
            match symbol.Declarations |> ValueOption.defaultValue [||] |> Array.distinct with
            | [| declaration |] ->
                match NodeHandle.parse declaration with
                | ValueNone -> return false
                | ValueSome handle ->
                    let! metadata =
                        ctx.Session.getSourceFileMetadata (DocumentIdentifier.FileName handle.Path)

                    if not (metadata |> ValueOption.exists _.IsDefaultLibrary) then
                        return false
                    else
                        let! source = sourceFile ctx handle.Path

                        return
                            source
                            |> ValueOption.exists (fun source ->
                                SourceFile.statements (Node.root source)
                                |> Seq.exists (fun node ->
                                    if
                                        Node.index node <> handle.Index
                                        || node.Kind <> SyntaxKind.TypeAliasDeclaration
                                    then
                                        false
                                    else
                                        let alias = Node.retag<Statement, TypeAliasDeclaration> node

                                        (TypeAliasDeclaration.name alias |> ValueOption.bind Identifier.text) =
                                            ValueSome "NoInfer"
                                        && (TypeAliasDeclaration.typeParameters alias |> Seq.length) = 1
                                        && (TypeAliasDeclaration.``type`` alias
                                            |> ValueOption.exists (fun body ->
                                                body.Kind = SyntaxKind.IntrinsicKeyword))))
            | _ -> return false
    }

/// Retains alias binders on the exported declaration rather than its possibly shared body type.
let private resolveAliasParameters: Pass<ResolveModel> =
    {
        Name = "resolve-alias-parameters"
        Run =
            fun ctx model ->
                async {
                    let mutable exports = model.ExportTypes
                    let mutable types = model.Types
                    let mutable findings = []
                    let mutable compilerNoInferSymbols = model.CompilerNoInferSymbols

                    for export in model.Harvest.Exports do
                        if export.Symbol.Flags.HasFlag SymbolFlags.TypeAlias then
                            let! parameters =
                                attempt (
                                    async {
                                        let! candidates =
                                            export.Symbol.Declarations
                                            |> ValueOption.defaultValue [||]
                                            |> Array.map (fun declaration ->
                                                async {
                                                    match NodeHandle.parse declaration with
                                                    | ValueNone -> return Some declaration
                                                    | ValueSome handle ->
                                                        let! source = sourceFile ctx handle.Path

                                                        match source with
                                                        | ValueNone -> return Some declaration
                                                        | ValueSome source ->
                                                            let node = Node.ofIndex<AnyNode> source handle.Index

                                                            return
                                                                if node.Kind = SyntaxKind.TypeAliasDeclaration then
                                                                    Some declaration
                                                                else
                                                                    None
                                                })
                                            |> Async.Sequential

                                        let handles = candidates |> Array.choose id |> Array.distinct

                                        if handles.Length <> 1 then
                                            return Error "Expected one type alias declaration"
                                        else
                                            match NodeHandle.parse handles[0] with
                                            | ValueNone -> return Error "Malformed type alias declaration handle"
                                            | ValueSome handle ->
                                                let! source = sourceFile ctx handle.Path

                                                match source with
                                                | ValueNone -> return Error "Type alias source is unavailable"
                                                | ValueSome source ->
                                                    let node = Node.ofIndex<AnyNode> source handle.Index

                                                    if node.Kind <> SyntaxKind.TypeAliasDeclaration then
                                                        return Error "Expected a type alias declaration node"
                                                    else
                                                        let parameters =
                                                            Node.retag<AnyNode, TypeAliasDeclaration> node
                                                            |> TypeAliasDeclaration.typeParameters
                                                            |> Seq.toList

                                                        let! responses =
                                                            parameters
                                                            |> List.map (fun parameter ->
                                                                ctx.Session.getTypeAtLocation (
                                                                    NodeHandle.format
                                                                        {
                                                                            Index = Node.index parameter
                                                                            Kind = parameter.Kind
                                                                            Path = handle.Path
                                                                        }
                                                                ))
                                                            |> Async.Sequential

                                                        return Ok(Array.toList responses)
                                    }
                                )

                            match Result.bind id parameters with
                            | Error reason ->
                                findings <-
                                    findings
                                    @ [
                                        Finding.make
                                            export.ExportName
                                            (ResolveExportTypes.FacetNotResolved("alias parameters", reason))
                                    ]
                            | Ok parameters ->
                                if not (Set.contains export.Symbol.SymbolId compilerNoInferSymbols) then
                                    let! authenticated = attempt (compilerNoInferDeclaration ctx export.Symbol)

                                    if authenticated = Ok true then
                                        compilerNoInferSymbols <- Set.add export.Symbol.SymbolId compilerNoInferSymbols

                                exports <-
                                    exports
                                    |> Map.change
                                        export.Symbol.SymbolId
                                        (Option.map (fun ids ->
                                            { ids with
                                                AliasParameters =
                                                    Some(
                                                        parameters
                                                        |> List.map (fun (parameter: TypeResponse) -> parameter.TypeId)
                                                    )
                                            }))

                                for parameter in parameters do
                                    if not (Map.containsKey parameter.TypeId types) then
                                        types <- Map.add parameter.TypeId (TypeFacts.shallow parameter) types

                    let model =
                        { model with
                            ExportTypes = exports
                            Types = types
                            CompilerNoInferSymbols = compilerNoInferSymbols
                        }

                    return
                        if findings.IsEmpty then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

/// Records the element labels of every tuple a rest parameter is typed by, directly or as a union
/// arm, so its spread parameters are named as written. It reads after the walk, over a closed
/// type table, and adds no type to it.
let private resolveRestTupleLabels: Pass<ResolveModel> =
    {
        Name = "resolve-rest-tuple-labels"
        Run =
            fun ctx model ->
                async {
                    let isTuple id =
                        Map.tryFind id model.Types
                        |> Option.exists (fun facts -> facts.Response.IsTupleType = ValueSome true)

                    let tuples =
                        model.Types
                        |> Map.toList
                        |> List.collect (fun (_, facts) -> facts.CallSignatures @ facts.ConstructSignatures)
                        |> List.filter _.HasRest
                        |> List.choose (fun signature -> List.tryLast signature.Parameters)
                        |> List.collect (fun rest ->
                            match Map.tryFind rest.TypeId model.Types with
                            | Some facts when facts.Response.Flags.HasFlag TypeFlags.Union -> facts.UnionMembers
                            | _ -> [ rest.TypeId ])
                        |> List.filter isTuple
                        |> List.distinct

                    let mutable types = model.Types

                    for id in tuples do
                        let! target = attempt (ctx.Session.getTargetOfType model.Types[id].Response.Id)

                        match target with
                        | Ok target ->
                            let! labels =
                                target.LabeledElementDeclarations
                                |> ValueOption.defaultValue [||]
                                |> Array.map (fun handle ->
                                    if isNull handle then
                                        async.Return None
                                    else
                                        declaredIdentifier ctx handle)
                                |> Async.Sequential

                            if labels |> Array.exists Option.isSome then
                                types <-
                                    types
                                    |> Map.change
                                        id
                                        (Option.map (fun facts ->
                                            { facts with
                                                TupleLabels = Array.toList labels
                                            }))
                        | Error _ -> ()

                    return Advanced { model with Types = types }
                }
    }

let passes: Pass<ResolveModel> list =
    [
        resolveExportTypes
        resolveAliasParameters
        resolveTypeTable
        resolveDeclarationIdentities
        resolveRestTupleLabels
    ]

let internal customizationFacts (ctx: Context) (facts: TypeFacts) =
    async {
        let! properties, privateMembers = publicPropertiesOfType ctx facts.Response

        let! members = properties |> Array.map (resolveMember ctx true) |> Async.Parallel

        let! bases = ctx.Session.getBaseTypes facts.Response.Id

        return
            { facts with
                Members = members |> Array.map fst |> Array.toList
                PrivateMembers = privateMembers
                BaseTypes = bases |> ValueOption.defaultValue [||] |> Array.map _.TypeId |> Array.toList
            },
            (members |> Array.map snd |> Array.toList)
            @ (bases |> ValueOption.defaultValue [||] |> Array.toList)
    }
