/// Stable declaration ownership shared by independently generated entry modules.
module Xantham.Generator.DeclarationCatalog

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.Generator.Measure

[<CLIMutable>]
type Source =
    {
        Package: string
        Version: string
        File: string
        Sha256: string
        ManifestSha256: string
    }

[<CLIMutable>]
type Declaration =
    {
        Identity: string
        Handles: string array
        Sources: Source array
        Role: string
        FSharpName: string
        Arity: int
        Constraints: string array
        Api: string
        Owner: string
    }

[<CLIMutable>]
type Owner =
    {
        Name: string
        Dependencies: string array
    }

[<CLIMutable>]
type Catalog =
    {
        SchemaVersion: int
        Compiler: string
        Generator: string
        InferenceProfile: string
        Owner: string
        Inputs: Source array
        Owners: Owner array
        Declarations: Declaration array
    }

let private options =
    JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, WriteIndented = true)

let private compact =
    JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

let private json value =
    JsonSerializer.Serialize(value, compact)

let private hash (bytes: byte array) =
    SHA256.HashData bytes |> Convert.ToHexStringLower

let private hashText (value: string) = Encoding.UTF8.GetBytes value |> hash
let private slash (path: string) = path.Replace('\\', '/')

let private fail message =
    failwith $"declaration catalog: {message}"

let private sourceKey (source: Source) =
    $"{source.Package}@{source.Version}/{source.File}"

let private profile (config: GeneratorConfig) =
    json
        {|
            inferenceVersion = 1
            lib = config.Lib |> Option.map List.sort
            types = config.Types |> Option.map List.sort
            groups =
                config.Groups
                |> Map.toArray
                |> Array.map (fun (key, disposition) -> key, sprintf "%A" disposition)
            resolveNoInfer = config.ResolveNoInfer
        |}
    |> hashText

let private compiler (ctx: Context) =
    match Tsc.locate (ctx.PackageDir / uom<dirPath>) with
    | Some path -> File.ReadAllBytes path |> hash
    | None -> fail "the compiler executable could not be identified"

let private packageOf (ctx: Context) (file: string) =
    let rec find directory =
        let manifest = Path.Combine(directory, "package.json")

        let parent () =
            match Directory.GetParent directory with
            | null -> fail $"{file} has no containing package manifest"
            | parent -> find parent.FullName

        if File.Exists manifest then
            use doc = JsonDocument.Parse(File.ReadAllText manifest)

            let field name =
                match doc.RootElement.TryGetProperty(name: string) with
                | true, value when value.ValueKind = JsonValueKind.String -> value.GetString()
                | _ -> fail $"{manifest} must declare its {name} for stable identity"

            match doc.RootElement.TryGetProperty "name" with
            | true, _ -> directory, field "name", field "version"
            | _ -> parent ()
        else
            parent ()

    if file.StartsWith "bundled:" then
        ctx.PackageDir / uom<dirPath>, "typescript/lib", "bundled"
    else
        find (Path.GetDirectoryName file)

let private sources (ctx: Context) (handles: string<Measure.declHandle> list) =
    async {
        let paths =
            handles
            |> List.map (fun handle ->
                match (handle / uom<declHandle>).Split([| '.' |], 3) with
                | [| index; kind; file |] when not (String.IsNullOrWhiteSpace index || String.IsNullOrWhiteSpace kind) ->
                    file
                | _ -> fail $"invalid declaration handle {handle}")
            |> List.distinct
            |> List.sort

        let! sourceFiles =
            paths
            |> List.map (fun file ->
                async {
                    let root, package, version = packageOf ctx file

                    let! bytes =
                        if File.Exists file then
                            async.Return(File.ReadAllBytes file)
                        else
                            async {
                                let! source = ctx.Session.getSourceFile (Proto.DocumentIdentifier.FileName file)

                                match source with
                                | ValueSome source -> return Encoding.UTF8.GetBytes(Ast.sourceText source)
                                | ValueNone -> return fail $"source content is unavailable for {file}"
                            }

                    return
                        file,
                        {
                            Package = package
                            Version = version
                            File =
                                if file.StartsWith "bundled:" then
                                    file
                                else
                                    slash (Path.GetRelativePath(root, file))
                            Sha256 = hash bytes
                            ManifestSha256 =
                                if file.StartsWith "bundled:" then
                                    "bundled"
                                else
                                    Path.Combine(root, "package.json") |> File.ReadAllBytes |> hash
                        }
                })
            |> Async.Parallel

        return Map.ofArray sourceFiles
    }

let private normalizeHandle (sources: Map<string, Source>) (handle: string<Measure.declHandle>) =
    match (handle / uom<declHandle>).Split([| '.' |], 3) with
    | [| index; kind; file |] -> $"{sourceKey sources[file]}#{index}.{kind}"
    | _ -> fail $"invalid declaration handle {handle}"

let private parameters =
    function
    | FsInterface decl -> decl.TypeParameters
    | FsAbbrev decl -> decl.TypeParameters
    | FsDelegateType decl -> decl.TypeParameters
    | FsPhantom decl -> decl.TypeParameters
    | _ -> []

let private order =
    function
    | FsInterface decl -> decl.Order
    | FsAbbrev decl -> decl.Order
    | FsDelegateType decl -> decl.Order
    | FsPhantom decl -> decl.Order
    | FsMeasure decl -> decl.Order
    | FsStringEnum decl -> decl.Order
    | FsTaggedUnion decl -> decl.Order
    | FsEnum decl -> decl.Order
    | FsExports _ -> None

type private Identity =
    {
        Key: string
        Handles: string list
        Sources: Source list
        Role: string
    }

let private identities (ctx: Context) (shape: ShapeModel) (sourceFiles: Map<string, Source>) =
    let identity role rawHandles arguments =
        let handles =
            rawHandles
            |> List.map (normalizeHandle sourceFiles)
            |> List.distinct
            |> List.sort

        let sources =
            rawHandles
            |> List.map (fun handle -> sourceFiles[(handle / uom<declHandle>).Split([| '.' |], 3)[2]])
            |> List.distinct
            |> List.sortBy sourceKey

        {
            Key = json (role, handles, arguments) |> hashText
            Handles = handles
            Sources = sources
            Role = role
        }

    let exportHandles =
        shape.Harvest.Exports
        |> List.collect (fun export ->
            match Map.tryFind export.Symbol.SymbolId shape.ExportTypes with
            | Some ids ->
                let handles =
                    export.Symbol.DeclarationHandles
                    |> ValueOption.defaultValue [||]
                    |> Array.toList

                [ yield! ids.Declared |> Option.toList; yield! ids.Value |> Option.toList ]
                |> List.map (fun id -> id, handles)
            | None -> [])
        |> List.groupBy fst
        |> List.map (fun (id, entries) -> id, entries |> List.collect snd |> List.distinct)
        |> Map.ofList

    let literalUnionIdentity includeNullish (facts: TypeFacts) =
        let literalKey id =
            match Map.tryFind id shape.Types with
            | Some member_ ->
                let flags = member_.Response.Flags

                if
                    not (flags.HasFlag TypeFlags.EnumLiteral)
                    && (flags.HasFlag TypeFlags.StringLiteral
                        || flags.HasFlag TypeFlags.NumberLiteral
                        || flags.HasFlag TypeFlags.BigIntLiteral
                        || flags.HasFlag TypeFlags.BooleanLiteral
                        || flags.HasFlag TypeFlags.Null
                        || flags.HasFlag TypeFlags.Undefined)
                then
                    Some(json (uint32 flags, member_.Response.Value))
                else
                    None
            | None -> None

        let memberIds =
            facts.UnionMembers
            |> List.filter (fun id ->
                includeNullish
                || not (Map.tryFind id shape.Types |> Option.exists Shape.Spec.isNullish))

        let members = memberIds |> List.choose literalKey

        if
            List.isEmpty facts.AliasDeclarations
            && not (List.isEmpty members)
            && members.Length = memberIds.Length
        then
            // Literal unions are checker-interned across unrelated properties. Their values,
            // not whichever parent happens to be reachable first, define anonymous identity.
            Some(identity "literal-union" [] (List.sort members))
        else
            None

    let rec typeIdentity visited bindings id =
        if List.contains id visited then
            None
        else
            match Map.tryFind id shape.Types with
            | None -> None
            | Some facts ->
                let flags = facts.Response.ObjectFlags |> ValueOption.defaultValue ObjectFlags.None

                let structural =
                    not (flags.HasFlag ObjectFlags.Reference)
                    && (flags.HasFlag ObjectFlags.Anonymous || flags.HasFlag ObjectFlags.Mapped)

                let handles =
                    if List.isEmpty facts.Declarations then
                        Map.tryFind id exportHandles |> Option.defaultValue []
                    else
                        facts.Declarations

                if List.isEmpty handles then
                    literalUnionIdentity true facts
                else
                    let role =
                        if List.isEmpty facts.ConstructSignatures then
                            "type"
                        else
                            "constructor"

                    let bindings =
                        (Shape.Spec.declParamIds facts @ Shape.Spec.freeParamsOf shape id)
                        |> List.distinct
                        |> List.mapi (fun index parameter -> parameter, "parameter:" + string index)
                        |> List.fold (fun bindings (parameter, key) -> Map.add parameter key bindings) bindings

                    let rec argumentKeyWith visited bindings argument =
                        match Map.tryFind argument bindings with
                        | Some key -> key
                        | None ->
                            // Recursive references use their distance along the current type path.
                            match List.tryFindIndex ((=) argument) visited with
                            | Some depth -> json ("recursive", depth)
                            | None ->
                                match typeIdentity visited bindings argument with
                                | Some identity -> identity.Key
                                | None ->
                                    let keys kind children =
                                        let parts =
                                            children |> List.map (argumentKeyWith (argument :: visited) bindings)

                                        if List.contains "" parts then
                                            ""
                                        else
                                            json (kind, if kind = "union" then List.sort parts else parts)

                                    match Map.tryFind argument shape.Types with
                                    | Some argument when argument.Response.Flags.HasFlag TypeFlags.TypeParameter ->
                                        "parameter:"
                                        + Option.defaultValue
                                            ""
                                            (argument.SymbolName |> Option.map (fun x -> x / uom<symbolName>))
                                    | Some argument when
                                        uint32 (
                                            argument.Response.Flags
                                            &&& (TypeFlags.StringLike
                                                 ||| TypeFlags.NumberLike
                                                 ||| TypeFlags.BooleanLike
                                                 ||| TypeFlags.BigIntLike
                                                 ||| TypeFlags.ESSymbolLike
                                                 ||| TypeFlags.Any
                                                 ||| TypeFlags.Unknown
                                                 ||| TypeFlags.Null
                                                 ||| TypeFlags.Undefined
                                                 ||| TypeFlags.Void
                                                 ||| TypeFlags.Never
                                                 ||| TypeFlags.NonPrimitive)
                                        )
                                        <> 0u
                                        ->
                                        json (uint32 argument.Response.Flags, argument.Response.Value)
                                    | Some facts when not (List.isEmpty facts.UnionMembers) ->
                                        keys "union" facts.UnionMembers
                                    | Some facts when not (List.isEmpty facts.IntersectionMembers) ->
                                        keys "intersection" facts.IntersectionMembers
                                    | Some facts when facts.Response.IsTupleType = ValueSome true ->
                                        let parts =
                                            facts.TypeArguments
                                            |> List.map (argumentKeyWith (argument :: visited) bindings)

                                        if List.contains "" parts || parts.Length <> facts.TupleElements.Length then
                                            ""
                                        else
                                            json (
                                                "tuple",
                                                List.zip (facts.TupleElements |> List.map uint32) parts,
                                                facts.Response.Readonly |> ValueOption.toOption
                                            )
                                    | Some _ -> ""
                                    | None -> ""

                    let argumentKey = argumentKeyWith (id :: visited) bindings

                    let arguments =
                        match facts.Response.TargetTypeId with
                        | ValueSome target when target <> id -> facts.TypeArguments |> List.map argumentKey
                        | _ -> []

                    let mutable complete = true

                    let partKey id =
                        let key = argumentKey id

                        if key = "" then
                            complete <- false

                        key

                    let signature (signature: ResolvedSignature) =
                        let signatureBindings =
                            signature.TypeParameters
                            |> List.mapi (fun index parameter -> parameter, "method-parameter:" + string index)
                            |> List.fold (fun bindings (parameter, key) -> Map.add parameter key bindings) bindings

                        let signatureKey id =
                            let key = argumentKeyWith (facts.Response.TypeId :: visited) signatureBindings id

                            if key = "" then
                                complete <- false

                            key

                        let bounds parameter =
                            match Map.tryFind parameter shape.Types with
                            | Some facts ->
                                Option.map signatureKey facts.Constraint, Option.map signatureKey facts.Default
                            | None ->
                                complete <- false
                                None, None

                        json (
                            signature.TypeParameters |> List.map bounds,
                            signature.Parameters
                            |> List.map (fun parameter -> parameter.Optional, signatureKey parameter.TypeId),
                            signature.HasRest,
                            signatureKey signature.ReturnTypeId
                        )

                    let arguments =
                        if structural then
                            [
                                yield!
                                    facts.Members
                                    |> List.map (fun member_ ->
                                        json (
                                            member_.Symbol.Name,
                                            member_.Optional,
                                            member_.ReadOnly,
                                            partKey member_.TypeId
                                        ))
                                yield!
                                    facts.IndexInfos
                                    |> List.map (fun index ->
                                        json (partKey index.KeyTypeId, partKey index.ValueTypeId, index.IsReadonly))
                                yield! facts.CallSignatures |> List.map signature
                                yield! facts.ConstructSignatures |> List.map signature
                            ]
                        else
                            arguments

                    let arguments =
                        if structural && not (List.isEmpty arguments) then
                            // Populated structural keys already describe the applied members,
                            // including transparent aliases that carry different alias arguments.
                            arguments
                        else
                            let declarationArguments =
                                facts.DeclarationArguments
                                |> List.map (fun argument ->
                                    if Map.containsKey argument.TypeId shape.Types then
                                        partKey argument.TypeId
                                    elif argument.Flags.HasFlag TypeFlags.Object then
                                        ""
                                    else
                                        json (uint32 argument.Flags, argument.Value))

                            if not structural then
                                arguments @ declarationArguments
                            elif List.isEmpty declarationArguments then
                                arguments
                            else
                                // Opaque dependencies retain no members. Their alias arguments
                                // still distinguish defaults and repeated slots, but belong to
                                // the alias declaration, which can reorder the underlying type's
                                // parameters. Retain that normalized argument owner as well.
                                let owner =
                                    facts.AliasDeclarations
                                    |> List.map (normalizeHandle sourceFiles)
                                    |> List.distinct
                                    |> List.sort

                                if List.contains "" declarationArguments then
                                    complete <- false

                                arguments @ [ json ("declaration-arguments", owner, declarationArguments) ]

                    if not complete || List.contains "" arguments then
                        let aliasArguments =
                            facts.DeclarationArguments
                            |> List.map (fun argument ->
                                if Map.containsKey argument.TypeId shape.Types then
                                    argumentKey argument.TypeId
                                elif argument.Flags.HasFlag TypeFlags.Object then
                                    ""
                                else
                                    json (uint32 argument.Flags, argument.Value))

                        if List.isEmpty facts.AliasDeclarations || List.contains "" aliasArguments then
                            if role = "constructor" && not (List.contains "" aliasArguments) then
                                Some(identity role handles aliasArguments)
                            else
                                None
                        else
                            Some(identity "alias" facts.AliasDeclarations aliasArguments)
                    else
                        Some(identity role handles arguments)

    let mutable byType =
        shape.Types
        |> Map.toList
        |> List.choose (fun (id, _) -> typeIdentity [] Map.empty id |> Option.map (fun identity -> id, identity))
        |> Map.ofList

    let dependencies (facts: TypeFacts) =
        [
            yield! facts.Members |> List.map _.TypeId
            for index in facts.IndexInfos do
                yield index.KeyTypeId
                yield index.ValueTypeId
            for signature in facts.CallSignatures @ facts.ConstructSignatures do
                yield signature.ReturnTypeId
                yield! signature.Parameters |> List.map _.TypeId
                yield! signature.TypeParameters
            yield! facts.BaseTypes
            yield! facts.TypeArguments
            yield! facts.AliasTypeArguments
            yield! facts.DeclarationArguments |> List.map _.TypeId
            yield! facts.UnionMembers
            yield! Option.toList facts.NonNullableAlias
            yield! facts.IntersectionMembers
            yield! Option.toList facts.Constraint
            yield! Option.toList facts.Default
            yield! facts.Response.TargetTypeId |> ValueOption.toList
            yield! facts.Conditional |> Option.bind _.Branch |> Option.map snd |> Option.toList
        ]

    let closure id =
        let bound =
            Shape.Spec.declParamIds shape.Types[id] @ Shape.Spec.freeParamsOf shape id
            |> Set.ofList

        let visited = Collections.Generic.HashSet<int<Measure.typeId>>()
        let files = Collections.Generic.HashSet<string>()
        let pending = Collections.Generic.Stack<int<Measure.typeId>>()
        pending.Push id

        while pending.Count > 0 do
            let current = pending.Pop()

            if visited.Add current && not (Set.contains current bound) then
                match Map.tryFind current shape.Types with
                | None -> ()
                | Some facts ->
                    if not (facts.Response.Flags.HasFlag TypeFlags.TypeParameter) then
                        for handle in facts.Declarations @ facts.AliasDeclarations do
                            files.Add((handle / uom<declHandle>).Split([| '.' |], 3)[2]) |> ignore

                    for dependency in dependencies facts do
                        pending.Push dependency

        files
        |> Seq.map (fun file -> sourceFiles[file])
        |> Seq.distinct
        |> Seq.sortBy sourceKey
        |> Seq.toList

    // Parent roles cover checker-synthesized types whose symbol has no declaration handle.
    let edges (facts: TypeFacts) =
        [
            for member_ in facts.Members do
                yield member_.TypeId, "member:" + member_.Symbol.Name
            for index, signature in List.indexed facts.CallSignatures do
                yield signature.ReturnTypeId, $"call:{index}:return"

                for parameter in signature.Parameters do
                    yield parameter.TypeId, $"call:{index}:parameter:{parameter.Symbol.Name}"
            for index, signature in List.indexed facts.ConstructSignatures do
                yield signature.ReturnTypeId, $"construct:{index}:return"

                for parameter in signature.Parameters do
                    yield parameter.TypeId, $"construct:{index}:parameter:{parameter.Symbol.Name}"
            for index, id in List.indexed facts.UnionMembers do
                yield id, $"union:{index}"
            for index, id in List.indexed facts.IntersectionMembers do
                yield id, $"intersection:{index}"
            for index, id in List.indexed facts.TypeArguments do
                yield id, $"argument:{index}"
        ]

    let mutable changed = true

    while changed do
        let additions =
            byType
            |> Map.toList
            |> List.collect (fun (id, parent) ->
                edges shape.Types[id]
                |> List.choose (fun (child, role) ->
                    if Map.containsKey child byType || not (Map.containsKey child shape.Types) then
                        None
                    else
                        Some(
                            child,
                            { parent with
                                Key = hashText (parent.Key + "/" + role)
                                Role = role
                            }
                        )))
            |> List.groupBy fst
            |> List.map (fun (id, candidates) -> id, candidates |> List.map snd |> List.minBy _.Key)

        changed <- not (List.isEmpty additions)
        byType <- List.fold (fun map (id, identity) -> Map.add id identity map) byType additions

    let stringEnums =
        shape.Decls
        |> List.choose (function
            | FsStringEnum enum -> Some enum.Name
            | _ -> None)
        |> Set.ofList

    let byName =
        shape.DeclNames
        |> Map.toList
        |> List.filter (fun (id, _) -> not (Map.containsKey id shape.AliasApplications))
        |> List.choose (fun (id, name) ->
            Map.tryFind id byType
            |> Option.map (fun identity ->
                let identity =
                    if identity.Role = "literal-union" && Set.contains name stringEnums then
                        // Shape hoists nullish members into FsOption and shares the remaining enum.
                        literalUnionIdentity false shape.Types[id] |> Option.defaultValue identity
                    else
                        identity

                name,
                { identity with
                    Sources = identity.Sources @ closure id |> List.distinct |> List.sortBy sourceKey
                }))
        |> List.groupBy fst
        |> List.map (fun (name, entries) ->
            let identities = entries |> List.map snd |> List.distinctBy _.Key

            match identities with
            | [ identity ] -> name, identity
            | _ -> fail $"{name} has conflicting declaration identities")
        |> Map.ofList

    let rec forDecl name decl =
        match Map.tryFind name byName with
        | Some identity -> identity
        | None ->
            let exported =
                shape.Harvest.Exports
                |> List.tryFind (fun export ->
                    let exportedName = Shape.Spec.fsName (Shape.Spec.defaultExportName ctx) export

                    export.Order.IsSome
                    && export.Order = order decl
                    && (exportedName = name || Naming.pascalSegment exportedName = name))

            match exported with
            | Some export ->
                identity
                    "alias"
                    (export.Symbol.DeclarationHandles
                     |> ValueOption.defaultValue [||]
                     |> Array.toList)
                    []
            | None ->
                match name.LastIndexOf '.' with
                | -1 -> fail $"{name} has no stable declaration or parent role"
                | at ->
                    let parent = name.Substring(0, at)
                    let role = "generated:" + name.Substring(at + 1)
                    let origin = forDecl parent decl

                    { origin with
                        Key = hashText (origin.Key + "/" + role)
                        Role = role
                    }

    shape.Decls
    |> List.map (fun decl -> Render.declName decl |> (fun name -> name, forDecl name decl))
    |> Map.ofList

let private load profile compiler generator (path: string) =
    if not (File.Exists path) then
        fail $"reference does not exist: {path}"

    let catalog = JsonSerializer.Deserialize<Catalog>(File.ReadAllText path, options)

    if isNull (box catalog) || catalog.SchemaVersion <> 1 then
        fail $"{path} has an unsupported schema"

    if catalog.Compiler <> compiler then
        fail $"{path} uses a different compiler"

    if catalog.Generator <> generator then
        fail $"{path} uses a different generator"

    if catalog.InferenceProfile <> profile then
        fail $"{path} uses a different inference profile"

    if
        isNull catalog.Declarations
        || isNull catalog.Owners
        || isNull catalog.Inputs
        || String.IsNullOrWhiteSpace catalog.Owner
    then
        fail $"{path} is incomplete"

    for declaration in catalog.Declarations do
        if
            String.IsNullOrWhiteSpace declaration.Identity
            || String.IsNullOrWhiteSpace declaration.FSharpName
            || String.IsNullOrWhiteSpace declaration.Owner
            || isNull declaration.Handles
            || isNull declaration.Sources
            || isNull declaration.Constraints
            || String.IsNullOrWhiteSpace declaration.Api
            || declaration.Arity < 0
            || declaration.Constraints.Length <> declaration.Arity
        then
            fail $"{path} has an invalid declaration"

    catalog

let private ownerOrder (owners: Owner list) =
    let owners =
        owners
        |> List.groupBy _.Name
        |> List.map (fun (name, entries) ->
            let dependencies =
                entries
                |> List.map (fun entry -> Array.sort entry.Dependencies)
                |> List.distinct

            match dependencies with
            | [ dependencies ] -> name, dependencies
            | _ -> fail $"owner {name} has conflicting dependency lists")
        |> Map.ofList

    let rec visit visiting visited ordered name =
        if Set.contains name visiting then
            fail $"owner dependency cycle at {name}"
        elif Set.contains name visited then
            visited, ordered
        else
            let dependencies =
                Map.tryFind name owners
                |> Option.defaultWith (fun () -> fail $"missing owner {name}")

            let visited, ordered =
                dependencies
                |> Array.fold
                    (fun (visited, ordered) dependency -> visit (Set.add name visiting) visited ordered dependency)
                    (visited, ordered)

            Set.add name visited,
            ordered
            @ [
                {
                    Name = name
                    Dependencies = dependencies
                }
            ]

    owners
    |> Map.toList
    |> List.fold (fun (visited, ordered) (name, _) -> visit Set.empty visited ordered name) (Set.empty, [])
    |> snd

let private classValues (ctx: Context) (shape: ShapeModel) (groups: Render.GroupModule list) =
    let mutable shape = shape
    let mutable groups = groups
    let mutable values = Map.empty

    let claimed =
        Collections.Generic.HashSet<string>(shape.Decls |> List.map Render.declName)

    for declaration in shape.Decls do
        match declaration with
        | FsInterface class_ when not class_.Statics.IsEmpty ->
            let export =
                shape.Harvest.Exports
                |> List.tryFind (fun export ->
                    Shape.Spec.fsName (Shape.Spec.defaultExportName ctx) export = class_.Name)

            match
                export
                |> Option.bind (fun export ->
                    Map.tryFind export.Symbol.SymbolId shape.ExportTypes
                    |> Option.bind (fun types -> types.Value |> Option.map (fun id -> export, id)))
            with
            | Some(export, valueId) ->
                let helper =
                    match Map.tryFind valueId shape.DeclNames with
                    | Some name -> name
                    | None ->
                        let basis = class_.Name + "Constructor"
                        let mutable name = basis
                        let mutable suffix = 2

                        while not (claimed.Add name) do
                            name <- basis + string suffix
                            suffix <- suffix + 1

                        let constructors =
                            shape.Decls
                            |> List.collect (function
                                | FsExports container -> container.Members |> List.map _.Member
                                | _ -> [])
                            |> List.choose (fun member_ ->
                                match member_.Body with
                                | ExportConstructor(parameters, returns) when member_.Name = class_.Name ->
                                    Some(
                                        FsConstructor
                                            {
                                                Docs = member_.Docs
                                                Tags = member_.Tags
                                                TypeParameters = member_.TypeParameters
                                                Parameters = parameters
                                                Return = returns
                                            }
                                    )
                                | _ -> None)

                        let members =
                            class_.Statics
                            |> List.map (fun member_ ->
                                match member_.Body with
                                | ExportFunction(parameters, returns) ->
                                    FsMethod
                                        {
                                            Name = member_.Name
                                            Docs = member_.Docs
                                            Tags = member_.Tags
                                            TypeParameters = member_.TypeParameters
                                            Parameters = parameters
                                            Return = returns
                                        }
                                | ExportValue reference ->
                                    FsProperty
                                        {
                                            Name = member_.Name
                                            Docs = member_.Docs
                                            Tags = member_.Tags
                                            ReadOnly = not member_.Settable
                                            Type = reference
                                        }
                                | ExportConstructor(parameters, returns) ->
                                    FsConstructor
                                        {
                                            Docs = member_.Docs
                                            Tags = member_.Tags
                                            TypeParameters = member_.TypeParameters
                                            Parameters = parameters
                                            Return = returns
                                        })

                        let helper =
                            FsInterface
                                { class_ with
                                    Name = name
                                    TypeParameters = []
                                    Inherits = []
                                    Members = members @ constructors
                                    Entrypoint = None
                                    Statics = []
                                    CreateOverloads = []
                                }

                        shape <-
                            { shape with
                                DeclNames = Map.add valueId name shape.DeclNames
                                Decls = shape.Decls @ [ helper ]
                            }

                        groups <-
                            groups
                            |> List.map (fun group ->
                                if group.Decls |> List.exists (fun decl -> Render.declName decl = class_.Name) then
                                    { group with
                                        Decls = group.Decls @ [ helper ]
                                    }
                                else
                                    group)

                        name

                values <- Map.add class_.Name (Shape.Spec.bindingOf export, FsNamed helper) values
            | None -> fail $"class value export {class_.Name} has no resolved constructor identity"
        | _ -> ()

    shape, groups, values

/// Redirects matching F# references, retains public aliases and value imports, and emits a catalog.
let apply (ctx: Context) (shape: ShapeModel) (groups: Render.GroupModule list) =
    async {
        if
            not ctx.Config.DeclarationCatalog
            && List.isEmpty ctx.Config.DeclarationReferences
        then
            return shape, None
        else
            let shape, groups, classValues = classValues ctx shape groups
            let inferenceProfile = profile ctx.Config
            let compiler = compiler ctx

            let generator =
                typeof<GeneratorConfig>.Assembly.Location |> File.ReadAllBytes |> hash

            let owner = Naming.groupModule ctx.Config ctx.PackageName EntryPackage

            let catalogs =
                ctx.Config.DeclarationReferences
                |> List.map (fun path ->
                    load
                        inferenceProfile
                        compiler
                        generator
                        (Path.GetFullPath(Path.Combine(ctx.PackageDir / uom<dirPath>, path))))

            let inherited =
                catalogs
                |> List.collect (fun catalog -> Array.toList catalog.Declarations)
                |> List.groupBy _.Identity
                |> List.map (fun (key, entries) ->
                    match List.distinct entries with
                    | [ entry ] -> key, entry
                    | _ -> fail $"references disagree on declaration {key}")
                |> Map.ofList

            let rawHandles =
                [
                    yield! shape.Types |> Map.toList |> List.collect (snd >> _.Declarations)
                    for export in shape.Harvest.Exports do
                        yield! export.Symbol.DeclarationHandles |> ValueOption.defaultValue [||]
                ]
                |> List.distinct

            let! inputFiles = ctx.Session.getSourceFileNames ()

            let! sourceFiles =
                sources
                    ctx
                    (rawHandles
                     @ (inputFiles
                        |> Array.map (fun file -> ("0.SourceFile." + file) * uom<Measure.declHandle>)
                        |> Array.toList))

            let inputSources =
                sourceFiles
                |> Map.toList
                |> List.map snd
                |> List.distinct
                |> List.sortBy sourceKey

            let inputKey (source: Source) = source.Package + "/" + source.File

            let currentInputs = inputSources |> List.groupBy sourceKey |> Map.ofList

            for catalog in catalogs do
                for input in catalog.Inputs do
                    match Map.tryFind (sourceKey input) currentInputs with
                    | Some candidates when not (List.contains input candidates) ->
                        if candidates |> List.exists (fun current -> current.Sha256 = input.Sha256) then
                            fail $"package manifest mismatch for {input.Package}"
                        else
                            fail $"input source hash mismatch for {inputKey input}"
                    | _ -> ()

            let identities = identities ctx shape sourceFiles

            let modules =
                groups
                |> List.collect (fun group ->
                    group.Decls
                    |> List.map (fun decl -> Render.declName decl |> (fun name -> name, group.Module)))
                |> Map.ofList

            let reused =
                identities
                |> Map.toList
                |> List.choose (fun (name, identity) ->
                    match Map.tryFind identity.Key inherited with
                    | Some producer ->
                        if producer.Sources |> Array.toList <> identity.Sources then
                            fail $"source hash mismatch for {name} ({producer.FSharpName})"

                        Some(name, producer)
                    | None ->
                        inherited
                        |> Map.iter (fun _ producer ->
                            if
                                producer.Role = identity.Role
                                && Set.ofArray producer.Handles <> Set.ofList identity.Handles
                                && producer.Handles
                                   |> Array.exists (fun handle -> List.contains handle identity.Handles)
                            then
                                fail $"declaration handle set conflict for {name} ({producer.FSharpName})")

                        None)
                |> Map.ofList

            let canonicalNames =
                [
                    yield!
                        identities
                        |> Map.toList
                        |> List.map (fun (name, identity) -> name, identity.Key)
                    yield!
                        inherited
                        |> Map.toList
                        |> List.map (fun (_, entry) -> entry.FSharpName, entry.Identity)
                ]
                |> Map.ofList

            let abbreviations =
                shape.Decls
                |> List.choose (function
                    | FsAbbrev alias -> Some(alias.Name, alias)
                    | _ -> None)
                |> Map.ofList

            let rec canonicalReference visited vars reference =
                let canonical = canonicalReference visited vars

                let named name arguments applied =
                    let arguments = List.map canonical arguments

                    match Map.tryFind name abbreviations with
                    | Some alias when
                        not (Set.contains name visited)
                        && alias.TypeParameters.Length = arguments.Length
                        ->
                        let bindings =
                            List.zip (alias.TypeParameters |> List.map _.Name) arguments |> Map.ofList

                        canonicalReference (Set.add name visited) bindings alias.Target
                    | _ ->
                        let name = Map.tryFind name canonicalNames |> Option.defaultValue name
                        if applied then FsApp(name, arguments) else FsNamed name

                match reference with
                | FsNamed name -> named name [] false
                | FsApp(name, arguments) -> named name arguments true
                | FsTypeVar name -> Map.tryFind name vars |> Option.defaultValue (FsTypeVar name)
                | FsOption inner -> FsOption(canonical inner)
                | FsArray inner -> FsArray(canonical inner)
                | FsTuple items -> FsTuple(List.map canonical items)
                | FsErasedUnion items -> FsErasedUnion(List.map canonical items)
                | FsFunc(argument, returns) -> FsFunc(canonical argument, canonical returns)
                | FsDelegate(arguments, returns) -> FsDelegate(List.map canonical arguments, canonical returns)
                | FsBranded(primitive, measure) ->
                    FsBranded(canonical primitive, Map.tryFind measure canonicalNames |> Option.defaultValue measure)
                | other -> other

            let constraints decl =
                let parameters = parameters decl

                let vars =
                    parameters
                    |> List.mapi (fun index parameter -> parameter.Name, FsTypeVar(string index))
                    |> Map.ofList

                let canonical = canonicalReference Set.empty vars

                parameters
                |> List.map (fun parameter -> parameter.Constraint |> Option.map canonical |> sprintf "%A")
                |> List.toArray

            let surface decl =
                let bind prefix (parameters: FsTypeParam list) vars =
                    parameters
                    |> List.mapi (fun index parameter -> parameter.Name, FsTypeVar(prefix + string index))
                    |> List.fold (fun vars (name, key) -> Map.add name key vars) vars

                let reference = canonicalReference Set.empty

                let vars = bind "type:" (parameters decl) Map.empty

                let parameter vars (parameter: FsParam) =
                    parameter.Optional, parameter.Rest, reference vars parameter.Type

                let signature parameters args returns =
                    let vars = bind "method:" parameters vars

                    sprintf
                        "%A"
                        (parameters
                         |> List.map (fun parameter -> Option.map (reference vars) parameter.Constraint),
                         List.map (parameter vars) args,
                         reference vars returns)

                let member_ =
                    function
                    | FsProperty property ->
                        sprintf "%A" ("property", property.Name, property.ReadOnly, reference vars property.Type)
                    | FsMethod method_ ->
                        sprintf
                            "%A"
                            ("method", method_.Name, signature method_.TypeParameters method_.Parameters method_.Return)
                    | FsConstructor constructor ->
                        "constructor:"
                        + signature constructor.TypeParameters constructor.Parameters constructor.Return
                    | FsInvoke invoke -> "invoke:" + signature invoke.TypeParameters invoke.Parameters invoke.Return
                    | FsIndexer index ->
                        sprintf "%A" ("index", index.ReadOnly, reference vars index.Key, reference vars index.Value)

                let api =
                    match decl with
                    | FsInterface interface_ ->
                        let entrypoint =
                            interface_.Entrypoint
                            |> Option.map (fun entrypoint ->
                                List.map (parameter vars) entrypoint.Parameters,
                                Option.map (reference vars) entrypoint.Inherits)

                        sprintf
                            "%A"
                            ("interface",
                             List.map (reference vars) interface_.Inherits,
                             interface_.Members |> List.map member_ |> List.sort,
                             entrypoint)
                    | FsAbbrev abbrev -> sprintf "%A" ("alias", reference vars abbrev.Target)
                    | FsDelegateType delegate_ ->
                        sprintf
                            "%A"
                            ("delegate",
                             delegate_.Parameters
                             |> List.map (fun parameter -> reference vars parameter.Type),
                             reference vars delegate_.Return)
                    | FsStringEnum enum -> sprintf "%A" ("string-enum", enum.Cases)
                    | FsEnum enum -> sprintf "%A" ("enum", enum.Cases)
                    | FsTaggedUnion union ->
                        sprintf
                            "%A"
                            ("tagged",
                             union.Tag,
                             union.Cases
                             |> List.map (fun case ->
                                 case.Name,
                                 case.CompiledName,
                                 case.Fields |> List.map (fun field -> field.Name, reference vars field.Type)))
                    | FsPhantom phantom -> sprintf "%A" ("phantom", reference vars phantom.Carrier)
                    | FsMeasure measure -> sprintf "%A" ("measure", reference vars measure.Primitive)
                    | FsExports _ -> "exports"

                hashText api

            let owned =
                shape.Decls
                |> List.choose (fun decl ->
                    Render.declName decl
                    |> (fun name ->
                        let constraints = constraints decl

                        match Map.tryFind name reused with
                        | Some producer ->
                            if producer.Arity <> List.length (parameters decl) then
                                fail $"arity mismatch for {name} ({producer.FSharpName})"

                            if producer.Constraints <> constraints then
                                fail $"constraint mismatch for {name} ({producer.FSharpName})"

                            if producer.Api <> surface decl then
                                fail $"F# API mismatch for {name} ({producer.FSharpName})"

                            None
                        | None ->
                            let identity = identities[name]

                            Some
                                {
                                    Identity = identity.Key
                                    Handles = List.toArray identity.Handles
                                    Sources = List.toArray identity.Sources
                                    Role = identity.Role
                                    FSharpName = modules[name] + "." + name
                                    Arity = List.length (parameters decl)
                                    Constraints = constraints
                                    Api = surface decl
                                    Owner = owner
                                }))

            let exportedNames =
                shape.Harvest.Exports
                |> List.map (Shape.Spec.fsName (Shape.Spec.defaultExportName ctx))
                |> Set.ofList

            let localName (declaration: Declaration) =
                identities
                |> Map.toList
                |> List.pick (fun (name, identity) ->
                    if
                        identity.Key = declaration.Identity
                        && modules[name] + "." + name = declaration.FSharpName
                    then
                        Some name
                    else
                        None)

            let owned, localAliases =
                owned
                |> List.groupBy _.Identity
                |> List.map (fun (identity, entries) ->
                    let entries =
                        entries
                        |> List.sortBy (fun entry ->
                            let name = localName entry
                            not (Set.contains name exportedNames), name.Split('.').Length, name)

                    let canonical = List.head entries

                    for entry in List.tail entries do
                        if
                            entry.Arity <> canonical.Arity
                            || entry.Constraints <> canonical.Constraints
                            || entry.Sources <> canonical.Sources
                            || entry.Api <> canonical.Api
                        then
                            fail
                                $"incompatible F# declarations claim identity {identity}: {canonical.FSharpName}, {entry.FSharpName} (arity {canonical.Arity}/{entry.Arity}, constraints {canonical.Constraints = entry.Constraints}, sources {canonical.Sources.Length}/{entry.Sources.Length})"

                    canonical, List.tail entries |> List.map (fun entry -> localName entry, canonical))
                |> List.unzip

            let reused =
                localAliases
                |> List.concat
                |> List.fold (fun map (name, declaration) -> Map.add name declaration map) reused

            let redirects = reused |> Map.map (fun _ producer -> producer.FSharpName)

            let declarations =
                shape.Decls
                |> List.choose (fun decl ->
                    match
                        Render.declName decl
                        |> (fun name -> Map.tryFind name reused |> Option.map (fun producer -> name, producer))
                    with
                    | None -> Some(Render.qualifyDecl redirects decl)
                    | Some(name, producer) when Set.contains name exportedNames ->
                        let parameters = parameters decl

                        let target =
                            match parameters with
                            | [] -> FsNamed producer.FSharpName
                            | parameters ->
                                FsApp(
                                    producer.FSharpName,
                                    parameters |> List.map (fun parameter -> FsTypeVar parameter.Name)
                                )

                        Some(
                            Render.qualifyDecl
                                redirects
                                (FsAbbrev
                                    {
                                        Value = Map.tryFind name classValues
                                        Name = name
                                        Docs = ""
                                        Tags = []
                                        Order = order decl
                                        TypeParameters = parameters
                                        Target = target
                                    })
                        )
                    | Some _ -> None)

            let owners =
                [
                    yield! catalogs |> List.collect (fun catalog -> Array.toList catalog.Owners)
                    yield
                        {
                            Name = owner
                            Dependencies =
                                reused
                                |> Map.toList
                                |> List.map (snd >> _.Owner)
                                |> List.filter ((<>) owner)
                                |> List.distinct
                                |> List.sort
                                |> List.toArray
                        }
                ]
                |> ownerOrder

            let allDeclarations =
                [ yield! inherited |> Map.toList |> List.map snd; yield! owned ]
                |> List.groupBy _.Identity
                |> List.map (fun (identity, entries) ->
                    match List.distinct entries with
                    | [ declaration ] -> declaration
                    | _ ->
                        let names = entries |> List.map _.FSharpName |> String.concat ", "
                        fail $"multiple F# declarations claim identity {identity}: {names}")
                |> List.sortBy (fun declaration -> declaration.Owner, declaration.FSharpName)

            let catalog =
                {
                    SchemaVersion = 1
                    Compiler = compiler
                    Generator = generator
                    InferenceProfile = inferenceProfile
                    Owner = owner
                    Inputs = List.toArray inputSources
                    Owners = List.toArray owners
                    Declarations = List.toArray allDeclarations
                }

            return
                { shape with Decls = declarations },
                if ctx.Config.DeclarationCatalog then
                    Some(JsonSerializer.Serialize(catalog, options) + "\n")
                else
                    None
    }
