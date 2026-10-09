module internal Xantham.Generator.Customization.Semantics

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator
open Xantham.Generator.Measure

let private packageName (ctx: Context) =
    function
    | CompilerLib -> "typescript/lib"
    | Dependency name -> name / uom<_>
    | _ -> ctx.PackageName / uom<_>

let private packageBoundary (file: string) =
    let rec visit directory =
        let manifest = Path.Combine(directory, "package.json")

        if File.Exists manifest then
            use document = JsonDocument.Parse(File.ReadAllText manifest)

            match document.RootElement.TryGetProperty "name" with
            | true, name when name.ValueKind = JsonValueKind.String -> Some(name.GetString(), directory)
            | _ -> None
        else
            let parent = Directory.GetParent directory
            if isNull parent then None else visit parent.FullName

    visit (Path.GetDirectoryName(Path.GetFullPath file))

let private handleFile (handle: string<declHandle>) =
    match (handle / uom<declHandle>).Split([| '.' |], 3) with
    | [| _; _; file |] -> Some file
    | _ -> None

let private sourcePackage ctx origin handles =
    if origin = CompilerLib then
        "typescript/lib"
    else
        handles
        |> List.tryPick (handleFile >> Option.bind packageBoundary >> Option.map fst)
        |> Option.defaultValue (packageName ctx origin)

let private normalizedHandle (ctx: Context) (handle: string<declHandle>) =
    match (handle / uom<declHandle>).Split([| '.' |], 3) with
    | [| index; kind; file |] ->
        let _, owner, path = Grouping.sourceOrderKey ctx.PackageDir (file * uom<filePath>)

        let owner, path =
            match packageBoundary file with
            | Some(package, boundary) -> package, Path.GetRelativePath(boundary, file).Replace('\\', '/')
            | None -> owner, path

        String.concat ":" [ owner; path; kind; index ]
    | _ -> invalidOp "customization/missing-source-metadata: malformed declaration handle"

let private declarationIdentity ctx handles =
    handles
    |> List.map (normalizedHandle ctx)
    |> List.distinct
    |> List.sort
    |> JsonSerializer.Serialize

let private declaredPath (harvest: HarvestModel) (symbol: SymbolResponse) =
    let parent =
        symbol.ParentSymbolId
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id harvest.Namespaces)
        |> Option.map (fun name -> (name / uom<symbolName>).Split '.' |> Array.toList)
        |> Option.defaultValue []

    parent @ [ symbol.Name ]

/// Captures exported declarations separately from their checker type ids: two named aliases
/// may resolve to the same union while retaining different source declarations.
let projectResolved (ctx: Context) (model: ResolveModel) =
    async {
        let projectExport (export: HarvestedExport) =
            let handles =
                export.Symbol.DeclarationHandles
                |> ValueOption.defaultValue [||]
                |> Array.toList

            let path =
                declaredPath
                    model.Harvest
                    { export.Symbol with
                        Name = export.ExportName
                    }

            let source = String.concat "." path
            let origin = Grouping.classify ctx.PackageDir (ValueSome export.Symbol)
            let package = sourcePackage ctx origin handles

            let diagnostic code message =
                Diagnostic.create ("projection/" + code) message (Some source)

            let declaration =
                if List.isEmpty handles then
                    Error
                        [
                            diagnostic "missing-source-metadata" "The exported declaration has no source handles"
                        ]
                else
                    try
                        Ok(declarationIdentity ctx handles)
                    with :? InvalidOperationException ->
                        Error
                            [
                                diagnostic
                                    "missing-source-metadata"
                                    "The exported declaration has malformed source handles"
                            ]

            let rec arms visited id =
                match Map.tryFind id model.NotFollowed, Map.tryFind id model.Types with
                | Some reason, _ ->
                    Error
                        [
                            diagnostic "incomplete-union" $"A selected union constituent was not resolved: {reason}"
                        ]
                | _, None ->
                    Error
                        [
                            diagnostic
                                "incomplete-union"
                                "A selected union constituent is missing from the resolved type table"
                        ]
                | _, Some facts when Set.contains id visited ->
                    Error
                        [
                            diagnostic "incomplete-union" "The selected union contains a recursive constituent"
                        ]
                | _, Some facts when not (List.isEmpty facts.AliasTypeArguments) ->
                    Error
                        [
                            diagnostic
                                "unsupported-union"
                                "Generic aliases are outside the finite union projection contract"
                        ]
                | _, Some facts when facts.Response.Flags.HasFlag TypeFlags.Union ->
                    if List.isEmpty facts.UnionMembers then
                        Error
                            [
                                diagnostic "incomplete-union" "The selected union has no resolved constituents"
                            ]
                    else
                        let results = facts.UnionMembers |> List.map (arms (Set.add id visited))

                        let errors =
                            results
                            |> List.collect (function
                                | Error errors -> errors
                                | Ok _ -> [])

                        if List.isEmpty errors then
                            results
                            |> List.collect (function
                                | Ok values -> values
                                | Error _ -> [])
                            |> Ok
                        else
                            Error errors
                | _, Some facts ->
                    match facts.Response.Flags with
                    | TypeFlags.StringLiteral ->
                        try
                            match Shape.Spec.literalOf facts with
                            | Some(LitString value) -> Ok [ ResolvedUnionArm.StringLiteral value ]
                            | _ ->
                                Error
                                    [
                                        diagnostic
                                            "incomplete-union"
                                            "A string literal constituent has no literal value"
                                    ]
                        with :? InvalidOperationException ->
                            Error
                                [
                                    diagnostic
                                        "incomplete-union"
                                        "A string literal constituent has an invalid literal value"
                                ]
                    | TypeFlags.Number -> Ok [ ResolvedUnionArm.Number ]
                    | TypeFlags.Null -> Ok [ ResolvedUnionArm.Null ]
                    | TypeFlags.Undefined -> Ok [ ResolvedUnionArm.Undefined ]
                    | flags ->
                        Error
                            [
                                diagnostic
                                    "unsupported-union"
                                    $"The selected constituent {flags} is outside the finite union projection contract"
                            ]

            let union =
                match Map.tryFind export.Symbol.SymbolId model.ExportTypes |> Option.bind _.Declared with
                | None -> Error [ diagnostic "incomplete-union" "The export has no resolved declared type" ]
                | Some id -> arms Set.empty id |> Result.map (List.distinct >> List.sort)

            let fingerprint =
                match declaration, union with
                | Ok identity, Ok union ->
                    try
                        let sources =
                            handles
                            |> List.map (fun handle ->
                                let file = handleFile handle |> Option.get

                                normalizedHandle ctx handle,
                                File.ReadAllBytes file |> SHA256.HashData |> Convert.ToHexString)
                            |> List.distinct
                            |> List.sort

                        let armKeys =
                            union
                            |> List.map (function
                                | ResolvedUnionArm.StringLiteral value -> "string:" + JsonSerializer.Serialize value
                                | ResolvedUnionArm.Number -> "number"
                                | ResolvedUnionArm.Null -> "null"
                                | ResolvedUnionArm.Undefined -> "undefined")

                        JsonSerializer.Serialize(("resolved-union-v1", identity, sources, armKeys))
                        |> Encoding.UTF8.GetBytes
                        |> SHA256.HashData
                        |> Convert.ToHexString
                        |> fun value -> Ok(value.ToLowerInvariant())
                    with
                    | :? IOException
                    | :? UnauthorizedAccessException ->
                        Error
                            [
                                diagnostic
                                    "missing-source-metadata"
                                    "The selected declaration source cannot be read for authentication"
                            ]
                | Error errors, _ -> Error errors
                | _, Error errors -> Error errors

            {
                Package = package
                Path = path
                Declaration = declaration |> Result.toOption
                Fingerprint = fingerprint |> Result.toOption
                Union =
                    match fingerprint with
                    | Ok _ -> union
                    | Error errors -> Error errors
            }

        let! sources =
            model.Harvest.Exports
            |> List.map (fun export ->
                async {
                    let info = projectExport export

                    match info.Union with
                    | Error _ -> return info
                    | Ok _ ->
                        let! parameters = Resolve.declarationHasTypeParameters ctx export.Symbol

                        let diagnostic message =
                            Diagnostic.create
                                "projection/unsupported-union"
                                message
                                (Some(String.concat "." info.Path))

                        match parameters with
                        | Ok false -> return info
                        | Ok true ->
                            return
                                { info with
                                    Fingerprint = None
                                    Union =
                                        Error
                                            [
                                                diagnostic
                                                    "Generic aliases are outside the finite union projection contract"
                                            ]
                                }
                        | Error reason ->
                            return
                                { info with
                                    Fingerprint = None
                                    Union =
                                        Error
                                            [
                                                Diagnostic.create
                                                    "projection/missing-source-metadata"
                                                    reason
                                                    (Some(String.concat "." info.Path))
                                            ]
                                }
                })
            |> Async.Sequential

        return
            sources
            |> Array.toList
            |> List.distinctBy (fun info -> info.Package, info.Path, info.Declaration)
            |> ContractData.resolvedSnapshot
    }

let project (ctx: Context) (shape: ShapeModel) (findings: Finding list) =
    async {
        let mutable table = shape.Types
        let mutable symbols = Map.empty
        let mutable diagnostics = []
        let mutable pending = table |> Map.toList |> List.map fst
        let mutable visited = Set.empty

        while not (List.isEmpty pending) do
            let typeId = List.head pending
            pending <- List.tail pending

            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited
                let facts = table[typeId]
                let! actual = ctx.Session.getSymbolOfType (typeId / uom<_>)

                let! symbol =
                    match actual with
                    | ValueSome _ -> async.Return actual
                    | ValueNone -> ctx.Session.getAliasSymbolOfType (typeId / uom<_>)

                match symbol with
                | ValueSome symbol when not (symbol.Name.StartsWith "__") ->
                    symbols <- Map.add typeId symbol symbols

                    let handles =
                        symbol.DeclarationHandles |> ValueOption.defaultValue [||] |> Array.toList

                    let origin = Grouping.classify ctx.PackageDir (ValueSome symbol)

                    let facts =
                        { facts with
                            SymbolName = Some symbol.SymbolName
                            SymbolParent = symbol.ParentSymbolId |> ValueOption.toOption
                            Origin = origin
                            Declarations = handles
                        }

                    let! resolved, discovered =
                        if
                            facts.Response.Flags.HasFlag TypeFlags.Object
                            && not (symbol.Flags.HasFlag SymbolFlags.TypeParameter)
                        then
                            Resolve.customizationFacts ctx facts
                        else
                            async.Return(facts, [])

                    table <- Map.add typeId resolved table

                    for response in discovered do
                        if not (Map.containsKey response.TypeId table) then
                            table <- Map.add response.TypeId (TypeFacts.shallow response) table

                        if List.contains response.TypeId resolved.BaseTypes then
                            pending <- pending @ [ response.TypeId ]
                | _ -> ()

        let projectionShape = { shape with Types = table }

        let declaredPath = declaredPath shape.Harvest

        let declarationIds =
            symbols
            |> Map.toList
            |> List.choose (fun (id, symbol) ->
                let handles = table[id].Declarations

                if List.isEmpty handles then
                    diagnostics <-
                        diagnostics
                        @ [
                            Diagnostic.create
                                "customization/missing-source-metadata"
                                $"Source metadata is unavailable for {symbol.Name}"
                                (Some symbol.Name)
                        ]

                    None
                else
                    Some(id, declarationIdentity ctx handles))
            |> Map.ofList

        let keyOf id =
            let declaration = declarationIds[id]
            let facts = table[id]

            let arguments =
                facts.TypeArguments
                |> List.map (fun arg ->
                    Shape.Spec.typeRef ctx projectionShape None "customization" arg
                    |> fst
                    |> Render.printType)

            JsonSerializer.Serialize((declaration, arguments))

        let keys = declarationIds |> Map.map (fun id _ -> keyOf id)

        let sourceForSymbol =
            symbols
            |> Map.toList
            |> List.filter (fun (id, _) -> Map.containsKey id keys)
            |> List.sortBy (fun (id, _) -> table[id].Response.Target |> ValueOption.exists ((<>) (id / uom<_>)))
            |> List.fold
                (fun map (id, symbol) ->
                    if Map.containsKey symbol.SymbolId map then
                        map
                    else
                        Map.add symbol.SymbolId keys[id] map)
                Map.empty

        let exportsFor id =
            shape.Harvest.Exports
            |> List.choose (fun export ->
                match Map.tryFind export.Symbol.SymbolId shape.ExportTypes with
                | Some types when types.Declared = Some id ->
                    Some(
                        declaredPath
                            { export.Symbol with
                                Name = export.ExportName
                            }
                    )
                | _ -> None)

        let mutable memberInfos = []

        let typeInfos =
            keys
            |> Map.toList
            |> List.map (fun (id, key) ->
                let facts = table[id]
                let symbol = symbols[id]

                let isInstantiation =
                    facts.Response.Target |> ValueOption.exists ((<>) (id / uom<_>))

                let paths =
                    if isInstantiation then
                        []
                    else
                        [ declaredPath symbol ] @ exportsFor id |> List.distinct

                let target = Map.tryFind id shape.DeclNames
                let external = GeneratorConfig.disposition ctx.Config facts.Origin <> Ship

                let properties, excluded =
                    facts.Members
                    |> List.partition (fun member_ -> not (member_.Symbol.Flags.HasFlag SymbolFlags.Method))

                let memberKeys =
                    facts.Members
                    |> List.distinctBy (fun m -> m.Symbol.Name)
                    |> List.map (fun member_ ->
                        let name = member_.Symbol.Name
                        let memberKey = JsonSerializer.Serialize((key, name))

                        let declaring =
                            member_.Symbol.ParentSymbolId
                            |> ValueOption.toOption
                            |> Option.bind (fun parent -> Map.tryFind parent sourceForSymbol)
                            |> Option.defaultValue key

                        let mapped, mappedFindings =
                            Shape.Spec.typeRef ctx projectionShape target (symbol.Name + "." + name) member_.TypeId

                        let mapped =
                            target
                            |> Option.bind (fun target ->
                                shape.Decls
                                |> List.tryPick (function
                                    | FsInterface decl when decl.Name = target ->
                                        decl.Members
                                        |> List.tryPick (function
                                            | FsProperty p when p.Name = name -> Some p.Type
                                            | _ -> None)
                                    | _ -> None))
                            |> Option.defaultValue mapped

                        let mapped =
                            if
                                member_.Optional
                                && not (
                                    match mapped with
                                    | FsOption _ -> true
                                    | _ -> false
                                )
                            then
                                FsOption mapped
                            else
                                mapped

                        let targets =
                            target
                            |> Option.toList
                            |> List.collect (fun target ->
                                shape.Decls
                                |> List.collect (function
                                    | FsInterface decl when decl.Name = target ->
                                        decl.Members
                                        |> List.choose (function
                                            | FsProperty p when p.Name = name -> Some(target, name, external)
                                            | FsMethod m when m.Name = name -> Some(target, name, external)
                                            | _ -> None)
                                    | _ -> []))

                        memberInfos <-
                            memberInfos
                            @ [
                                {
                                    Key = memberKey
                                    Receiver = key
                                    Declaring = declaring
                                    Name = name
                                    IsProperty = not (member_.Symbol.Flags.HasFlag SymbolFlags.Method)
                                    ReadOnly = member_.ReadOnly
                                    Optional = member_.Optional
                                    Type = mapped
                                    Findings = mappedFindings
                                    Targets = targets
                                }
                            ]

                        memberKey)

                {
                    Key = key
                    Package = sourcePackage ctx facts.Origin facts.Declarations
                    Paths = paths
                    Declaration = declarationIds[id]
                    Bases = facts.BaseTypes |> List.choose (fun id -> Map.tryFind id keys)
                    Implemented = facts.ImplementedTypes |> List.choose (fun id -> Map.tryFind id keys)
                    Members = memberKeys
                    Target = target
                    External = external
                    Findings =
                        findings
                        |> List.filter (fun f -> f.Symbol = symbol.Name || f.Symbol.StartsWith(symbol.Name + "."))
                    Excluded =
                        (excluded |> List.map (fun m -> "method:" + m.Symbol.Name))
                        @ (facts.IndexInfos |> List.map (fun _ -> "indexer"))
                })

        let typeInfos =
            typeInfos
            |> List.groupBy _.Key
            |> List.map (fun (_, infos) ->
                let first = List.head infos

                { first with
                    Paths = infos |> List.collect _.Paths |> List.distinct
                    Members = infos |> List.collect _.Members |> List.distinct
                })

        let aliases =
            shape.Harvest.Exports
            |> List.choose (fun export ->
                Map.tryFind export.Symbol.SymbolId shape.ExportTypes
                |> Option.bind _.Declared
                |> Option.bind (fun id -> Map.tryFind id keys)
                |> Option.bind (fun key -> typeInfos |> List.tryFind (fun info -> info.Key = key))
                |> Option.bind (fun info ->
                    let owner =
                        sourcePackage
                            ctx
                            (Grouping.classify ctx.PackageDir (ValueSome export.Symbol))
                            (export.Symbol.DeclarationHandles
                             |> ValueOption.defaultValue [||]
                             |> Array.toList)

                    let path =
                        declaredPath
                            { export.Symbol with
                                Name = export.ExportName
                            }

                    if owner = info.Package then
                        None
                    else
                        Some
                            { info with
                                Key = JsonSerializer.Serialize((owner, path, info.Key))
                                Package = owner
                                Paths = [ path ]
                                Bases = [ info.Key ]
                            }))

        let exportTypes =
            shape.Decls
            |> List.choose (function
                | FsExports container -> Some container
                | _ -> None)
            |> List.map (fun container ->
                let key =
                    JsonSerializer.Serialize((ctx.PackageName / uom<_>, "exports", container.Name))

                let memberKeys =
                    container.Members
                    |> List.map (fun owned ->
                        let source =
                            shape.Harvest.Exports
                            |> List.find (fun export -> export.Symbol.SymbolId = owned.SourceSymbolId)

                        let handles =
                            source.Symbol.DeclarationHandles
                            |> ValueOption.defaultValue [||]
                            |> Array.toList

                        let memberKey =
                            JsonSerializer.Serialize(
                                (handles |> List.map (normalizedHandle ctx) |> List.sort,
                                 owned.ExportName,
                                 owned.Member.Name)
                            )

                        let reference =
                            match owned.Member.Body with
                            | ExportValue reference -> reference
                            | ExportFunction(args, returns)
                            | ExportConstructor(args, returns) -> FsDelegate(args |> List.map _.Type, returns)

                        memberInfos <-
                            memberInfos
                            @ [
                                {
                                    Key = memberKey
                                    Receiver = key
                                    Declaring = key
                                    Name = owned.ExportName
                                    IsProperty = false
                                    ReadOnly = true
                                    Optional = false
                                    Type = reference
                                    Findings = []
                                    Targets = [ container.Name, owned.Member.Name, false ]
                                }
                            ]

                        memberKey)
                    |> List.distinct

                {
                    Key = key
                    Package = ctx.PackageName / uom<_>
                    Paths = []
                    Declaration = key
                    Bases = []
                    Implemented = []
                    Members = memberKeys
                    Target = None
                    External = false
                    Findings = []
                    Excluded = []
                })

        return
            ContractData.snapshot (typeInfos @ aliases @ exportTypes) (memberInfos |> List.distinctBy _.Key) diagnostics
    }
