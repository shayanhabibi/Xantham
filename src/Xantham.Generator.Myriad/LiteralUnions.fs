namespace Xantham.Generator.Myriad

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open Myriad.Core
open Xantham.Generator
open Xantham.Generator.Customization

/// A resolved declaration and the companion module and type exposed to F# callers.
type LiteralUnionSelection =
    {
        Package: string
        Path: string list
        ModuleName: string
        TypeName: string
    }

module internal Source =
    let validateNames moduleName typeName =
        let writable name =
            not (String.IsNullOrWhiteSpace name)
            && Identifier.isPlain name
            && Render.ident name = name

        if
            String.IsNullOrWhiteSpace moduleName
            || moduleName.Split('.') |> Array.exists (writable >> not)
        then
            invalidArg "moduleName" "The module name must contain unquoted ASCII F# name segments"

        if not (writable typeName) || not (Char.IsUpper typeName[0]) then
            invalidArg "typeName" "The type name must be an unquoted ASCII F# name starting with an uppercase letter"

    let stringLiteral (value: string) = JsonSerializer.Serialize value

    let render moduleName typeName (arms: ResolvedUnionArm list) =
        validateNames moduleName typeName

        if List.isEmpty arms then
            invalidArg "arms" "A literal union must contain at least one arm"

        let strings =
            arms
            |> List.choose (function
                | ResolvedUnionArm.StringLiteral value -> Some value
                | _ -> None)
            |> List.distinct
            |> List.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))

        let has arm = List.contains arm arms

        let typedCases =
            [
                if has ResolvedUnionArm.Number then
                    "Number"
                if has ResolvedUnionArm.Null then
                    "Null"
                if has ResolvedUnionArm.Undefined then
                    "Undefined"
            ]

        let candidates = typedCases @ (strings |> List.map Naming.enumCaseOfString)

        let rec allocate reserved =
            let names =
                (Set.toList reserved @ candidates)
                |> Shape.Spec.uniqueCaseNames
                |> List.skip reserved.Count

            let expanded =
                Set.union reserved (names |> List.map (fun name -> "Is" + name) |> Set.ofList)

            if expanded = reserved then names else allocate expanded

        let names =
            allocate (Set.ofList [ "Tags"; "ToString" ])
            |> List.skip typedCases.Length
            |> List.map Render.ident

        let literalCases = List.zip strings names
        let typ = Render.ident typeName
        let modul = moduleName.Split('.') |> Array.map Render.ident |> String.concat "."

        let predicates =
            [
                for value, name in literalCases do
                    $"equalsLiteral value {stringLiteral value}", $"{typ}.{name}"
                if has ResolvedUnionArm.Number then
                    "isNumber value", $"({typ}.Number (numberAfterCheck value))"
                if has ResolvedUnionArm.Null then
                    "isNull value", $"{typ}.Null"
                if has ResolvedUnionArm.Undefined then
                    "isUndefined value", $"{typ}.Undefined"
            ]

        [
            $"module {modul}"
            ""
            "open Fable.Core"
            ""
            "[<RequireQualifiedAccess>]"
            $"type {typ} ="
            for _, name in literalCases do
                $"    | {name}"
            if has ResolvedUnionArm.Number then
                "    | Number of float"
            if has ResolvedUnionArm.Null then
                "    | Null"
            if has ResolvedUnionArm.Undefined then
                "    | Undefined"
            ""
            if not (List.isEmpty literalCases) then
                "[<Emit(\"$0 === $1\")>]"
                "let private equalsLiteral (_: obj) (_: string) : bool = jsNative"
                ""
            if has ResolvedUnionArm.Number then
                "[<Emit(\"typeof $0 === 'number'\")>]"
                "let private isNumber (_: obj) : bool = jsNative"
                ""
                "[<Emit(\"$0\")>]"
                "let private numberAfterCheck (_: obj) : float = jsNative"
                ""
            if has ResolvedUnionArm.Null then
                "[<Emit(\"$0 === null\")>]"
                "let private isNull (_: obj) : bool = jsNative"
                ""
            if has ResolvedUnionArm.Undefined then
                "[<Emit(\"$0 === undefined\")>]"
                "let private isUndefined (_: obj) : bool = jsNative"
                ""
                "[<Emit(\"undefined\")>]"
                "let private undefinedValue () : obj = jsNative"
                ""
            $"let encode (value: {typ}) : obj ="
            "    match value with"
            for value, name in literalCases do
                $"    | {typ}.{name} -> box {stringLiteral value}"
            if has ResolvedUnionArm.Number then
                $"    | {typ}.Number number -> box number"
            if has ResolvedUnionArm.Null then
                $"    | {typ}.Null -> null"
            if has ResolvedUnionArm.Undefined then
                $"    | {typ}.Undefined -> undefinedValue ()"
            ""
            $"let decode (value: obj) : Microsoft.FSharp.Core.Result<{typ}, string> ="
            for index, (predicate, result) in List.indexed predicates do
                let branch = if index = 0 then "if" else "elif"
                $"    {branch} {predicate} then Ok {result}"
            "    else Error \"Value is outside the declared union\""
            ""
            "let (|Decoded|Invalid|) (value: obj) ="
            "    match decode value with"
            "    | Ok typed -> Decoded typed"
            "    | Error diagnostic -> Invalid diagnostic"
            ""
        ]
        |> String.concat "\n"

    let json moduleName typeName arms =
        let strings, others =
            arms
            |> List.fold
                (fun (strings, others) arm ->
                    match arm with
                    | ResolvedUnionArm.StringLiteral value -> value :: strings, others
                    | ResolvedUnionArm.Number -> strings, "number" :: others
                    | ResolvedUnionArm.Null -> strings, "null" :: others
                    | ResolvedUnionArm.Undefined -> strings, "undefined" :: others)
                ([], [])

        JsonSerializer.Serialize
            {|
                formatVersion = 1
                moduleName = moduleName
                typeName = typeName
                stringLiterals = List.rev strings
                otherArms = List.rev others
            |}

    let read inputFile =
        use document = JsonDocument.Parse(File.ReadAllText inputFile)
        let root = document.RootElement

        if root.GetProperty("formatVersion").GetInt32() <> 1 then
            invalidArg "inputFile" "Unsupported literal-union input format"

        let text (element: JsonElement) =
            let value = element.GetString()

            if isNull value then
                invalidArg "inputFile" "Literal-union string fields must be strings"

            value

        let moduleName = root.GetProperty("moduleName") |> text
        let typeName = root.GetProperty("typeName") |> text

        let arms =
            [
                for value in root.GetProperty("stringLiterals").EnumerateArray() do
                    ResolvedUnionArm.StringLiteral(text value)
                for value in root.GetProperty("otherArms").EnumerateArray() do
                    match text value with
                    | "number" -> ResolvedUnionArm.Number
                    | "null" -> ResolvedUnionArm.Null
                    | "undefined" -> ResolvedUnionArm.Undefined
                    | value -> invalidArg "inputFile" $"Unsupported literal-union arm: {value}"
            ]

        render moduleName typeName arms

/// Emits ordinary F# unions, membership codecs and a Decoded/Invalid active pattern.
[<MyriadGenerator("xantham-literal-unions")>]
type LiteralUnionGenerator() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".json" ]

        member _.Generate context =
            Output.Source(Source.read context.InputFilename)

module internal ProjectionRunner =
    let generate
        receiverType
        workspace
        (plugin: IMyriadGenerator)
        (input: string)
        source
        moduleName
        exportedTypes
        snapshot
        =
        try
            let directory = Path.Combine(workspace, Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory directory |> ignore

            try
                let inputFile = Path.Combine(directory, "projection.json")
                File.WriteAllText(inputFile, input)

                let context =
                    GeneratorContext.Create(None, (fun _ -> Seq.empty), inputFile, None, Dictionary<string, string>())

                match plugin.Generate context with
                | Output.Source text ->
                    match receiverType with
                    | None -> Ok(ProjectionCompanion.create source (moduleName + ".fs") exportedTypes text snapshot)
                    | Some receiver ->
                        Ok(
                            ProjectionCompanion.forOperation
                                source
                                receiver
                                (moduleName + ".fs")
                                exportedTypes
                                text
                                snapshot
                        )
                | Output.Ast _ -> invalidOp "The projection generator must return source text"
            finally
                Directory.Delete(directory, true)
        with error ->
            Error
                [
                    Diagnostic.create "myriad/generation-failed" error.Message (Some(Resolved.identity source snapshot))
                ]

module LiteralUnions =
    /// Selects declarations before Shape and invokes Myriad in a caller-owned workspace.
    let create (identity: ExtensionIdentity) workspace (selections: LiteralUnionSelection list) : ProjectionExtension =
        let configurationKey = "xantham.myriad.literal-unions"

        if Map.containsKey configurationKey identity.Configuration then
            invalidArg "identity" $"Configuration key '{configurationKey}' is reserved for the adapter"

        for selection in selections do
            Source.validateNames selection.ModuleName selection.TypeName

        let configuration =
            selections
            |> List.map (fun selection ->
                {|
                    package = selection.Package
                    path = selection.Path
                    moduleName = selection.ModuleName
                    typeName = selection.TypeName
                |})
            |> fun selections ->
                JsonSerializer.Serialize
                    {|
                        formatVersion = 1
                        selections = selections
                    |}

        {
            Identity =
                { identity with
                    Configuration = Map.add configurationKey configuration identity.Configuration
                }
            Transform =
                fun snapshot ->
                    let generate selection =
                        match Resolved.tryFind selection.Package selection.Path snapshot with
                        | None ->
                            let path = String.concat "." selection.Path

                            Error
                                [
                                    Diagnostic.create
                                        "myriad/missing-declaration"
                                        $"Declaration {selection.Package}/{path} was not resolved"
                                        None
                                ]
                        | Some source ->
                            match Resolved.union source snapshot with
                            | Error diagnostics -> Error diagnostics
                            | Ok arms ->
                                ProjectionRunner.generate
                                    None
                                    workspace
                                    (LiteralUnionGenerator() :> IMyriadGenerator)
                                    (Source.json selection.ModuleName selection.TypeName arms)
                                    source
                                    selection.ModuleName
                                    [ selection.ModuleName + "." + selection.TypeName ]
                                    snapshot

                    let results = List.map generate selections

                    let diagnostics =
                        results
                        |> List.collect (function
                            | Error diagnostics -> diagnostics
                            | Ok _ -> [])

                    if List.isEmpty diagnostics then
                        Ok(
                            results
                            |> List.choose (function
                                | Ok companion -> Some companion
                                | Error _ -> None)
                        )
                    else
                        Error diagnostics
        }
