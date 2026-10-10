namespace Xantham.Generator.Myriad

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Myriad.Core
open Xantham.Generator
open Xantham.Generator.Customization

/// A method argument, or one optional field of that argument, exposed as a strict F# contract.
type OperationSelection =
    {
        Package: string
        ReceiverPath: string list
        MethodName: string
        ParameterName: string
        FieldName: string option
        ReceiverType: string
        ModuleName: string
        TypeName: string
        FunctionName: string
    }

module private OperationSource =
    let validate selection =
        Source.validateNames selection.ModuleName selection.TypeName
        Source.validateNames selection.ReceiverType "Value"

        if
            not (Identifier.isPlain selection.FunctionName)
            || Render.ident selection.FunctionName <> selection.FunctionName
        then
            invalidArg "selection" "An operation name must be an unquoted F# identifier"

    let rec writeShape =
        function
        | ResolvedValueShape.String ->
            JsonObject [ KeyValuePair("kind", JsonValue.Create "string" :> JsonNode) ] :> JsonNode
        | ResolvedValueShape.Number ->
            JsonObject [ KeyValuePair("kind", JsonValue.Create "number" :> JsonNode) ] :> JsonNode
        | ResolvedValueShape.Boolean ->
            JsonObject [ KeyValuePair("kind", JsonValue.Create "boolean" :> JsonNode) ] :> JsonNode
        | ResolvedValueShape.Null ->
            JsonObject [ KeyValuePair("kind", JsonValue.Create "null" :> JsonNode) ] :> JsonNode
        | ResolvedValueShape.Undefined ->
            JsonObject [ KeyValuePair("kind", JsonValue.Create "undefined" :> JsonNode) ] :> JsonNode
        | ResolvedValueShape.StringLiteral value ->
            JsonObject
                [
                    KeyValuePair("kind", JsonValue.Create "literal" :> JsonNode)
                    KeyValuePair("value", JsonValue.Create value :> JsonNode)
                ]
            :> JsonNode
        | ResolvedValueShape.Array item ->
            JsonObject
                [
                    KeyValuePair("kind", JsonValue.Create "array" :> JsonNode)
                    KeyValuePair("item", writeShape item)
                ]
            :> JsonNode
        | ResolvedValueShape.Union arms ->
            JsonObject
                [
                    KeyValuePair("kind", JsonValue.Create "union" :> JsonNode)
                    KeyValuePair("arms", JsonArray(arms |> List.map writeShape |> List.toArray) :> JsonNode)
                ]
            :> JsonNode
        | ResolvedValueShape.Record fields ->
            let fields =
                fields
                |> List.map (fun field ->
                    JsonObject
                        [
                            KeyValuePair("name", JsonValue.Create field.Name :> JsonNode)
                            KeyValuePair("optional", JsonValue.Create field.Optional :> JsonNode)
                            KeyValuePair("shape", writeShape field.Shape)
                        ]
                    :> JsonNode)

            JsonObject
                [
                    KeyValuePair("kind", JsonValue.Create "record" :> JsonNode)
                    KeyValuePair("fields", JsonArray(List.toArray fields) :> JsonNode)
                ]
            :> JsonNode

    let rec readShape (node: JsonElement) =
        match node.GetProperty("kind").GetString() with
        | "string" -> ResolvedValueShape.String
        | "number" -> ResolvedValueShape.Number
        | "boolean" -> ResolvedValueShape.Boolean
        | "null" -> ResolvedValueShape.Null
        | "undefined" -> ResolvedValueShape.Undefined
        | "literal" -> ResolvedValueShape.StringLiteral(node.GetProperty("value").GetString())
        | "array" -> ResolvedValueShape.Array(readShape (node.GetProperty "item"))
        | "union" ->
            ResolvedValueShape.Union(node.GetProperty("arms").EnumerateArray() |> Seq.map readShape |> Seq.toList)
        | "record" ->
            ResolvedValueShape.Record(
                node.GetProperty("fields").EnumerateArray()
                |> Seq.map (fun field ->
                    {
                        Name = field.GetProperty("name").GetString()
                        Optional = field.GetProperty("optional").GetBoolean()
                        Shape = readShape (field.GetProperty "shape")
                    })
                |> Seq.toList
            )
        | _ -> invalidArg "input" "Unsupported operation shape"

    let private caseNames candidates =
        let rec allocate reserved =
            let names =
                (Set.toList reserved @ candidates)
                |> Shape.Spec.uniqueCaseNames
                |> List.skip reserved.Count

            let expanded =
                Set.union reserved (names |> List.map (fun name -> "Is" + name) |> Set.ofList)

            if reserved = expanded then
                names |> List.map Render.ident
            else
                allocate expanded

        allocate (Set.ofList [ "Tags"; "ToString" ])

    let render typesModule (selection: OperationSelection) (operation: ResolvedOperation) shape =
        validate selection

        if
            operation.ParameterIndex < 0
            || operation.ParameterIndex >= operation.ParameterNames.Length
            || operation.ParameterNames.Length <> operation.ParameterOptional.Length
        then
            invalidArg "operation" "Invalid operation parameter metadata"

        let declarations = ResizeArray<string>()
        let encoders = ResizeArray<string>()
        let names = HashSet<string>(StringComparer.Ordinal)
        let exported = ResizeArray<string>()

        let allocate candidate =
            let mutable name = candidate
            let mutable suffix = 2

            while not (names.Add name) do
                name <- candidate + string suffix
                suffix <- suffix + 1

            name

        let rootName = allocate selection.TypeName

        let rec lower named shape =
            match shape with
            | ResolvedValueShape.String -> "string", "stringValue"
            | ResolvedValueShape.Number -> "float", "box"
            | ResolvedValueShape.Boolean -> "bool", "box"
            | ResolvedValueShape.Array item ->
                let itemType, encode = lower (allocate (named + "Item")) item
                $"({itemType}) array", $"(fun values -> values |> Array.map {encode} |> box)"
            | ResolvedValueShape.Record fields ->
                let payloadFields =
                    fields
                    |> List.filter (fun field ->
                        match field.Shape, field.Optional with
                        | ResolvedValueShape.StringLiteral _, false -> false
                        | _ -> true)

                let fieldNames =
                    payloadFields
                    |> List.map (fun field -> Naming.enumCaseOfString field.Name)
                    |> caseNames

                let payloads =
                    List.zip payloadFields fieldNames
                    |> List.map (fun (field, fieldName) ->
                        let fieldType, encode = lower (allocate (named + fieldName.Trim('`'))) field.Shape

                        field,
                        fieldName,
                        (if field.Optional then
                             $"({fieldType}) option"
                         else
                             fieldType),
                        encode)

                let body =
                    if List.isEmpty payloads then
                        $"[<RequireQualifiedAccess>]\ntype {named} = Value"
                    else
                        let fields =
                            payloads
                            |> List.map (fun (_, fieldName, typ, _) -> $"        {fieldName}: {typ}")
                            |> String.concat "\n"

                        $"type {named} =\n    {{\n{fields}\n    }}"

                declarations.Add body
                exported.Add named

                let assigns =
                    fields
                    |> List.map (fun field ->
                        let property = Source.stringLiteral field.Name

                        match field.Shape, field.Optional with
                        | ResolvedValueShape.StringLiteral literal, false ->
                            $"    setField result {property} (box {Source.stringLiteral literal})"
                        | _ ->
                            let _, fieldName, _, encode =
                                payloads |> List.find (fun (candidate, _, _, _) -> candidate.Name = field.Name)

                            if field.Optional then
                                $"    match value.{fieldName} with\n    | None -> ()\n    | Some field -> setField result {property} ({encode} field)"
                            else
                                $"    setField result {property} ({encode} value.{fieldName})")
                    |> String.concat "\n"

                let encode = "encode" + named

                encoders.Add(
                    $"let private {encode} (value: {named}) : obj =\n    let result = newObject ()\n{assigns}\n    result"
                )

                named, encode
            | ResolvedValueShape.Union arms -> lowerUnion named arms
            | literal -> lowerUnion named [ literal ]

        and lowerUnion named arms =
            if List.isEmpty arms then
                invalidArg "shape" "An operation union must contain at least one arm"

            let candidate =
                function
                | ResolvedValueShape.StringLiteral value -> Naming.enumCaseOfString value
                | ResolvedValueShape.String -> "Text"
                | ResolvedValueShape.Number -> "Number"
                | ResolvedValueShape.Boolean -> "Boolean"
                | ResolvedValueShape.Null -> "Null"
                | ResolvedValueShape.Undefined -> "Undefined"
                | ResolvedValueShape.Array _ -> "Items"
                | ResolvedValueShape.Record fields ->
                    fields
                    |> List.tryPick (fun field ->
                        match field.Shape with
                        | ResolvedValueShape.StringLiteral value when
                            not field.Optional && List.contains field.Name [ "type"; "kind"; "role" ]
                            ->
                            Some(Naming.enumCaseOfString value)
                        | _ -> None)
                    |> Option.defaultValue "Record"
                | ResolvedValueShape.Union _ -> "Choice"

            let cases = List.zip arms (caseNames (List.map candidate arms))

            let lowered =
                cases
                |> List.map (fun (arm, case) ->
                    match arm with
                    | ResolvedValueShape.StringLiteral literal -> case, None, "box " + Source.stringLiteral literal
                    | ResolvedValueShape.Null -> case, None, "null"
                    | ResolvedValueShape.Undefined -> case, None, "undefinedValue ()"
                    | shape ->
                        let typ, encode = lower (allocate (named + case.Trim('`'))) shape
                        case, Some typ, encode + " value")

            let casesText =
                lowered
                |> List.map (fun (case, typ, _) ->
                    match typ with
                    | Some typ -> $"    | {case} of {typ}"
                    | None -> $"    | {case}")
                |> String.concat "\n"

            declarations.Add($"[<RequireQualifiedAccess>]\ntype {named} =\n{casesText}")
            exported.Add named
            let encode = "encode" + named

            let branches =
                lowered
                |> List.map (fun (case, typ, expression) ->
                    let payload = if typ.IsSome then " value" else ""
                    $"    | {named}.{case}{payload} -> {expression}")
                |> String.concat "\n"

            encoders.Add($"let private {encode} (value: {named}) : obj =\n    match value with\n{branches}")
            named, encode

        let rootType, encode = lower rootName shape

        if rootType <> rootName then
            declarations.Add($"type {rootName} = {rootType}")
            exported.Add rootName

        let arguments =
            operation.ParameterNames |> List.mapi (fun index _ -> $"argument{index}")

        let remaining =
            arguments
            |> List.indexed
            |> List.filter (fst >> (<>) operation.ParameterIndex)
            |> List.map snd

        let encoded =
            match operation.FieldName with
            | None -> $"{encode} value"
            | Some field ->
                $"match value with None -> newObject () | Some field -> fieldArgument {Source.stringLiteral field} ({encode} field)"

        let argumentType =
            if operation.FieldName.IsSome then
                rootName + " option"
            else
                rootName

        let call =
            List.zip3 operation.ParameterNames operation.ParameterOptional arguments
            |> List.mapi (fun index (name, optional, argument) ->
                if index = operation.ParameterIndex then
                    let expression = $"unbox ({encoded})"

                    if optional then
                        $"{Render.ident name} = {expression}"
                    else
                        expression
                elif optional then
                    $"?{Render.ident name} = {argument}"
                else
                    argument)
            |> String.concat ", "

        let remaining = String.concat " " remaining

        let source =
            [
                $"module {selection.ModuleName}"
                ""
                "open Fable.Core"
                ""
                (match typesModule with
                 | None -> String.concat "\n\n" declarations
                 | Some owner ->
                     exported
                     |> Seq.map (fun name -> $"type {name} = {owner}.{name}")
                     |> String.concat "\n")
                ""
                "[<Emit(\"typeof $0 === 'string'\")>]"
                "let private isString (_: string) : bool = jsNative"
                ""
                "let private stringValue (value: string) : obj ="
                "    if isString value then box value"
                "    else invalidArg \"value\" \"The operation requires a JavaScript string\""
                ""
                "[<Emit(\"undefined\")>]"
                "let private undefinedValue () : obj = jsNative"
                ""
                "[<Emit(\"({})\")>]"
                "let private newObject () : obj = jsNative"
                ""
                "[<Emit(\"Object.defineProperty($0, $1, {value: $2, enumerable: true, writable: true, configurable: true})\")>]"
                "let private setField (_: obj) (_: string) (_: obj) : unit = jsNative"
                ""
                "[<Emit(\"({[$0]: $1})\")>]"
                "let private fieldArgument (_: string) (_: obj) : obj = jsNative"
                ""
                String.concat "\n\n" encoders
                ""
                $"let {selection.FunctionName} (receiver: {selection.ReceiverType}) (value: {argumentType}) {remaining} ="
                $"    receiver.{Render.ident operation.MethodName}({call})"
                ""
            ]
            |> String.concat "\n"

        source,
        exported
        |> Seq.map (fun name -> selection.ModuleName + "." + name)
        |> Seq.toList

    let json (typesModule: string option) (selection: OperationSelection) (operation: ResolvedOperation) shape =
        let node = JsonObject()
        node["formatVersion"] <- JsonValue.Create 1

        node["typesModule"] <-
            match typesModule with
            | Some value -> JsonValue.Create value
            | None -> null

        node["selection"] <-
            JsonSerializer.SerializeToNode
                {|
                    Package = selection.Package
                    ReceiverPath = selection.ReceiverPath
                    MethodName = selection.MethodName
                    ParameterName = selection.ParameterName
                    ReceiverType = selection.ReceiverType
                    ModuleName = selection.ModuleName
                    TypeName = selection.TypeName
                    FunctionName = selection.FunctionName
                |}

        node["fieldName"] <-
            match selection.FieldName with
            | Some value -> JsonValue.Create value
            | None -> null

        let op = JsonObject()
        op["methodName"] <- JsonValue.Create operation.MethodName
        op["parameterNames"] <- JsonSerializer.SerializeToNode operation.ParameterNames
        op["parameterOptional"] <- JsonSerializer.SerializeToNode operation.ParameterOptional
        op["parameterIndex"] <- JsonValue.Create operation.ParameterIndex
        node["operation"] <- op
        node["shape"] <- writeShape shape
        node.ToJsonString()

    let read input =
        use document = JsonDocument.Parse(File.ReadAllText input)
        let root = document.RootElement

        if root.GetProperty("formatVersion").GetInt32() <> 1 then
            invalidArg "input" "Unsupported operation input version"

        let raw = root.GetProperty "selection"

        let text name =
            raw.GetProperty(name: string).GetString()

        let field =
            match root.GetProperty "fieldName" with
            | value when value.ValueKind = JsonValueKind.Null -> None
            | value -> Some(value.GetString())

        let selection =
            {
                Package = text "Package"
                ReceiverPath =
                    raw.GetProperty("ReceiverPath").EnumerateArray()
                    |> Seq.map _.GetString()
                    |> Seq.toList
                MethodName = text "MethodName"
                ParameterName = text "ParameterName"
                FieldName = field
                ReceiverType = text "ReceiverType"
                ModuleName = text "ModuleName"
                TypeName = text "TypeName"
                FunctionName = text "FunctionName"
            }

        let op = root.GetProperty "operation"

        let operation =
            {
                MethodName = op.GetProperty("methodName").GetString()
                ParameterNames =
                    op.GetProperty("parameterNames").EnumerateArray()
                    |> Seq.map _.GetString()
                    |> Seq.toList
                ParameterOptional =
                    op.GetProperty("parameterOptional").EnumerateArray()
                    |> Seq.map _.GetBoolean()
                    |> Seq.toList
                ParameterIndex = op.GetProperty("parameterIndex").GetInt32()
                FieldName = field
            }

        let typesModule =
            match root.GetProperty "typesModule" with
            | value when value.ValueKind = JsonValueKind.Null -> None
            | value -> Some(value.GetString())

        render typesModule selection operation (readShape (root.GetProperty "shape"))
        |> fst

/// Emits input contracts and the operations that encode them at the JavaScript boundary.
[<MyriadGenerator("xantham-operations")>]
type OperationGenerator() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".json" ]

        member _.Generate context =
            Output.Source(OperationSource.read context.InputFilename)

module Operations =
    let private createCore
        shared
        (identity: ExtensionIdentity)
        workspace
        (selections: OperationSelection list)
        : ProjectionExtension =
        let key = "xantham.myriad.operations"

        if Map.containsKey key identity.Configuration then
            invalidArg "identity" $"Configuration key '{key}' is reserved for the adapter"

        selections |> List.iter OperationSource.validate

        let configuration =
            selections
            |> List.map (fun selection ->
                {|
                    package = selection.Package
                    receiver = selection.ReceiverPath
                    methodName = selection.MethodName
                    parameter = selection.ParameterName
                    field = selection.FieldName |> Option.toList
                    receiverType = selection.ReceiverType
                    moduleName = selection.ModuleName
                    typeName = selection.TypeName
                    functionName = selection.FunctionName
                |})
            |> fun selections ->
                JsonSerializer.Serialize
                    {|
                        sharedTypes = shared
                        selections = selections
                    |}

        {
            Identity =
                { identity with
                    Configuration = Map.add key configuration identity.Configuration
                }
            Transform =
                fun snapshot ->
                    let mutable sharedOwner = None

                    let results =
                        selections
                        |> List.map (fun selection ->
                            let source =
                                match selection.FieldName with
                                | None ->
                                    Resolved.tryFindParameter
                                        selection.Package
                                        selection.ReceiverPath
                                        selection.MethodName
                                        selection.ParameterName
                                        snapshot
                                | Some field ->
                                    Resolved.tryFindParameterField
                                        selection.Package
                                        selection.ReceiverPath
                                        selection.MethodName
                                        selection.ParameterName
                                        field
                                        snapshot

                            match source with
                            | None ->
                                Error
                                    [
                                        Diagnostic.create
                                            "myriad/missing-operation"
                                            $"The selected method argument {selection.MethodName}/{selection.ParameterName} was not resolved"
                                            None
                                    ]
                            | Some source ->
                                match Resolved.shape source snapshot, Resolved.operation source snapshot with
                                | Ok shape, Ok operation ->
                                    let owner =
                                        if not shared then
                                            Ok None
                                        else
                                            match sharedOwner with
                                            | None ->
                                                sharedOwner <- Some(shape, selection.TypeName, selection.ModuleName)
                                                Ok None
                                            | Some(expected, typeName, owner) when
                                                expected = shape && typeName = selection.TypeName
                                                ->
                                                Ok(Some owner)
                                            | Some _ ->
                                                Error
                                                    [
                                                        Diagnostic.create
                                                            "myriad/shared-shape-mismatch"
                                                            "Shared operation contracts require identical resolved value shapes and type names"
                                                            (Some(Resolved.identity source snapshot))
                                                    ]

                                    match owner with
                                    | Error diagnostics -> Error diagnostics
                                    | Ok owner ->
                                        let _, exports = OperationSource.render owner selection operation shape

                                        ProjectionRunner.generate
                                            (Some selection.ReceiverType)
                                            workspace
                                            (OperationGenerator() :> IMyriadGenerator)
                                            (OperationSource.json owner selection operation shape)
                                            source
                                            selection.ModuleName
                                            exports
                                            snapshot
                                | Error diagnostics, _
                                | _, Error diagnostics -> Error diagnostics)

                    let errors =
                        results
                        |> List.collect (function
                            | Error diagnostics -> diagnostics
                            | Ok _ -> [])

                    if List.isEmpty errors then
                        Ok(
                            results
                            |> List.choose (function
                                | Ok result -> Some result
                                | Error _ -> None)
                        )
                    else
                        Error errors
        }

    /// Generates each selected operation with its own input contract.
    let create identity workspace selections =
        createCore false identity workspace selections

    /// Uses the first selection's input contract across operations with identical resolved shapes.
    let createShared identity workspace selections =
        createCore true identity workspace selections
