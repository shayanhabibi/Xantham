module internal Xantham.Generator.CatalogCompatibility

open System
open System.Text.Json
open System.Text.Json.Nodes

type CompilerIdentity =
    | TypeScriptPackage of version: string * revision: string * astProtocolVersion: uint32
    | Binary of astProtocolVersion: uint32

type Contract =
    {
        ContractVersion: int
        IdentityVersion: int
        ApiVersion: int
        InferenceVersion: int
        CustomizationVersion: int
        Compiler: CompilerIdentity
    }

type Producer =
    {
        Compiler: string
        Generator: string
        InferenceProfile: string
        Contract: Contract
    }

let current compiler =
    {
        ContractVersion = 1
        IdentityVersion = 3
        ApiVersion = 3
        InferenceVersion = 1
        CustomizationVersion = 1
        Compiler = compiler
    }

let private fail path message =
    failwith $"declaration catalog: {path}: {message}"

let private objectFields path name (element: JsonElement) =
    if element.ValueKind <> JsonValueKind.Object then
        fail path $"{name} must be an object"

    element.EnumerateObject()
    |> Seq.fold
        (fun fields property ->
            if Map.containsKey property.Name fields then
                fail path $"duplicate {name}.{property.Name}"

            Map.add property.Name property.Value fields)
        Map.empty

let private field path name (fields: Map<string, JsonElement>) =
    match Map.tryFind name fields with
    | Some value -> value
    | None -> fail path $"missing compatibility field {name}"

let private positiveInt path name fields =
    let value = field path name fields

    match value.ValueKind with
    | JsonValueKind.Number ->
        match value.TryGetInt32() with
        | true, number when number > 0 -> number
        | _ -> fail path $"{name} must be a positive integer"
    | _ -> fail path $"{name} must be a positive integer"

let private nonemptyString path name fields =
    let value = field path name fields

    match value.ValueKind with
    | JsonValueKind.String ->
        let text = value.GetString()

        if String.IsNullOrWhiteSpace text || text <> text.Trim() then
            fail path $"{name} must be a nonempty string without surrounding whitespace"

        text
    | _ -> fail path $"{name} must be a string"

let validRevision (value: string) =
    not (isNull value) && value.Length = 40 && Seq.forall Char.IsAsciiHexDigit value

let schema path root =
    let fields = objectFields path "catalog schema" root
    positiveInt path "schemaVersion" fields

let read path root =
    match schema path root with
    | 1 -> None
    | 2 ->
        let rootFields = objectFields path "catalog" root

        let fields =
            field path "compatibility" rootFields |> objectFields path "compatibility"

        let compiler =
            field path "compiler" fields |> objectFields path "compatibility.compiler"

        let protocol = field path "astProtocolVersion" compiler

        let protocol =
            if protocol.ValueKind <> JsonValueKind.Number then
                fail path "astProtocolVersion must be a positive integer"

            match protocol.TryGetUInt32() with
            | true, value when value > 0u -> value
            | _ -> fail path "astProtocolVersion must be a positive integer"

        let identity =
            match nonemptyString path "kind" compiler with
            | "binary" -> Binary protocol
            | "typescript-package" ->
                let version = nonemptyString path "version" compiler
                let revision = nonemptyString path "revision" compiler

                if not (validRevision revision) then
                    fail path "compiler revision must be a full hexadecimal gitHead"

                TypeScriptPackage(version, revision.ToLowerInvariant(), protocol)
            | kind -> fail path $"unsupported compiler identity kind {kind}"

        Some
            {
                ContractVersion = positiveInt path "contractVersion" fields
                IdentityVersion = positiveInt path "identityVersion" fields
                ApiVersion = positiveInt path "apiVersion" fields
                InferenceVersion = positiveInt path "inferenceVersion" fields
                CustomizationVersion = positiveInt path "customizationVersion" fields
                Compiler = identity
            }
    | version -> fail path $"unsupported schema {version}; upgrade Xantham or regenerate the producer"

let write (contract: Contract) : JsonNode =
    let compiler =
        match contract.Compiler with
        | Binary protocol ->
            JsonSerializer.SerializeToNode(
                {|
                    kind = "binary"
                    astProtocolVersion = protocol
                |}
            )
        | TypeScriptPackage(version, revision, protocol) ->
            JsonSerializer.SerializeToNode(
                {|
                    kind = "typescript-package"
                    version = version
                    revision = revision.ToLowerInvariant()
                    astProtocolVersion = protocol
                |}
            )

    let result =
        JsonSerializer.SerializeToNode(
            {|
                contractVersion = contract.ContractVersion
                identityVersion = contract.IdentityVersion
                apiVersion = contract.ApiVersion
                inferenceVersion = contract.InferenceVersion
                customizationVersion = contract.CustomizationVersion
            |}
        )

    result["compiler"] <- compiler
    result

let validate path schema (expected: Producer) compilerHash generatorHash inferenceProfile actual =
    let equal name expected actual =
        if actual <> expected then
            fail
                path
                $"uses a different {name}: expected {expected}, actual {actual}; regenerate with a compatible toolchain"

    match schema, actual with
    | 1, _ ->
        equal "compiler" expected.Compiler compilerHash
        equal "generator" expected.Generator generatorHash
    | 2, Some contract ->
        let expectedContract = expected.Contract
        equal "contract version" (string expectedContract.ContractVersion) (string contract.ContractVersion)
        equal "identity version" (string expectedContract.IdentityVersion) (string contract.IdentityVersion)
        equal "API version" (string expectedContract.ApiVersion) (string contract.ApiVersion)
        equal "inference version" (string expectedContract.InferenceVersion) (string contract.InferenceVersion)

        equal
            "customization version"
            (string expectedContract.CustomizationVersion)
            (string contract.CustomizationVersion)

        match expectedContract.Compiler, contract.Compiler with
        | Binary expectedProtocol, Binary actualProtocol ->
            equal "AST protocol" (string expectedProtocol) (string actualProtocol)
            equal "compiler fingerprint" expected.Compiler compilerHash
        | TypeScriptPackage(expectedVersion, expectedRevision, expectedProtocol),
          TypeScriptPackage(actualVersion, actualRevision, actualProtocol) ->
            equal "compiler version" expectedVersion actualVersion
            equal "compiler revision" expectedRevision actualRevision
            equal "AST protocol" (string expectedProtocol) (string actualProtocol)
        | expectedKind, actualKind ->
            let kind =
                function
                | Binary _ -> "binary"
                | TypeScriptPackage _ -> "typescript-package"

            equal "compiler identity kind" (kind expectedKind) (kind actualKind)
    | 2, None -> fail path "missing compatibility metadata"
    | version, _ -> fail path $"unsupported schema {version}"

    equal "inference profile" expected.InferenceProfile inferenceProfile
