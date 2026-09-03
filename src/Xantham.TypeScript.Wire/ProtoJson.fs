namespace Xantham.TypeScript.Wire

open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open Xantham.TypeScript.Wire.Proto

/// `System.Text.Json` has no native understanding of `voption`, and the generated records use it
/// for every field the schema marks optional. Reading is the easy half; the writing half matters
/// more, because the server distinguishes an absent field from an explicit null - `ValueNone` must
/// vanish from the payload rather than serialise as `null`. That omission is what the generated
/// `JsonIgnoreCondition.WhenWritingDefault` attributes buy: `ValueNone` is the default value of the
/// struct, so the property is skipped before this converter is ever asked to write it.
type ValueOptionConverter<'T>() =
    inherit JsonConverter<'T voption>()

    override _.Read(reader, _, options) =
        match reader.TokenType with
        | JsonTokenType.Null -> ValueNone
        | _ -> JsonSerializer.Deserialize<'T>(&reader, options) |> ValueSome

    override _.Write(writer, value, options) =
        match value with
        | ValueNone -> writer.WriteNullValue()
        | ValueSome v -> JsonSerializer.Serialize(writer, v, options)

type ValueOptionConverterFactory() =
    inherit JsonConverterFactory()

    override _.CanConvert(t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<ValueOption<_>>

    override _.CreateConverter(t: Type, _) =
        typedefof<ValueOptionConverter<_>>.MakeGenericType(t.GetGenericArguments())
        |> Activator.CreateInstance
        :?> JsonConverter

/// `DocumentIdentifier` is `string | { uri: string }` on the wire - the schema's one structural
/// union - so neither arm can be inferred from the F# union's shape.
type DocumentIdentifierConverter() =
    inherit JsonConverter<DocumentIdentifier>()

    override _.Read(reader, _, _) =
        match reader.TokenType with
        | JsonTokenType.String -> FileName(reader.GetString())
        | JsonTokenType.StartObject ->
            use document = JsonDocument.ParseValue &reader

            match document.RootElement.TryGetProperty "uri" with
            | true, value -> Uri(value.GetString())
            | _ -> failwith "DocumentIdentifier object had no 'uri' property"
        | other -> failwithf $"DocumentIdentifier expected a string or an object, got %A{other}"

    override _.Write(writer, value, _) =
        match value with
        | FileName path -> writer.WriteStringValue path
        | Uri uri ->
            writer.WriteStartObject()
            writer.WriteString("uri", uri)
            writer.WriteEndObject()

/// Serialisation for the generated wire types, and the typed request helpers built on it.
[<RequireQualifiedAccess>]
module ProtoJson =

    /// The only sanctioned options for wire types. The generated records carry explicit
    /// `JsonPropertyName` attributes rather than relying on a naming policy, so the wire names
    /// are pinned even where they are not a plain camel-casing of the F# name.
    let options =
        let options = JsonSerializerOptions(JsonSerializerDefaults.General)
        options.Converters.Add(ValueOptionConverterFactory())
        options.Converters.Add(DocumentIdentifierConverter())
        options.DefaultIgnoreCondition <- JsonIgnoreCondition.Never
        options

    let serialize (value: 'T) =
        JsonSerializer.SerializeToUtf8Bytes(value, options)

    let deserialize<'T> (bytes: byte[]) =
        JsonSerializer.Deserialize<'T>(ReadOnlySpan bytes, options)

    /// An empty payload is the server's `undefined`; a literal `null` is its `null`. Both mean
    /// "no result" to a caller, and both must be caught before `Deserialize` sees them - asking
    /// for a record and getting null back yields a null record rather than an exception.
    let isAbsent (bytes: byte[]) =
        bytes.Length = 0
        || (bytes.Length = 4
            && bytes[0] = 110uy
            && bytes[1] = 117uy
            && bytes[2] = 108uy
            && bytes[3] = 108uy)

    /// Encodes any parameter record for use as `BatchRequest.Params`.
    ///
    /// The field is raw UTF-8 JSON, so a batch entry carries byte-for-byte what the single-shot
    /// call would have sent - there is no second encoding to keep in step, and every parameter
    /// type works here without the generator knowing anything about batching.
    let batchEntry (method: string) (parameters: 'Params) : BatchRequest =
        {
            Method = method
            Params = serialize parameters
        }

    /// A batch entry for one of the methods the schema types as taking no parameters. The payload
    /// is left absent rather than sent as a literal null, which is what those methods expect
    /// inside a batch.
    let batchEntryNoParams (method: string) : BatchRequest = { Method = method; Params = null }

    /// A request whose result the schema permits to be null.
    let requestOption<'Params, 'Result> (channel: TscChannel) (method: string) (parameters: 'Params) =
        let response = channel.Request(method, serialize parameters)

        if isAbsent response then
            ValueNone
        else
            ValueSome(deserialize<'Result> response)

    /// A request whose result the schema says is always present.
    let request<'Params, 'Result> (channel: TscChannel) (method: string) (parameters: 'Params) =
        match requestOption<'Params, 'Result> channel method parameters with
        | ValueSome result -> result
        | ValueNone -> failwithf "%s returned no result, but the schema declares one" method

    /// A request that returns nothing.
    let requestUnit<'Params> (channel: TscChannel) (method: string) (parameters: 'Params) =
        channel.Request(method, serialize parameters) |> ignore

    /// A request taking no parameters. The schema types these as `null`, and the server rejects
    /// an empty payload, so the four bytes are required.
    let requestNoParams<'Result> (channel: TscChannel) (method: string) =
        let response = channel.Request(method, "null")

        if isAbsent response then
            ValueNone
        else
            ValueSome(deserialize<'Result> response)

    /// The four AST-returning methods.
    ///
    /// The schema types their result as `SourceFileResponse = { data: string }`, a base64 blob in
    /// a JSON envelope. That describes the compiler's *async* client, which speaks JSON-RPC and so
    /// has no way to carry raw bytes - it base64s them into an envelope and decodes on arrival.
    /// This is the sync client: the msgpack transport carries bytes natively, so the AST arrives
    /// raw, with no envelope and no base64, and goes straight to the decoder.
    let requestAst<'Params> (channel: TscChannel) (method: string) (parameters: 'Params) =
        match channel.Request(method, serialize parameters) with
        | [||] -> ValueNone
        | bytes -> ValueSome(Ast.read bytes)
