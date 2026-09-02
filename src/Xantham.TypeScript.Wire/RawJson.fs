namespace Xantham.TypeScript.Wire

open System
open System.Buffers
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization

/// <summary>
/// For serialized/decode passthrough of bytes for batch requests without any DOM parsing.
/// </summary>
type RawJsonConverter() =
    inherit JsonConverter<byte[]>()

    // One buffer and one writer per thread
    // The pool is in practice always warm and never contended.
    [<ThreadStatic; DefaultValue>]
    static val mutable private pooledBuffer: ArrayBufferWriter<byte>

    [<ThreadStatic; DefaultValue>]
    static val mutable private pooledWriter: Utf8JsonWriter

    // A `null` in the payload never reaches this: `HandleNull` is left false, so the field is set
    // to a null `byte[]` without the converter being asked. Callers check for that, not for the
    // four bytes of a literal `null`.
    override _.Read(reader, _, _) =
        // Parsed before the try so that the byref reader is not touched inside it.
        use document = JsonDocument.ParseValue &reader

        if isNull RawJsonConverter.pooledBuffer then
            RawJsonConverter.pooledBuffer <- ArrayBufferWriter<byte>(1024)

            // "Unsafe" here means only that the output is not escaped for embedding in HTML,
            // which it never is: these bytes go straight back to `ProtoJson.deserialize`. The
            // default encoder would escape `+` and `/` to six bytes each, which on the base64
            // an AST result carries inside a batch is most of the payload.
            RawJsonConverter.pooledWriter <-
                new Utf8JsonWriter(
                    RawJsonConverter.pooledBuffer,
                    JsonWriterOptions(Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
                )

        let buffer = RawJsonConverter.pooledBuffer
        let writer = RawJsonConverter.pooledWriter

        try
            document.RootElement.WriteTo writer
            writer.Flush()
            buffer.WrittenSpan.ToArray()
        finally
            // Both are reset even when the write threw, or the next field read on this thread
            // inherits a half-written value and a writer stopped mid-document.
            buffer.Clear()
            writer.Reset()

    // The bytes are written verbatim, without re-parsing to check they are well formed:
    // They are expected to come from `ProtoJson.serialize`, which cannot produce anything else.
    override _.Write(writer, value, _) =
        if isNull value || value.Length = 0 then
            writer.WriteNullValue()
        else
            writer.WriteRawValue(ReadOnlySpan value, skipInputValidation = true)
