/// The JSON Schema for `xantham.json` (decision O4), emitted from `GeneratorConfig`,
/// `GroupDisposition` and `MappedName` by reflection. The property set, their JSON types and
/// the disposition vocabulary are read from the F# declarations; the key spellings and the
/// descriptions are read from the tables here, which carry an entry for every member.
module Xantham.Cli.Schema

open System
open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open FSharp.Reflection
open Xantham.Generator

[<Literal>]
let private Draft = "https://json-schema.org/draft/2020-12/schema"

[<Literal>]
let private Id =
    "https://raw.githubusercontent.com/shayanhabibi/Xantham/master/xantham.schema.json"

/// The JSON key and description of one `GeneratorConfig` field.
let private configKeys =
    Map.ofList
        [
            "ModuleName",
            ("module",
             "The F# module the binding is written into. Defaults to the package name under the O7 naming \
          contract, where `@scope/pkg-name` becomes `Scope.PkgName`.")

            "Namespace",
            ("namespace",
             "The F# namespace a package family is written under. The entry package takes it, and each \
          group named under `groups` takes `<namespace>.<Leaf>`, so `@cloudedge/agents` under \
          `FSharp.CloudEdge` reads `FSharp.CloudEdge.Agents`. Both sides of a reference configure the \
          same namespace.")

            "Groups",
            ("groups",
             "What the generator does with each package boundary its declarations reach (decision O7), keyed \
          by npm name, with the compiler's own library as `typescript/lib`. An unlisted group widens.")

            "Lib",
            ("lib",
             "The compiler's `lib` option, as `tsconfig.json` spells it. Omitted, the compiler's default \
          applies, which includes the DOM. A global type library that redeclares DOM names sets this to \
          what its README prescribes.")

            "RuntimePackage",
            ("runtime",
             "The npm package the generated `[<Import(…)>]` attributes name. Defaults to the package name \
          with DefinitelyTyped's `@types/` convention undone, so `@types/three` imports from `three`.")
        ]

/// The JSON key, description and requiredness of one `MappedName` field.
let private mappedKeys =
    Map.ofList
        [
            "FSharpName",
            ("name", true, "The F# name a reference renders as, qualified as the destination package spells it.")

            "Arity",
            ("arity",
             false,
             "The number of type arguments the destination takes. A reference applying any other number widens with finding TR053.")
        ]

let private unwrapOption (t: Type) =
    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
        t.GetGenericArguments()[0]
    else
        t

let private isList (t: Type) (element: Type) =
    t.IsGenericType
    && t.GetGenericTypeDefinition() = typedefof<_ list>
    && t.GetGenericArguments()[0] = element

let private isMapOf (t: Type) (value: Type) =
    t.IsGenericType
    && t.GetGenericTypeDefinition() = typedefof<Map<_, _>>
    && t.GetGenericArguments() = [| typeof<string>; value |]

/// The disposition names spelled as `xantham.json` takes them: every nullary case of
/// `GroupDisposition`, lower-cased. A case carrying a payload takes a JSON form of its own.
let private plainDispositions =
    FSharpType.GetUnionCases typeof<GroupDisposition>
    |> Array.filter (fun case -> case.GetFields().Length = 0)
    |> Array.map (fun case -> case.Name.ToLowerInvariant())
    |> Array.toList

/// The one disposition carrying a payload. Its JSON form is `{ "map": { ... } }`, and the
/// payload is the destination table.
let private mappedDisposition =
    match
        FSharpType.GetUnionCases typeof<GroupDisposition>
        |> Array.filter (fun case -> case.GetFields().Length > 0)
        |> Array.toList
    with
    | [ case ] when case.Name = "Map" ->
        match case.GetFields() with
        | [| field |] when isMapOf field.PropertyType typeof<MappedName> -> case.Name.ToLowerInvariant()
        | _ ->
            failwith
                "GroupDisposition.Map no longer carries one Map<string, MappedName>; Schema.fs describes the old shape"
    | cases ->
        let names = cases |> List.map _.Name |> String.concat ", "

        failwith $"GroupDisposition carries a payload on [{names}]; Schema.fs writes a JSON form for Map alone"

let private writeDescribed (w: Utf8JsonWriter) (description: string) (body: Utf8JsonWriter -> unit) =
    w.WriteStartObject()
    body w
    w.WriteString("description", description)
    w.WriteEndObject()

/// The schema fragment for one F# type. `option` is unwrapped: every key of `xantham.json` is
/// optional.
let private writeFieldType (w: Utf8JsonWriter) (name: string) (t: Type) =
    match unwrapOption t with
    | t when t = typeof<string> -> w.WriteString("type", "string")
    | t when t = typeof<int> ->
        w.WriteString("type", "integer")
        w.WriteNumber("minimum", 0)
    | t when isList t typeof<string> ->
        w.WriteString("type", "array")
        w.WriteStartObject "items"
        w.WriteString("type", "string")
        w.WriteEndObject()
    | t when isMapOf t typeof<GroupDisposition> ->
        w.WriteString("type", "object")
        w.WriteStartObject "additionalProperties"
        w.WriteString("$ref", "#/$defs/disposition")
        w.WriteEndObject()
    | t -> failwith $"xantham.json: {name} has type {t.FullName}, which Schema.fs writes no JSON form for"

let private writeConfigProperties (w: Utf8JsonWriter) =
    for field in FSharpType.GetRecordFields typeof<GeneratorConfig> do
        match Map.tryFind field.Name configKeys with
        | None ->
            failwith
                $"GeneratorConfig.{field.Name} needs an entry in Schema.fs's key table before the schema \
                  can describe it"
        | Some(key, description) ->
            w.WritePropertyName key
            writeDescribed w description (fun w -> writeFieldType w field.Name field.PropertyType)

let private writeMappedProperties (w: Utf8JsonWriter) =
    for field in FSharpType.GetRecordFields typeof<MappedName> do
        match Map.tryFind field.Name mappedKeys with
        | None -> failwith $"MappedName.{field.Name} is absent from Schema.fs's key table"
        | Some(key, _, description) ->
            w.WritePropertyName key
            writeDescribed w description (fun w -> writeFieldType w field.Name field.PropertyType)

let private writeMappedName (w: Utf8JsonWriter) =
    w.WriteStartObject()

    w.WriteString(
        "description",
        "Where one TypeScript name is redirected to. A bare string is a destination taking no type \
         arguments; the object form states the arity."
    )

    w.WriteStartArray "oneOf"

    w.WriteStartObject()
    w.WriteString("type", "string")
    w.WriteEndObject()

    w.WriteStartObject()
    w.WriteString("type", "object")
    w.WriteBoolean("additionalProperties", false)

    w.WriteStartArray "required"

    for field in FSharpType.GetRecordFields typeof<MappedName> do
        match Map.tryFind field.Name mappedKeys with
        | Some(key, true, _) -> w.WriteStringValue key
        | _ -> ()

    w.WriteEndArray()

    w.WriteStartObject "properties"
    writeMappedProperties w
    w.WriteEndObject()

    w.WriteEndObject()

    w.WriteEndArray()
    w.WriteEndObject()

let private writeDisposition (w: Utf8JsonWriter) =
    w.WriteStartObject()

    w.WriteString(
        "description",
        "One group's disposition (decision O7): how deep resolution follows into the group, and what a \
         reference to its types renders as."
    )

    w.WriteStartArray "oneOf"

    w.WriteStartObject()
    w.WriteString("type", "string")
    w.WriteStartArray "enum"

    for name in plainDispositions do
        w.WriteStringValue name

    w.WriteEndArray()
    w.WriteEndObject()

    w.WriteStartObject()
    w.WriteString("type", "object")
    w.WriteBoolean("additionalProperties", false)
    w.WriteStartArray "required"
    w.WriteStringValue mappedDisposition
    w.WriteEndArray()
    w.WriteStartObject "properties"
    w.WritePropertyName mappedDisposition

    w.WriteStartObject()
    w.WriteString("type", "object")

    w.WriteString(
        "description",
        "The destination of each redirected name, keyed by the TypeScript name the group declares. A \
         name outside the table widens."
    )

    w.WritePropertyName "additionalProperties"
    writeMappedName w
    w.WriteEndObject()

    w.WriteEndObject()
    w.WriteEndObject()

    w.WriteEndArray()
    w.WriteEndObject()

/// The whole schema: the text committed at the repository root, and what `xantham schema`
/// writes. LF and two-space indentation on every platform.
let json () : string =
    use stream = new MemoryStream()

    let options =
        JsonWriterOptions(
            Indented = true,
            IndentCharacter = ' ',
            IndentSize = 2,
            NewLine = "\n",
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        )

    use writer = new Utf8JsonWriter(stream, options)

    writer.WriteStartObject()
    writer.WriteString("$schema", Draft)
    writer.WriteString("$id", Id)
    writer.WriteString("title", "xantham.json")

    writer.WriteString(
        "description",
        "Per-package configuration for the Xantham TypeScript-to-F# bindings generator, read from the \
         file beside the package manifest. Comments and trailing commas are accepted."
    )

    writer.WriteString("type", "object")
    writer.WriteBoolean("additionalProperties", false)

    writer.WriteStartObject "properties"
    writeConfigProperties writer
    writer.WriteEndObject()

    writer.WriteStartObject "$defs"
    writer.WritePropertyName "disposition"
    writeDisposition writer
    writer.WriteEndObject()

    writer.WriteEndObject()
    writer.Flush()

    Encoding.UTF8.GetString(stream.ToArray()) + "\n"
