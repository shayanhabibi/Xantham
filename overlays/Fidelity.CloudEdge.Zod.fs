// ─────────────────────────────────────────────────────────────────────────────
// HAND-SHAPED OVERLAY (recipe: [[entry]] zod, policy = opaque-handle).
// Replaces the generated Zod machinery module at emission — this FRAGMENT is
// appended at namespace level inside the Zod unit file; it must not declare a
// namespace of its own. Developer-owned (the Farscape Overlay discipline):
// regeneration never touches it.
//
// Policy: zod is OPAQUE. F# code builds schemas through the builders below and
// passes them around as `ZodType` handles; none of zod's ~13k-line generic
// machinery (ZodObject<'Shape,'Config>, internals, v3/v4 twins) is emitted.
// The MCP SDK's zod-compat sniffs the runtime `_zod` marker and routes these
// v4-classic values down its v4 arm, so the one handle covers the SDK's
// AnySchema / AnyObjectSchema / ZodRawShapeCompat union.
// ─────────────────────────────────────────────────────────────────────────────
module Zod =

    open Fable.Core

    /// Opaque handle to a zod schema value (v4 classic, runtime import "zod").
    [<AllowNullLiteral>]
    type ZodType =
        /// z.string().describe("...") — descriptions flow into the JSON Schema
        /// the MCP SDK advertises for a tool; keep them on the handle.
        abstract describe: description: string -> ZodType
        /// Postfix optionality: z.string().optional()
        abstract optional: unit -> ZodType
        abstract nullable: unit -> ZodType
        /// `default` is reserved in F#; emit the raw member call.
        [<Emit("$0.default($1)")>]
        abstract withDefault: value: obj -> ZodType

    /// Phantom arity aliases: generated references that survive substitution as
    /// generic applications (`Zod.ZodType<Shape, Mode>` from `ZodObject<S, $strip>`
    /// instantiations) collapse onto the same handle. The parameters carry no
    /// meaning — the schema is opaque by policy.
    type ZodType<'A> = ZodType
    type ZodType<'A, 'B> = ZodType
    type ZodType<'A, 'B, 'C> = ZodType

    /// ZodRawShapeCompat = Record<string, AnySchema> — the tool-authoring shape
    /// (`registerTool` inputSchema/outputSchema). MUST be a plain JS object:
    /// the SDK enumerates it with Object.keys/values. Fable's Dictionary is a
    /// class instance, not a POJO, so the honest encoding is `obj` built with
    /// the `shape` helper below (createObj yields a true POJO).
    type ZodRawShape = obj

    /// Build a raw shape for registerTool: `Zod.shape [ "city", Zod.string() ]`.
    let inline shape (fields: (string * ZodType) list) : ZodRawShape =
        JsInterop.createObj (fields |> List.map (fun (k, v) -> k, box v))

    /// The builder set (recipe `builders` key). Selective named imports from the
    /// zod package root — the same specifier the MCP SDK imports at runtime, so
    /// the Workers bundle already carries it.
    [<Import("string", "zod")>]
    let string () : ZodType = jsNative

    [<Import("number", "zod")>]
    let number () : ZodType = jsNative

    [<Import("boolean", "zod")>]
    let boolean () : ZodType = jsNative

    [<Import("object", "zod")>]
    let object (shape: ZodRawShape) : ZodType = jsNative

    [<Import("literal", "zod")>]
    let literal (value: obj) : ZodType = jsNative

    /// z.enum([...]) — `enum` is reserved in F#.
    [<Import("enum", "zod")>]
    let enumOf (values: string array) : ZodType = jsNative

    [<Import("array", "zod")>]
    let array (element: ZodType) : ZodType = jsNative

    [<Import("optional", "zod")>]
    let optional (inner: ZodType) : ZodType = jsNative

    [<Import("union", "zod")>]
    let union (options: ZodType array) : ZodType = jsNative

    [<Import("discriminatedUnion", "zod")>]
    let discriminatedUnion (discriminator: string) (options: ZodType array) : ZodType = jsNative
