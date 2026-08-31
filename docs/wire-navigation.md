# Navigating the wire AST

How to get from "I have a thing" to "I have the thing's text" in
`Xantham.TypeScript.Wire`. For *why* the format is shaped this way, see the AST section of
`AGENTS.md`; this file is only the map.

There are two worlds and they meet in exactly one place:

| | Where it lives | What a thing is |
|---|---|---|
| **Syntax** | the binary blob, decoded into `Node<'Tag>` | a tagged index into one file |
| **Semantics** | the compiler process, called through `Api` | an opaque handle string, resolved per request |

Symbols, types and signatures are *not* in the blob. They only exist behind a request. The
bridge between the two is the node handle, at the bottom of this file.

## Getting a blob

```fsharp
match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" } with
| ValueNone -> failwith "no such file in this project"
| ValueSome ast -> Node.root ast   // Node<SourceFile>
```

`ast` is an `Ast.SourceFile`: the bytes plus its section offsets. `Node.root` is the way in;
everything after it is typed.

## A node is a tagged index

`Node<'Tag>` is a two-field struct — the blob and an index — and the tag is erased at runtime.
The tags are generated from `ast.json`: one per node type (`Identifier`, `FunctionDeclaration`),
one per node alias (`Expression`, `Statement`, `TypeNode`), one per token
(`QuestionToken`, `AsteriskToken`), plus `AnyNode` for a slot the schema does not narrow.

```fsharp
node.Kind          // SyntaxKind
node.Pos           // start with leading trivia, UTF-16 code units
node.End
node.Text          // string voption, for the kinds that carry text
Node.parent node   // Node<AnyNode> voption
Node.children node // Node<AnyNode> seq
Node.descendants node
```

Positions are UTF-16 code units, so they index straight into `Ast.sourceText ast` with no
conversion.

Tags inherit each other exactly when one's kinds are a subset of the other's, so `Identifier`
*is* an `Expression` to the compiler:

```fsharp
let widened: Node<Expression> = Expression.ofNode identifier
```

`<Alias>.ofNode` exists once per alias. There is no single generic `widen`: F# rejects a
constraint whose right-hand side is a type variable.

## Narrowing: the views

```fsharp
open Xantham.TypeScript.Wire.Patterns
```

One partial active pattern per node type and per alias. They take a node of *any* tag and
narrow it by testing its kind:

```fsharp
match statement with
| FunctionDeclaration declaration -> FunctionDeclaration.name declaration
| VariableStatement statement -> ...
| _ -> ValueNone
```

They are `[<return: Struct>]`, so a match is a kind read and a two-word copy and allocates
nothing at all — measured at exactly 0 bytes over 100,000 matches. That is why they are patterns
rather than a discriminated union view, which would allocate a `Choice` per match.

`Patterns` is not auto-opened: a few hundred patterns in scope by default would shadow more than
they are worth.

## Example: a StringLiteral's text

```fsharp
let literals =
    Node.descendants (Node.root ast)
    |> Seq.choose (fun node ->
        match node with
        | StringLiteral literal -> StringLiteral.text literal |> ValueOption.toOption
        | _ -> None)
```

Text reaches a node two ways, and `text` covers both:

- **Identifiers and the like** spend their data word on an index into the string table.
- **Literals** spend it on an offset to an extended-data record, whose first word is that string
  index. The same record carries `rawText`, `tokenFlags` and `templateFlags`, and those
  accessors are emitted only on the node types that have them.

Two things to expect:

- The text is **cooked, not the source spelling**. `0x2a` reads back as `"42"`, and the base is
  only recoverable from `tokenFlags`; `"\n"` reads back as an actual newline.
- It is `ValueNone` for a kind that carries no text at all, so `ValueNone` means "nothing there",
  not "empty string".

For the source spelling, slice it yourself:

```fsharp
let spelling = (Ast.sourceText ast).Substring(node.Pos, node.End - node.Pos)
```

## Example: a function's name, parameters and body

```fsharp
let declaration =
    Node.descendants (Node.root ast)
    |> Seq.pick (function FunctionDeclaration declaration -> Some declaration | _ -> None)

FunctionDeclaration.name declaration        // Node<Identifier> voption
FunctionDeclaration.parameters declaration  // Node<ParameterDeclaration> seq
FunctionDeclaration.body declaration        // Node<FunctionBody> voption
```

Each accessor is typed at what the schema declares for the slot, so the walk down carries its
types with it and no step needs a kind check the schema already answered:

```fsharp
match FunctionDeclaration.body declaration with
| ValueSome (Block body) ->
    Block.statements body
    |> Seq.pick (function ReturnStatement statement -> Some statement | _ -> None)
    |> ReturnStatement.expression
| _ -> ValueNone
```

You cannot point an accessor at the wrong node: `IfStatement.thenStatement` will not compile
unless the argument is a `Node<IfStatement>`. Slot numbers never appear — they come from the
schema and shift when upstream inserts a member.

Node types with packed members get those too, at their own types:
`ObjectLiteralExpression.multiLine` is a `bool`, `PrefixUnaryExpression.operator` is a
`SyntaxKind voption`.

## File-level metadata

The root's extended record holds nineteen words about the file. These take the `Ast.SourceFile`
value rather than a node, because a blob holds exactly one source file:

```fsharp
Ast.fileName ast        // string
Ast.path ast            // canonical path - this is what a node handle carries
Ast.sourceText ast      // the whole text
Ast.scriptKind ast      // ScriptKind, the Go enum, not the JS API's
Ast.imports ast         // int[], the module-specifier *nodes*
Ast.referencedFiles ast // FileReference[], from /// <reference path=...>
Ast.ambientModuleNames ast // string[]
```

Note `imports` and `moduleAugmentations` come back as raw node indexes, not strings — tag them
with `Node.ofIndex ast` and read `.Text` for the specifier itself.

`spanMap`, `contentMapper`, `virtualFileName`, `canonicalSourceFileName`,
`supplementalSourceFileNames` and `diagnosticDirectives` exist only for virtual/mapped files and
read as absent otherwise. `diagnosticDirectives` is *not* where `@ts-expect-error` comments go.

Absent and empty are indistinguishable: the encoder writes an empty collection as the same
`0xFFFFFFFF` it writes for absent.

## The escape hatch

`Slot` and `AstNode` — the untyped, unchecked accessors the typed layer is generated over — are
`internal`. Two public APIs over the same bytes is the failure mode worth avoiding. What is left
open, deliberately:

```fsharp
Node.index node             // the raw int, for anything that indexes by it
Node.file node              // the Ast.SourceFile
Node.ofIndex ast index      // back in, asserting a tag that is not checked
Node.retag node             // change the claim, not the node
```

`Ast` itself stays public: it owns the blob type, the file-level record above, and
`Ast.sourceText`.

## Example: a symbol's declarations

A symbol comes from a request. Its `Flags` and `CheckFlags` are typed, so read them by name
rather than by bit:

```fsharp
symbol |> ValueOption.exists (fun symbol -> symbol.Flags.HasFlag SymbolFlags.Property)
```

`SymbolFlags`, `TypeFlags`, `ObjectFlags`, `CheckFlags`, `SignatureFlags` and `ElementFlags` are
generated into `Enums.generated.fs` from upstream's published enums, and the response records name
them. Which field carries which enum is an explicit table in the proto generator, not a guess from
the field name — see entry 3 of `docs/wire-hand-written.md`.

Each enum comes in two halves, and the seam is invisible from here. The single bits upstream
defines are enum cases; the combinations it builds out of them are `[<Literal>]`s in a companion
module of the same name, because an enum case may not name another case of its own enum. Both
answer to the same prefix, and a literal still works as a match pattern:

```fsharp
match symbol.Flags &&& SymbolFlags.Value with
| SymbolFlags.Accessor -> "a getter or a setter"
| _ -> "something else"
```

Its `Declarations` is an array of **node handles**:

```fsharp
let symbol =
    Api.getSymbolAtPosition channel
        { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts"; Position = offset }
```

A handle is `"index.kind.path"` — the node's index in its file's blob, its kind ordinal, and the
file's canonical path. Paths contain dots, so split on the *first two* only:

```fsharp
/// index . kind . path, per `RemoteNode.id` in the typescript package.
let parseHandle (handle: string) =
    let first = handle.IndexOf '.'
    let second = handle.IndexOf('.', first + 1)
    if first < 0 || second < 0 then failwith $"not a node handle: {handle}"

    {| Index = int (handle.Substring(0, first))
       Kind =
        LanguagePrimitives.EnumOfValue<uint32, SyntaxKind>(
            uint32 (handle.Substring(first + 1, second - first - 1)))
       Path = handle.Substring(second + 1) |}
```

Resolving a declaration to a node is then: parse the handle, fetch that file, tag the index.

```fsharp
for declaration in symbol |> ValueOption.bind _.Declarations |> ValueOption.defaultValue [||] do
    let declaration = parseHandle declaration

    match Api.getSourceFile channel
              { Snapshot = snapshot.Snapshot
                Project = project
                File = DocumentIdentifier.FileName declaration.Path } with
    | ValueNone -> ()
    | ValueSome file ->
        match Node.ofIndex<AnyNode> file declaration.Index with
        | FunctionDeclaration found -> FunctionDeclaration.name found |> ignore
        | _ -> ()
```

`Node.ofIndex` is where a handle stops being a promise and starts being a claim, so narrow it
with a view rather than asserting the tag you expect.

Handles mean something only within the program that produced them: same snapshot, same project.
A handle from an older snapshot may point at a different node, or at nothing.

Going the other way — you have a node and want to ask the checker about it — build the same
string, because the `Location` field of the checker requests *is* a handle:

```fsharp
let handle = $"{Node.index node}.{uint32 node.Kind}.{Ast.path (Node.file node)}"
Api.getTypeAtLocation channel { Snapshot = snapshot.Snapshot; Project = project; Location = handle }
```

There is no handle helper in `Xantham.TypeScript.Wire` yet; the two snippets above are the whole
of it. When a second caller needs them, they belong beside `Ast.read` in `Library.fs`.

## Where the truth lives

`Enums.generated.fs`, `Ast.generated.fs`, `AstNode.generated.fs` and `Typed.generated.fs` are
emitted by `tools/tsc-ast/generate-ast.mts` from the vendored `ast.json`, plus the `SourceFile`
record layout parsed out of `encoder.go` and the flag enums transcribed from upstream's own
`packages/typescript/src/enums`. Regenerate with
`dotnet fsi tools/generate-wire.fsx generate ast`; check the vendor pin with
`dotnet fsi tools/generate-wire.fsx sync tsc-ast --check`.

Kind ordinals are positional in `ast.json` and move whenever upstream inserts a kind, so never
write one down: `SyntaxKind.StringLiteral`, never `11u`. The same goes for the flag enums:
`SymbolFlags.Property`, never `4`.

The handful of facts in this pipeline that are transcribed rather than derived — and what to do
when upstream moves them — are listed in `docs/wire-hand-written.md`.
