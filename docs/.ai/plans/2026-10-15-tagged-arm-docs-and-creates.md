# Tagged-Union Arm Removal Without Loss — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the unread arm interfaces that a `TypeScriptTaggedUnion` DU replaces (issue #158),
after first moving the two things those interfaces alone carry, documentation and `Create`
construction, onto the DU.

**Architecture:** First, both folds (tagged and exclusive-arm) compare inline object members by
shape rather than type id, so a member the arms spell identically survives the fold. Then
`detect-tagged-unions` copies the alias's JSDoc onto the DU and each arm
member's JSDoc, tags and literal value onto its case field. It also decides which cases get a
`Create<Case>` static member. Render derives that member's parameters and Emit from the case
fields, so every pass that rewrites field types (Arity, catalog qualification) keeps the helper
in step for free. `synthesize-anonymous` nests an anonymous arm's own declarations under the case
it becomes (`Recovery.Cases.Aborted.Type`, not `Recovery3.Type`). Only then does
`drop-orphan-delegates` prune anonymous arm interfaces that nothing reads, together with the
declarations only those arms read.

**Tech Stack:** F# generator (`src/Xantham.Generator`), Expecto suites, Fable 5.17.2 run gate,
Fable.Core 5.2.0 compile gate.

**Spec:** GitHub issue #158 ("Tagged-union arm interfaces are still declared after the DU
replaces them"), plus the **Design evidence** section below, which is this plan's own spec for
the docs and `Create` preconditions.

## Global Constraints

- Generated bindings target Fable 5.x only (`Fable.Core` 5.2.0 compile gate, Fable 5.17.2 run gate).
- `dotnet fsi build.fsx -- test …` runs tests, never `dotnet test` (it stalls on nested builds).
- A new finding case costs three edits: the union case, its `FindingCodes.table` row, and its
  `Findings.test.fs` snapshot line. Codes for this plan: `DT005`, `DT006`, `DD002`.
- Goldens are regenerated, never hand-edited. Read the golden diff before committing it, using
  aggregate measures and samples for the large fixtures (`.claude/rules/generator-fixtures.md`).
- Doc comments follow `.claude/rules/comments.md`. Reviewer-only notes use `//FOR-REVIEW`.
- `src/**/*.fs` stays Fantomas-formatted (`build.fsx` formats unless `--no-format`). Goldens stay unformatted.
- Scratch files go under `tests/.scratch/`, never the system temp directory.

## Design evidence (settled before planning)

**Lab:** `tests/fixtures/tagged-arm-lab/` (created during planning, not yet committed). At
`de6cc9fa` it generates `Recovery`/`Shape`/`Signal`/`Event`/`Message`/`Clash` DUs. Their
anonymous arms (`Recovery2`–`4`, `Shape2`–`3`, `Signal2`–`3`, `Event2`–`3`, `Clash2`–`3`) are
declared beside them. `Event2` is read by `Opener.open` through `Extract`. `Ping`/`Pong` are named
exported arms.

**What the arms alone carry today:**
1. The union's own JSDoc is dropped already: `TaggedUnions.fs` writes `Docs = ""`. `Recovery`
   renders with no comment, though its alias says "The outcome of recovering a fiber.".
2. Field JSDoc and tags (`@deprecated`) exist only on the arm interface members.
3. `Create` ergonomics the DU case lacks: optional fields are omittable
   (`Recovery2.Create(?snapshot, ?metadata)` vs `Recovery.Completed(None, None)`), and non-tag
   literal fields are written (`Signal2.Create(at)` writes `channel: 'bus'`, while the
   `Signal.Start` case takes `channel: string` from the caller).

**Spike** (`tests/.scratch/tagged-arm-spike/`, hand-written target emission, compiled with
`<WarnOn>3390</WarnOn>` + `TreatWarningsAsErrors`, then Fable-compiled and run under node):
- `[<ParamObject; Emit("({ status: 'completed', ...$0 })")>] static member CreateCompleted (?snapshot: obj, ?metadata: obj) : Recovery = jsNative`
  on a `TypeScriptTaggedUnion` DU compiles and runs. `CreateCompleted()` → `{"status":"completed"}`.
  The helper's JSON equals the case constructor's. The helper's result matches `Recovery.Completed`.
  A generic DU (`CreateRepeat (payload: 'T, ?previous: 'T) : Scheduled<'T>`) works the same.
  A literal-writing helper (`({ channel: 'bus', verb: 'start', ...$0 })`) matches `Signal.Start("bus", 2)`.
- A static member named exactly like its case fails: **FS0023** "The member 'Aborted' cannot be
  defined because the name 'Aborted' clashes with the union case". Helpers are therefore named
  `Create<Case>` (user decision, 2026-10-15).
- `/// <param name="field">` above a union case documents that case's fields. FS3390 checks it:
  an unknown name is an error, and **a case documenting one field must document every field**
  ("no documentation for parameter 'reason'"). An empty `<param name="x"></param>` satisfies it.
  A backtick-escaped field (`` ``type``: string``) is documented as `<param name="type">`.
- An arm's nested declaration in a submodule spelled like its case (`module Recovery = module Aborted = type Type`)
  resolves in type positions only. In expressions, `Recovery.Aborted.Type.User` resolves
  `Recovery.Aborted` to the case and fails with **FS0039** ("The field, constructor or member 'Type'
  is not defined"); in patterns the same path is **FS1127**. Under `module Recovery = module Cases =
  module Aborted = type Type`, `Recovery.Cases.Aborted.Type.User` builds and runs under Fable for
  case construction, the `CreateAborted` helper, a pattern through both, and after `open Lab`.

**Decisions:**
- A case gets `Create<Case>` only when it has an optional field or a literal field. Otherwise
  the case constructor already builds every value the helper would. When `Create<Case>` is itself
  a case name (`Clash.X` vs `Clash.CreateX`), the case gets no helper and `DT006` says so.
- Helper Emit writes the tag first, then literal fields in field order. Parameters are required
  fields first, then optional ones, the same as interface `Create` (`ParamObjects.fs`). A field
  counts as optional when its type is `FsOption _`, matching `ParamObjects.parameterFor`.
- No doc comment on the helper itself: arm `Create` overloads carry none today, so none is lost.
- `DeclarationCatalog`'s tagged-union API hash stays unchanged. Its interface hash excludes
  `CreateOverloads` and property literals, and the helper is derived from data the hash already covers.
- Only arms whose type symbol is synthetic (`isSyntheticName`, i.e. anonymous object literals)
  are pruned. A named interface arm is a declaration in its own right and stays, exported or not.
- An anonymous arm of a union `detect-tagged-unions` will declare nests its own declarations
  under `<Union>.Cases.<Case>` (user decision, 2026-10-15). The case names come from one Spec
  function shared by both passes, so the two cannot drift. Arms that fold into one case share its
  module, and the existing suffixing numbers a clash. A union with a case named `Cases` keeps the
  arm-name path. Named arms keep nesting under their own name.

## Review Focus

1. **A consumer package reading an arm its catalogue owner dropped.** Core.TS/Node catalogues are
   built from the final `Decls`, the same as orphan delegates today. A consumer that reads the
   arm through `Extract` must still compile, minting or reusing per the existing delegate
   precedent. The same applies to nested names that moved under `<Union>.Cases.<Case>`. Covered
   by the full `build.fsx -- test` catalog suites in Task 5. Any failure there is handed back, not
   chased.
2. **Case modules across the prediction boundary.** `synthesize-anonymous` predicts the DU before
   `detect-tagged-unions` decides it. A union that nests under `Cases` but is then not declared
   as a DU would leave the arm interface reading `Recovery.Cases.Aborted.Type`, which compiles but
   misleads. The prediction uses `isTaggedDeclaration`, the same check `detect-tagged-unions`
   applies, so the only gap is alias applications, which never walk their arms. Pinned in Task 3
   by `Shelf` (fallback) and `Job` (fold), and by the compile gate.
3. **Escaped field names in docs and helpers** (`` ``type`` ``). The `<param name>` must be the JS
   key and the helper must still write `type` as the object key. Pinned in Task 1 (doc assertion)
   and Task 2 (run gate `CreateAborted` round trip).
4. **Helper-name collision** (`Clash.X` → `CreateX` is a case). Pinned in Task 2 (`DT006`, no
   `static member CreateX`).
5. **Structural equality admitting a member the arms do not share.** `sameShape` treats two
   plain anonymous objects as one when members pair by name, optionality, readonly and type,
   recursively. A false positive would put a member on a case that one arm lacks at runtime. The
   comparison refuses any object with signatures, index signatures or type arguments, and Task 0
   pins both directions (`owner` kept; `ok` and `note` folded away). Task 5's corpus diff is the
   check that no fold widened unexpectedly.
6. **Arms still read after the drop**: erased fallbacks (`Onset = U2<Onset2, Onset3>`, `Log`,
   `Loose`) and an `Extract` reader (`Event2`). Pinned in Task 4, both in `tagged-arm-lab` and in
   `shared-tag-lab`'s existing test.

## File map

| File | Change | Task |
|---|---|---|
| `src/Xantham.Generator/Model.fs` | `FsTaggedField` + `Docs`, `Tags`, `Literal`; `FsTaggedCase` + `Create` | 1, 2 |
| `src/Xantham.Generator/Shape/TaggedUnions.fs` | union docs, field docs/literal, `Create` decision + findings; shared case names | 1, 2, 3 |
| `src/Xantham.Generator/Shape/Spec.fs` | `sameShape` in both folds; `taggedCaseNamesOf`, `taggedCaseNames` | 0, 3 |
| `src/Xantham.Generator/Shape/Anonymous.fs` | nest anonymous arm declarations under `<Union>.Cases.<Case>` | 3 |
| `src/Xantham.Generator/Render.fs` | case `<param>` docs; `caseCreate` + helper emission; EmitGroups parity | 1, 2 |
| `src/Xantham.Generator/Findings.fs` | `DT005`, `DT006`, `DD002`, `DD003` | 2, 4 |
| `src/Xantham.Generator/Shape/Orphans.fs` | prune unread anonymous tagged arms and declarations under arm roots | 4 |
| `tests/fixtures/tagged-arm-lab/{index.d.ts,package.json}` | the lab (exists, uncommitted; Task 0 adds `Task`, `Run`, `Choice`; Task 3 adds `Shelf`, `Job`) | 0, 3 |
| `tests/Xantham.Generator.Tests/{Pipeline,Render,Shape,Findings}.test.fs` | assertions + record literals | 0–4 |
| `tests/Xantham.Generator.RunGate/{Program.fs,*.fsproj}` | link golden, runtime checks | 2, 3 |
| `docs/.ai/plans/generator-architecture.md`, `generator-type-mapping.md`, `docs/.ai/footguns.md` | records | 5 |

---

### Task 0: Folds compare inline object members by shape

Both folds keep a member only when every arm gives it the same **type id**: the tagged fold
(`foldedArm`, `Spec.fs:470-475`) and the exclusive-arm fold (`exclusiveArmShape`, `Spec.fs:626`).
The checker gives every inline object literal its own id, so two arms spelling
`owner: { name: string }` identically never agree. Scratch lab `tests/.scratch/fold-gap-lab/`,
generated at `de6cc9fa`:
- `Task` (two `done` arms sharing `owner: { name: string }`) renders `| Done` with **no fields**.
- `Choice` (exclusive on `a`/`b`, sharing `meta: { id: string }`) stays `U2<Choice2, Choice3>`.
  The same union with a named `meta: Meta` folds into one `Named` interface with two `Create`
  overloads.

**Files:**
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (new `sameShape` above `foldedArm` at `:461`; `foldedArm` `:463-481` and its call `:543`; `exclusiveArmShape` `:626`)
- Modify: `tests/fixtures/tagged-arm-lab/index.d.ts` (append `Task`, `Run`, `Choice`)
- Test: `Pipeline.test.fs` (registers the `tagged-arm-lab` block, after the `shared-tag-lab` block at `:4415`)
- Add to git: `tests/fixtures/tagged-arm-lab/index.d.ts`, `tests/fixtures/tagged-arm-lab/package.json`

**Interfaces:**
- Produces: `val sameShape: ShapeModel -> int<typeId> -> int<typeId> -> bool` (Spec.fs, internal). `foldedArm` gains a leading `model: ShapeModel` parameter.

- [ ] **Step 0: Record the corpus baseline** (Task 5 diffs against it)

```
dotnet fsi build.fsx -- findings > tests/.scratch/tagged-arm-findings-before.txt
```

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/tagged-arm-lab/index.d.ts`:

```ts

// Folded arms agreeing on an inline object member keep it: equal shapes, distinct type ids.
// `ok` differs by literal and `note` differs inside its object, so both fold away.
export type Task =
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: true; note: { text: string } }
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: false; note: { text: number } }
    | { state: "queued" };

// Members only one folded arm carries fold away.
export type Run =
    | { state: "done"; result: { value: number } }
    | { state: "done"; failure: { message: string } }
    | { state: "queued"; position: number };

// The exclusive-arm fold over an inline object both arms share.
export type Choice =
    | { meta: { id: string }; a: number; b?: never }
    | { meta: { id: string }; b: string; a?: never };
```

- [ ] **Step 2: Register the lab with the failing test**

In `Pipeline.test.fs`, directly after the closing `)` of the `fixtureTests "shared-tag-lab" …`
block, add a sibling in the same list. Later tasks add their test cases to this list.

```fsharp
            fixtureTests "tagged-arm-lab" (handFixture "tagged-arm-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a fold keeps a member the arms spell as one shape" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains
                          source
                          "    | [<CompiledName(\"done\")>] Done of owner: Task2.Owner\n"
                          "equal inline objects agree, nested arrays of them included; `ok` and `note` differ"

                      Expect.stringContains
                          source
                          "    | [<CompiledName(\"done\")>] Done\n    | [<CompiledName(\"queued\")>] Queued of position: float"
                          "members only one arm carries still fold away"

                      Expect.isFalse (source.Contains "type Choice = U2<") "an exclusive fold over an inline shared object fires"
                      Expect.stringContains source "    abstract meta: Choice2.Meta" "and reads the first arm's declaration"
                      Expect.stringContains source "    static member Create (meta: Choice2.Meta, a: float) : Choice = jsNative" "one Create per arm"
                      Expect.stringContains source "    static member Create (meta: Choice2.Meta, b: string) : Choice = jsNative" "for both arms" ])
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on the first assertion (`Done` renders with no field).

- [ ] **Step 3: Structural comparison**

In `Spec.fs`, directly above the `foldedArm` doc comment:

```fsharp
/// An object type with members only: no symbol of its own, type arguments, signatures or index
/// signatures.
let private plainAnonymousObject (facts: TypeFacts) =
    flag TypeFlags.Object facts
    && (facts.SymbolName |> Option.forall isSyntheticName)
    && facts.TypeArguments.IsEmpty
    && facts.IndexInfos.IsEmpty
    && facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty

/// Whether two types read as one F# type: one id; two plain anonymous objects whose members pair
/// by name, optionality and readonly with types that read as one; two unions whose members pair
/// the same way; or two applications of one generic target whose arguments do.
let internal sameShape (model: ShapeModel) (left: int<Measure.typeId>) (right: int<Measure.typeId>) =
    let rec go (assumed: Set<int<Measure.typeId> * int<Measure.typeId>>) left right =
        left = right
        || Set.contains (left, right) assumed
        || (match Map.tryFind left model.Types, Map.tryFind right model.Types with
            | Some l, Some r ->
                let assumed = Set.add (left, right) assumed

                if plainAnonymousObject l && plainAnonymousObject r then
                    l.Members.Length = r.Members.Length
                    && l.Members
                       |> List.forall (fun m ->
                           r.Members
                           |> List.exists (fun o ->
                               o.Symbol.Name = m.Symbol.Name
                               && o.Optional = m.Optional
                               && o.ReadOnly = m.ReadOnly
                               && go assumed m.TypeId o.TypeId))
                elif flag TypeFlags.Union l && flag TypeFlags.Union r then
                    l.UnionMembers.Length = r.UnionMembers.Length
                    && l.UnionMembers |> List.forall (fun m -> r.UnionMembers |> List.exists (go assumed m))
                else
                    match l.Response.TargetTypeId, r.Response.TargetTypeId with
                    | ValueSome lt, ValueSome rt when lt = rt && lt <> left && rt <> right ->
                        l.TypeArguments.Length = r.TypeArguments.Length
                        && List.forall2 (go assumed) l.TypeArguments r.TypeArguments
                    | _ -> false
            | _ -> false)

    go Set.empty left right
```

`assumed` is the coinductive guard: a recursive pair under comparison is taken as equal, so
`type Node = { next?: { … } }` cycles terminate.

- [ ] **Step 4: Use it in both folds**

`foldedArm` becomes `let private foldedArm (model: ShapeModel) (arms: TypeFacts list) =`, and
its `declaredBy` compares `&& sameShape model other.TypeId m.TypeId` in place of
`&& other.TypeId = m.TypeId`. Its call at `:543` becomes `foldedArm model (group |> List.map fst)`.
The folded member keeps the head arm's type id, so the case field reads the head arm's declaration.

In `exclusiveArmShape` at `:626`, replace `m.TypeId = first.TypeId` with
`sameShape model m.TypeId first.TypeId`.

- [ ] **Step 5: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Read the new `tagged-arm-lab` golden in full, plus the `shared-tag-lab` diff.
Expected there: no change, because its folds share primitives only. If any other golden moved
(`rtk git status tests/Xantham.Generator.Tests/golden`), report which ones and why before
continuing.

- [ ] **Step 6: Compile gate and commit**

Run: `dotnet build Xantham.slnx` → Expected: success.

```
rtk git add src/Xantham.Generator/Shape/Spec.fs tests/fixtures/tagged-arm-lab tests/Xantham.Generator.Tests
rtk git commit -m "fix(generator): folds compare inline object members by shape (#158)"
```

---

### Task 1: Union and field documentation reach the DU

**Files:**
- Modify: `src/Xantham.Generator/Model.fs:1682-1685` (`FsTaggedField`)
- Modify: `src/Xantham.Generator/Shape/TaggedUnions.fs:15-118`
- Modify: `src/Xantham.Generator/Render.fs:1031-1052` (`renderTaggedUnion`), plus new private helpers just above it
- Modify: `tests/Xantham.Generator.Tests/Render.test.fs:250-297`, `tests/Xantham.Generator.Tests/Shape.test.fs:4580-4587` (record literals)
- Test: `tests/Xantham.Generator.Tests/Render.test.fs`, `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`tagged-arm-lab` block from Task 0)

**Interfaces:**
- Produces:
  ```fsharp
  type FsTaggedField = { Name: string; Docs: string; Tags: JSDocTagInfo list; Type: FsTypeRef; Literal: FsLiteral option }
  ```
  `Render.fieldDocText: FsTaggedField -> string list` and `Render.caseDocLines: FsTaggedCase -> string list` (private).

- [ ] **Step 1: Change the model**

In `Model.fs`, replace `type FsTaggedField = { Name: string; Type: FsTypeRef }` (keep its existing doc comment above) with:

```fsharp
type FsTaggedField =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Type: FsTypeRef
        /// The value of a literal field: a required property typed by one string or numeric
        /// literal. `Type` is the literal's primitive.
        Literal: FsLiteral option
    }
```

Update the three test record literals to compile: in `Render.test.fs` (`{ Name = "radius"; Type = FsFloat }` and `{ Name = "radius"; Type = FsTypeVar "T" }`) and `Shape.test.fs:4587` (`{ Name = "payload"; Type = FsTypeVar "T" }`), add `Docs = ""; Tags = []; Literal = None`.

- [ ] **Step 2: Write the failing render test**

Append to the `"render"` test list in `Render.test.fs`, after `"a generic tagged union writes its parameters, constraints at the head"`:

```fsharp
        testCase "a tagged case documents every field once any field carries text" <| fun _ ->
            let field name docs literal : FsTaggedField =
                { Name = name; Docs = docs; Tags = []; Type = FsString; Literal = literal }

            let model =
                { baseModel with
                    Decls =
                        [ FsTaggedUnion
                            { Name = "Signal"
                              Docs = "A signal."
                              Tags = []
                              Order = None
                              TypeParameters = []
                              Tag = "verb"
                              Cases =
                                [ { Name = "Start"
                                    CompiledName = Some "start"
                                    Fields =
                                      [ field "channel" "" (Some(LitString "bus"))
                                        field "at" "When it started." None
                                        field "note" "" None ] }
                                  { Name = "Stop"; CompiledName = Some "stop"; Fields = [ field "reason" "" None ] } ] } ] }

            let source = renderAll model |> Map.find "TestPkg.fs"

            Expect.stringContains source "/// A signal." "the union carries its own comment"
            Expect.stringContains source "    /// <param name=\"at\">When it started.</param>" "a documented field"

            Expect.stringContains
                source
                "    /// <param name=\"note\"></param>"
                "an undocumented field beside a documented one is written empty"

            Expect.stringContains
                source
                "    /// <param name=\"channel\">TypeScript type: <c>"
                "a literal field carries its TypeScript type"

            Expect.isFalse (source.Contains "<param name=\"reason\">") "a case with no field text carries no doc"
```

- [ ] **Step 3: Run it and see it fail**

Run: `dotnet fsi build.fsx -- test --quick --filter "render"`
Expected: FAIL on `"a documented field"`. The union doc assertion passes already, since `renderTaggedUnion` renders `decl.Docs`. Only the field docs are missing.

- [ ] **Step 4: Render the case docs**

In `Render.fs`, immediately above the `/// A tagged union (D4, §4.5(2)).` comment, add:

```fsharp
/// The doc text of one tagged-union field: its comment, each tag as `@name text`, and a literal
/// field's TypeScript type.
let private fieldDocText (field: FsTaggedField) : string list =
    [
        let summary = field.Docs.Trim()

        if summary <> "" then
            yield! splitLines summary

        for tag in field.Tags do
            match (tag.Text |> ValueOption.defaultValue "").Trim() with
            | "" -> yield $"@{tag.Name}"
            | text -> yield! splitLines $"@{tag.Name} {text}"

        match field.Literal with
        | Some value -> yield $"TypeScript type: `{Xantham.Generator.Shape.Spec.typeScriptLiteral value}`"
        | None -> ()
    ]

/// The XML doc of a tagged-union case: a `<param>` per field once any field carries text, empty
/// where a field carries none. FS3390 holds a documented case to documenting each field.
let private caseDocLines (case: FsTaggedCase) =
    let texts = case.Fields |> List.map (fun field -> field, fieldDocText field)

    if texts |> List.forall (snd >> List.isEmpty) then
        []
    else
        [
            for field, lines in texts do
                let name = xmlEscape field.Name

                match lines with
                | [] -> yield $"    /// <param name=\"{name}\"></param>"
                | [ single ] -> yield $"    /// <param name=\"{name}\">{inlineCode single}</param>"
                | lines ->
                    yield $"    /// <param name=\"{name}\">"
                    yield! docBody "    " lines
                    yield $"    /// </param>"
        ]
```

In `renderTaggedUnion`, make the first statement inside `for case in decl.Cases do` `yield! caseDocLines case`.

- [ ] **Step 5: Run the render test and see it pass**

Run: `dotnet fsi build.fsx -- test --quick --filter "render"`
Expected: PASS.

- [ ] **Step 6: Carry the docs through `detect-tagged-unions`**

In `TaggedUnions.fs`, add as the first binding inside `async {` (after `let mutable findings = []`):

```fsharp
                    // The alias's own comment, which the DU is the only declaration to carry.
                    let exportDocs =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            Map.tryFind export.Symbol.SymbolId model.ExportTypes
                            |> Option.bind _.Declared
                            |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                        |> Map.ofList
```

Replace the field record built at `:81-84` with:

```fsharp
                                                                let literal =
                                                                    literalMemberValue model m m.TypeId
                                                                    |> Option.filter (fun _ ->
                                                                        reference = FsFloat || reference = FsString)

                                                                {
                                                                    Name = Naming.memberName m.Symbol.Name
                                                                    Docs = m.Docs
                                                                    Tags = m.Tags
                                                                    Type = optionalRef m.Optional reference
                                                                    Literal = literal
                                                                }
```

Replace `Docs = ""` / `Tags = []` at `:109-110` with values read from `exportDocs`. Bind them just before `Some(FsTaggedUnion …)`:

```fsharp
                                            let docs, tags =
                                                Map.tryFind typeId exportDocs |> Option.defaultValue ("", [])
```

and set `Docs = docs` / `Tags = tags`.

- [ ] **Step 7: Write the failing end-to-end test**

Add a second test case to the `tagged-arm-lab` list Task 0 registered:

```fsharp
                  testCase "a tagged union carries the documentation its arms carried" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "/// The outcome of recovering a fiber." "the alias's comment reaches the DU"

                      Expect.stringContains
                          source
                          "    /// <param name=\"snapshot\">The state captured when the fiber finished.</param>"
                          "a field comment reaches its case"

                      Expect.stringContains
                          source
                          "    /// <param name=\"metadata\">@deprecated Read <c>snapshot</c> instead.</param>"
                          "a field tag reaches its case"

                      Expect.stringContains
                          source
                          "    /// <param name=\"type\">Which party aborted it.</param>"
                          "an escaped field is documented under its JS key"

                      Expect.stringContains
                          source
                          "    /// <param name=\"channel\">TypeScript type: <c>"
                          "a literal field records its literal"

                      Expect.stringContains source "    /// <param name=\"at\"></param>" "and its undocumented sibling is written empty"
```

- [ ] **Step 8: Run end-to-end, regenerating goldens**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS, with `tests/Xantham.Generator.Tests/golden/tagged-arm-lab/TaggedArmLab.fs` updated.
If the lab assertions fail before Step 6 is in place, that confirms Step 6 is what makes them pass.
Read the lab golden diff and the `shared-tag-lab` golden diff (expected:
`<param>` lines on `Signal`'s cases from the `channel` literal, nothing else).

- [ ] **Step 9: Compile gate**

Run: `dotnet build Xantham.slnx`
Expected: success. The compile gate globs every golden, the new one included.

- [ ] **Step 10: Commit**

```
rtk git add src/Xantham.Generator/Model.fs src/Xantham.Generator/Shape/TaggedUnions.fs src/Xantham.Generator/Render.fs tests/Xantham.Generator.Tests
rtk git commit -m "feat(generator): carry union and field docs onto tagged-union DUs (#158)"
```

---

### Task 2: `Create<Case>` static members on the DU

**Files:**
- Modify: `src/Xantham.Generator/Model.fs:1696-1701` (`FsTaggedCase`)
- Modify: `src/Xantham.Generator/Findings.fs:190-193` (table), `:854-871` (`DetectTaggedUnions`)
- Modify: `src/Xantham.Generator/Shape/TaggedUnions.fs` (cases builder)
- Modify: `src/Xantham.Generator/Render.fs` (`caseCreate`, `renderTaggedUnion`, EmitGroups `match decl with` at `:2042-2053`)
- Modify: `tests/Xantham.Generator.Tests/{Render,Shape}.test.fs` record literals, `Findings.test.fs:157`
- Modify: `tests/Xantham.Generator.RunGate/Xantham.Generator.RunGate.fsproj:46`, `tests/Xantham.Generator.RunGate/Program.fs`
- Test: `Render.test.fs`, `Pipeline.test.fs` (`tagged-arm-lab` block)

**Interfaces:**
- Consumes: `FsTaggedField.Literal` (Task 1), `Render.createAttribute`, `Render.renderParamList`, `Render.declRef`.
- Produces:
  ```fsharp
  type FsTaggedCase = { Name: string; CompiledName: string option; Fields: FsTaggedField list; Create: string option }
  // Render.fs, private
  val caseCreate: FsTaggedUnionDecl -> FsTaggedCase -> (string * FsCreateOverload) option
  // Findings.fs
  DetectTaggedUnions.CaseCreateSynthesized of create: string   // DT005, Exact
  DetectTaggedUnions.CaseCreateNameTaken of create: string     // DT006, Ergonomic
  ```

- [ ] **Step 1: Model and findings**

`Model.fs`, `FsTaggedCase` becomes:

```fsharp
type FsTaggedCase =
    {
        Name: string
        CompiledName: string option
        Fields: FsTaggedField list
        /// The name of the case's `Create` static member, which builds the case with its optional
        /// fields omittable and its literal fields written.
        Create: string option
    }
```

Add `Create = None` to every `FsTaggedCase` literal in `Render.test.fs` and `Shape.test.fs`, including Task 1's new test.

`Findings.fs`: after `"DT.ArmsMergedOnSharedTag", "DT004"` add

```fsharp
            "DT.CaseCreateSynthesized", "DT005"
            "DT.CaseCreateNameTaken", "DT006"
```

and append to `DetectTaggedUnions`:

```fsharp
    /// The case has a `Create` static member building it with its optional fields omittable and
    /// its literal fields written.
    | [<Exact>] CaseCreateSynthesized of create: string
    /// The case's `Create` name is a case of the same union, so the case has no `Create`.
    | [<Ergonomic>] CaseCreateNameTaken of create: string
```

with messages

```fsharp
            | CaseCreateSynthesized create ->
                $"case Create '{create}': optional fields omittable, literal fields written"
            | CaseCreateNameTaken create -> $"no case Create: '{create}' is already a case of the union"
```

`Findings.test.fs`, after line 157:

```fsharp
                    "DT.CaseCreateSynthesized DT005 exact"
                    "DT.CaseCreateNameTaken DT006 ergonomic"
```

Run: `dotnet fsi build.fsx -- test --quick --filter "findings"` → Expected: PASS.

- [ ] **Step 2: Write the failing render test**

Append to the `"render"` list:

```fsharp
        testCase "a tagged case with an optional or literal field has a Create" <| fun _ ->
            let field name ty literal : FsTaggedField =
                { Name = name; Docs = ""; Tags = []; Type = ty; Literal = literal }

            let model =
                { baseModel with
                    Decls =
                        [ FsTaggedUnion
                            { Name = "Signal"
                              Docs = ""
                              Tags = []
                              Order = None
                              TypeParameters = []
                              Tag = "verb"
                              Cases =
                                [ { Name = "Start"
                                    CompiledName = Some "start"
                                    Fields = [ field "channel" FsString (Some(LitString "bus")); field "at" FsFloat None ]
                                    Create = Some "CreateStart" }
                                  { Name = "Stop"
                                    CompiledName = Some "stop"
                                    Fields = [ field "reason" (FsOption FsString) None; field "code" FsFloat None ]
                                    Create = Some "CreateStop" }
                                  { Name = "Halt"; CompiledName = Some "halt"; Fields = []; Create = None } ] } ] }

            let source = renderAll model |> Map.find "TestPkg.fs"

            Expect.stringContains
                source
                "    [<ParamObject; Emit(\"({ verb: 'start', channel: 'bus', ...$0 })\")>]\n    static member CreateStart (at: float) : Signal = jsNative"
                "the tag and literal fields are written, the rest bound"

            Expect.stringContains
                source
                "    static member CreateStop (code: float, ?reason: string) : Signal = jsNative"
                "required parameters first, optional ones omittable"

            Expect.isFalse (source.Contains "CreateHalt") "a case without the field kinds has no Create"
```

Run: `dotnet fsi build.fsx -- test --quick --filter "render"` → Expected: FAIL (`CreateStart` not rendered).

- [ ] **Step 3: Render the helper**

In `Render.fs`, after `caseDocLines` (Task 1):

```fsharp
/// The `Create` overload of a tagged case: the non-literal fields as parameters, required ones
/// first, with the tag and the literal fields written into the object.
let private caseCreate (decl: FsTaggedUnionDecl) (case: FsTaggedCase) : (string * FsCreateOverload) option =
    case.Create
    |> Option.map (fun memberName ->
        let parameters =
            case.Fields
            |> List.filter (fun field -> field.Literal.IsNone)
            |> List.map (fun field ->
                ({
                    Name = field.Name
                    Optional =
                        match field.Type with
                        | FsOption _ -> true
                        | _ -> false
                    Rest = false
                    Type = field.Type
                 }
                : FsParam))

        let required, optional = parameters |> List.partition (fun p -> not p.Optional)
        let tag = case.CompiledName |> Option.defaultValue case.Name

        let literals =
            case.Fields
            |> List.choose (fun field -> field.Literal |> Option.map (fun value -> field.Name, value))

        memberName,
        {
            Parameters = required @ optional
            Literals = (decl.Tag, LitString tag) :: literals
        })
```

In `renderTaggedUnion`, after the `for case in decl.Cases do … ` loop that yields the cases, add:

```fsharp
        for case in decl.Cases do
            match caseCreate decl case with
            | Some(memberName, overload) ->
                yield "    " + createAttribute overload

                yield
                    $"    static member {ident memberName} {renderParamList overload.Parameters} : {declRef decl.Name decl.TypeParameters} = jsNative"
            | None -> ()
```

In the EmitGroups `match decl with` (`:2042`), before `| _ -> ()`, add the same parameter-name accounting interfaces get:

```fsharp
                                    | FsTaggedUnion union ->
                                        for case in union.Cases do
                                            match caseCreate union case with
                                            | Some(memberName, overload) ->
                                                yield! bound $"{union.Name}.{memberName}" overload.Parameters
                                            | None -> ()
```

Run: `dotnet fsi build.fsx -- test --quick --filter "render"` → Expected: PASS.

- [ ] **Step 4: Decide the helper in `detect-tagged-unions`**

In `TaggedUnions.fs`, inside the `List.map2` lambda, after `let fields = …` and before the case record:

```fsharp
                                                        let wantsCreate =
                                                            fields
                                                            |> List.exists (fun field ->
                                                                field.Literal.IsSome
                                                                || (match field.Type with
                                                                    | FsOption _ -> true
                                                                    | _ -> false))

                                                        let createName = $"Create{caseName}"
                                                        let symbol = $"{name}.{caseName}"

                                                        let create =
                                                            if not wantsCreate then
                                                                None
                                                            elif List.contains createName caseNames then
                                                                findings <-
                                                                    findings
                                                                    @ [
                                                                        Finding.make
                                                                            symbol
                                                                            (DetectTaggedUnions.CaseCreateNameTaken createName)
                                                                    ]

                                                                None
                                                            else
                                                                findings <-
                                                                    findings
                                                                    @ [
                                                                        Finding.make
                                                                            symbol
                                                                            (DetectTaggedUnions.CaseCreateSynthesized createName)
                                                                    ]

                                                                Some createName
```

and add `Create = create` to the case record.

- [ ] **Step 5: Write the failing end-to-end test**

Add a second `testCase` to the `tagged-arm-lab` block (it shares the `messagesFor` helper shape of `shared-tag-lab`):

```fsharp
                  testCase "a case with an optional or literal field has a Create" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      let messagesFor symbol key =
                          rendered.Findings
                          |> List.filter (fun f -> f.Symbol = symbol && f.Key = key)
                          |> List.map _.Message

                      Expect.stringContains
                          source
                          "    static member CreateCompleted (?snapshot: obj, ?metadata: JS.Record<string, obj>) : Recovery = jsNative"
                          "optional fields are omittable"

                      Expect.stringContains source "    static member CreateAborted (" "a case mixing both has one"
                      Expect.stringContains source "?reason: string) : Recovery = jsNative" "with its optional field last"

                      Expect.stringContains
                          source
                          "    [<ParamObject; Emit(\"({ verb: 'start', channel: 'bus', ...$0 })\")>]"
                          "a literal field is written by the Emit"

                      Expect.stringContains source "    static member CreateStart (at: float) : Signal = jsNative" "and binds no parameter"
                      Expect.stringContains source "    static member CreatePong (?at: float) : Message = jsNative" "named arms' cases too"

                      for absent in [ "CreatePending"; "CreateCircle"; "CreateSquare"; "CreateOpen"; "CreatePing" ] do
                          Expect.isFalse (source.Contains absent) $"{absent}: the case constructor already builds it"

                      Expect.isFalse (source.Contains "static member CreateX") "a helper name that is a case is not declared"

                      Expect.equal
                          (messagesFor "Clash.X" "DT006")
                          [ "no case Create: 'CreateX' is already a case of the union" ]
                          "and the collision is reported"

                      Expect.equal
                          (messagesFor "Recovery.Completed" "DT005")
                          [ "case Create 'CreateCompleted': optional fields omittable, literal fields written" ]
                          "a synthesized helper is reported once" ]
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Then read the `tagged-arm-lab` golden diff and `shared-tag-lab` diff. Expected in
`SharedTagLab.fs`: `CreateStart`/`CreateStop` on `Signal`, `CreateRepeat` on `Scheduled<'T>`,
`CreateWaiting` on `Scheduler.Current.Result<'T>`, and no other change.

- [ ] **Step 6: Run-gate checks**

`Xantham.Generator.RunGate.fsproj`, after line 46:

```xml
        <Compile Include="..\Xantham.Generator.Tests\golden\tagged-arm-lab\TaggedArmLab.fs" Link="tagged-arm-lab\TaggedArmLab.fs" />
```

`Program.fs`, after `genericTaggedCases`:

```fsharp
/// A case `Create` builds the object its case builds, absent optional fields omitted and literal
/// fields written, and the result matches the case its tag names.
let private taggedCaseCreates () =
    equal
        "a case Create with no argument writes the tag alone"
        """{"status":"completed"}"""
        (json (TaggedArmLab.Recovery.CreateCompleted()))

    equal
        "a case Create and its case build the same object"
        (json (TaggedArmLab.Recovery.Completed(Some(box 1), None)))
        (json (TaggedArmLab.Recovery.CreateCompleted(snapshot = 1)))

    equal
        "a case Create writes a literal field"
        """{"verb":"start","channel":"bus","at":2}"""
        (json (TaggedArmLab.Signal.CreateStart 2.0))

    match TaggedArmLab.Recovery.CreateAborted(TaggedArmLab.Recovery3.Type.User, reason = "r") with
    | TaggedArmLab.Recovery.Aborted(reason, _) ->
        equal "a case Create with an escaped field matches its case" "r" (Option.defaultValue "" reason)
    | _ -> check "a case Create reached another case" false

    equal
        "an escaped field is written under its JS key"
        """{"status":"aborted","type":"user"}"""
        (json (TaggedArmLab.Recovery.CreateAborted TaggedArmLab.Recovery3.Type.User))

    equal
        "a generic case Create writes its payload"
        """{"kind":"repeat","payload":1}"""
        (json (SharedTagLab.Scheduled.CreateRepeat 1.0))
```

Call `taggedCaseCreates ()` after `genericTaggedCases ()` (`:2303`). If the string-enum case is not
spelled `Recovery3.Type.User` in the golden, use the spelling the golden has.

Run: `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"`
Expected: run gate reports every check passed, the six new ones included.

- [ ] **Step 7: Compile gate and commit**

Run: `dotnet build Xantham.slnx` → Expected: success.

```
rtk git add src/Xantham.Generator tests/Xantham.Generator.Tests tests/Xantham.Generator.RunGate
rtk git commit -m "feat(generator): Create<Case> helpers on tagged-union DUs (#158)"
```

---

### Task 3: Nest arm declarations under their case

**Files:**
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (after `uniqueCaseNames`, `:3891`)
- Modify: `src/Xantham.Generator/Shape/TaggedUnions.fs:62-63`
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs:296-303` (mutables), `:641-644` (`into`), `:696-698` (union-member walk)
- Modify: `tests/fixtures/tagged-arm-lab/index.d.ts` (append `Shelf`, `Job`)
- Modify: `tests/Xantham.Generator.RunGate/Program.fs` (`taggedCaseCreates` from Task 2)
- Test: `Pipeline.test.fs` (`tagged-arm-lab` block)

**Interfaces:**
- Consumes: `Spec.isTaggedDeclaration`, `Spec.taggedUnionShape`, `Spec.taggedArmValue`, `Spec.uniqueCaseNames`, `Spec.nestUnder`, `Spec.isSyntheticName`.
- Produces:
  ```fsharp
  val taggedCaseNamesOf: (TypeFacts * string) list -> string list            // Spec.fs, internal
  val taggedCaseNames: ShapeModel -> TypeFacts -> Map<int<typeId>, string>  // Spec.fs, internal
  ```
  Declarations nested under an anonymous arm of a declared DU are named `<Union>.Cases.<Case>.<Segment>`.

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/tagged-arm-lab/index.d.ts`:

```ts

// A case named `Cases` would shadow the case modules, so these arms nest under their own names.
export type Shelf =
    | { kind: "cases"; size: "s" | "m" }
    | { kind: "box"; size: "l" | "xl" };

// Arms folding into one case nest under that case.
export type Job =
    | { state: "done"; level: "info" | "warn" }
    | { state: "done"; level: "info" | "warn"; code: number }
    | { state: "queued" };
```

- [ ] **Step 2: Write the failing end-to-end test**

Add to the `tagged-arm-lab` block:

```fsharp
                  testCase "an anonymous arm's declarations nest under its case" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains
                          source
                          "Aborted of reason: string option * ``type``: Recovery.Cases.Aborted.Type"
                          "the case field reads the declaration under its case"

                      Expect.isFalse (source.Contains "Recovery3.Type") "and not under the arm's numbered name"
                      Expect.stringContains source "Done of level: Job.Cases.Done.Level" "folded arms share their case's module"
                      Expect.stringContains source "Shelf2.Size" "a union with a case named Cases keeps the arm path"
                      Expect.stringContains source "Shelf3.Size" "for every arm"

                      Expect.stringContains
                          source
                          "    | [<CompiledName(\"done\")>] Done of owner: Task.Cases.Done.Owner\n"
                          "a member the fold kept reads the declaration under its case"

                      Expect.stringContains
                          source
                          "    abstract meta: Choice.Meta"
                          "an exclusive fold's shared member nests under the folded interface"

                      Expect.isFalse (source.Contains "Choice2.Meta") "not under an arm the fold removed"
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on `"the case field reads the declaration under its case"`. The `Shelf` assertions
already pass, since that is today's naming.

- [ ] **Step 3: One source for case names**

In `Spec.fs`, directly after `uniqueCaseNames`:

```fsharp
/// The DU case names of a discriminated union's cases, in case order.
let internal taggedCaseNamesOf (tagged: (TypeFacts * string) list) =
    tagged |> List.map (snd >> Naming.enumCaseOfString) |> uniqueCaseNames

/// The case each arm of a union reads under once `detect-tagged-unions` declares it, keyed by
/// the arm's type id. Arms folding into one case share its name. Empty for a union that pass
/// does not declare.
let internal taggedCaseNames (model: ShapeModel) (facts: TypeFacts) : Map<int<Measure.typeId>, string> =
    if not (isTaggedDeclaration model facts) then
        Map.empty
    else
        match taggedUnionShape model facts with
        | Discriminated(tag, tagged, _) ->
            let byValue =
                List.zip (tagged |> List.map snd) (taggedCaseNamesOf tagged) |> Map.ofList

            facts.UnionMembers
            |> List.choose (fun armId ->
                Map.tryFind armId model.Types
                |> Option.bind (taggedArmValue model tag)
                |> Option.bind byValue.TryFind
                |> Option.map (fun caseName -> armId, caseName))
            |> Map.ofList
        | TagCollides _
        | Untagged -> Map.empty
```

In `TaggedUnions.fs:62-63`, replace
`tagged |> List.map (snd >> Naming.enumCaseOfString) |> uniqueCaseNames` with `taggedCaseNamesOf tagged`.

- [ ] **Step 4: Nest under the case in `synthesize-anonymous`**

In `Anonymous.fs`, beside the other mutables at `:297-303`:

```fsharp
    /// The module an anonymous arm's own declarations nest under, where it differs from the
    /// arm's name: `Recovery.Cases.Aborted` for an arm of a union declared as a DU, `Choice` for
    /// an arm of a union the exclusive-arm fold declares as one interface.
    let mutable nestBases: Map<int<Measure.typeId>, string> = Map.empty
```

Replace `into` (`:643-644`) with:

```fsharp
                let into segment =
                    nestUnder
                        (Map.tryFind typeId nestBases
                         |> Option.orElse named
                         |> Option.defaultValue path)
                        segment
```

Replace the union-member walk (`:696-698`) with:

```fsharp
                    if not readsApplication then
                        // A case named `Cases` would take the module path the case modules
                        // open under, so that union keeps the arm-name path.
                        // The exclusive-arm fold declares the union as one interface under its own
                        // name, so its arms' declarations nest there.
                        let caseModules =
                            match named with
                            | Some unionName ->
                                let caseNames = taggedCaseNames model facts

                                if not caseNames.IsEmpty then
                                    if caseNames |> Map.exists (fun _ caseName -> caseName = "Cases") then
                                        Map.empty
                                    else
                                        caseNames
                                        |> Map.map (fun _ caseName -> nestUnder (nestUnder unionName "Cases") caseName)
                                else
                                    match exclusiveArmShape model facts with
                                    | ExclusiveArmsFold _ ->
                                        facts.UnionMembers |> List.map (fun armId -> armId, unionName) |> Map.ofList
                                    | NotExclusiveArms
                                    | ExclusiveArmsDecline _ -> Map.empty
                            | None -> Map.empty

                        for memberId in facts.UnionMembers do
                            let anonymous =
                                Map.tryFind memberId model.Types
                                |> Option.exists (fun arm -> arm.SymbolName |> Option.forall isSyntheticName)

                            match Map.tryFind memberId caseModules with
                            | Some caseModule when anonymous -> nestBases <- Map.add memberId caseModule nestBases
                            | _ -> ()

                            walk (named |> Option.defaultValue path) order memberId
```

The arm's own name is still claimed under the union's path (`Recovery3`). Only its nested
declarations move. That keeps the erased fallback and `Extract` readers of the arm unchanged.

- [ ] **Step 5: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Read the `tagged-arm-lab` golden diff, which should show a `module Recovery =` /
`module Cases =` / `module Aborted =` nesting holding `Type`, and `Choice.Meta` beside a
`Choice.Meta2` (the second arm's, read by nothing; Task 4 removes it). Folded `Task` arms show
`Task.Cases.Done.Owner` and `Owner2`, the same way. Read the `shared-tag-lab` diff as
well. Expected: no change, because none of its DU arms has a nested declaration. Any other moved
golden is reported before continuing.

Task 0's test case pins today's names, and this task moves them. In
`"a fold keeps a member the arms spell as one shape"`, replace `Done of owner: Task2.Owner` with
`Done of owner: Task.Cases.Done.Owner`, and each `Choice2.Meta` with `Choice.Meta`.

- [ ] **Step 6: Run gate spelling**

In `Program.fs`'s `taggedCaseCreates`, replace both `TaggedArmLab.Recovery3.Type.User` with
`TaggedArmLab.Recovery.Cases.Aborted.Type.User`. That exercises the expression-position path the
spike proved (FS0039 under the case-named form).

Run: `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"` and `dotnet build Xantham.slnx`
Expected: both green.

- [ ] **Step 7: Commit**

```
rtk git add src/Xantham.Generator/Shape tests/fixtures/tagged-arm-lab tests/Xantham.Generator.Tests tests/Xantham.Generator.RunGate
rtk git commit -m "feat(generator): nest tagged-arm declarations under their case module (#158)"
```

---

### Task 4: Drop unread arm interfaces and the declarations only they read

**Files:**
- Modify: `src/Xantham.Generator/Shape/Orphans.fs:111-183`
- Modify: `src/Xantham.Generator/Findings.fs:250`, `:1217-…` (`DropOrphanDelegates`), `tests/Xantham.Generator.Tests/Findings.test.fs:214`
- Test: `Pipeline.test.fs` (`tagged-arm-lab` and `shared-tag-lab` blocks)

**Interfaces:**
- Consumes: Tasks 0–3 (the DU carries everything the arm carried, and the arm's nested declarations live under `<Union>.Cases.<Case>` or the exclusive-folded interface). `Spec.taggedCaseNames`, `Spec.exclusiveArmShape`.
- Produces: `DropOrphanDelegates.TaggedArmReachedNoReference` (`DD002`, Exact), `DropOrphanDelegates.ArmDeclarationReachedNoReference` (`DD003`, Exact).

- [ ] **Step 1: Findings**

`Findings.fs`, after `"DD.DelegateReachedNoReference", "DD001"`:

```fsharp
            "DD.TaggedArmReachedNoReference", "DD002"
            "DD.ArmDeclarationReachedNoReference", "DD003"
```

Add to `DropOrphanDelegates`:

```fsharp
    /// The interface was named for an anonymous arm of a union declared as a tagged DU, which
    /// carries the arm's fields, documentation and construction; nothing else reads it.
    | [<Exact>] TaggedArmReachedNoReference
    /// The declaration was named under an arm the union no longer declares, for a member the
    /// union's own declaration does not carry; nothing else reads it.
    | [<Exact>] ArmDeclarationReachedNoReference
```

messages:

```fsharp
            | TaggedArmReachedNoReference ->
                "arm interface dropped: the tagged union carries its fields and nothing else reads it"
            | ArmDeclarationReachedNoReference ->
                "declaration dropped: only an arm the union replaced read it"
```

`Findings.test.fs` after line 214:

```fsharp
                    "DD.TaggedArmReachedNoReference DD002 exact"
                    "DD.ArmDeclarationReachedNoReference DD003 exact"
```

- [ ] **Step 2: Write the failing tests**

Add to the `tagged-arm-lab` block:

```fsharp
                  testCase "an arm interface nothing reads is dropped" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      let messagesFor symbol key =
                          rendered.Findings
                          |> List.filter (fun f -> f.Symbol = symbol && f.Key = key)
                          |> List.map _.Message

                      for gone in [ "Recovery2"; "Recovery3"; "Recovery4"; "Shape2"; "Shape3"; "Signal2"; "Signal3"; "Event3"; "Clash2"; "Clash3"; "Task2"; "Task3"; "Task4"; "Run2"; "Run3"; "Run4" ] do
                          Expect.isFalse (source.Contains $"type {gone} =") $"{gone} is dropped"

                      // What only a dropped or folded arm read goes with it, transitively.
                      for gone in [ "Owner2"; "Result"; "Failure"; "Meta2" ] do
                          Expect.isFalse (source.Contains $"type {gone} =") $"{gone}: read by no declaration left"

                      Expect.stringContains source "type Owner =" "the declaration a kept case field reads stays"
                      Expect.stringContains source "type Meta =" "and the one the exclusive-folded interface reads"

                      Expect.equal
                          (messagesFor "Task.Cases.Done.Owner2" "DD003")
                          [ "declaration dropped: only an arm the union replaced read it" ]
                          "a dropped nested declaration is reported"

                      Expect.stringContains source "type Event2 =" "an arm read through Extract stays"
                      Expect.stringContains source "type Ping =" "a named arm stays"
                      Expect.stringContains source "type Pong =" "a named arm stays when nothing reads it"
                      Expect.isFalse (source.Contains "Recovery3") "a dropped arm leaves no module behind"
                      Expect.stringContains source "module Recovery =" "its nested declarations live under the union"
                      Expect.isFalse (source.Contains "type Job2 =") "folded arms are dropped too"
                      Expect.isFalse (source.Contains "type Shelf2 =") "a Cases-collision union's arms are dropped as well"

                      Expect.stringContains
                          source
                          "Cases of size: Shelf2.Size"
                          "and their nested declarations keep the arm path, the one remaining residue"

                      Expect.equal
                          (messagesFor "Recovery2" "DD002")
                          [ "arm interface dropped: the tagged union carries its fields and nothing else reads it" ]
                          "each dropped arm is reported"

                      Expect.equal (messagesFor "Event2" "DD002") [] "a read arm is not"
```

Before committing, check the arm names against the Task 3 golden. If `Clash` arms are named differently,
use the golden's names.

In the `shared-tag-lab` test (`Pipeline.test.fs:4416` onward), append:

```fsharp
                      for gone in [ "Distinct2"; "Distinct3"; "Distinct4"; "Signal2"; "Signal3"; "Terminal2"; "Terminal3"; "Terminal4"; "Scheduled2"; "Scheduled3"; "Result2"; "Result3" ] do
                          Expect.isFalse (source.Contains $"type {gone}") $"{gone}: an unread tagged arm is dropped"

                      for kept in [ "Onset2"; "Onset3"; "Log2"; "Log3"; "Loose2"; "Loose3"; "FetchInfo" ] do
                          Expect.stringContains source $"type {kept} =" $"{kept}: an arm an erased union or name holds stays"
```

Run: `dotnet fsi build.fsx -- test --quick --filter "generator e2e"` → Expected: FAIL on `Recovery2 is dropped`.

- [ ] **Step 3: Prune**

In `Orphans.fs`, add above `prune`:

```fsharp
/// The interface names of the anonymous arms of every union declared as a tagged DU. The DU
/// carries each arm's fields, documentation and construction.
let private taggedArmNames (model: ShapeModel) : Set<string> =
    let unions =
        model.Decls
        |> List.choose (function
            | FsTaggedUnion u -> Some u.Name
            | _ -> None)
        |> Set.ofList

    model.DeclNames
    |> Map.toList
    |> List.filter (snd >> unions.Contains)
    |> List.collect (fun (typeId, _) ->
        Map.tryFind typeId model.Types
        |> Option.map _.UnionMembers
        |> Option.defaultValue [])
    |> List.filter (fun armId ->
        Map.tryFind armId model.Types
        |> Option.exists (fun arm -> arm.SymbolName |> Option.forall isSyntheticName))
    |> List.choose (fun armId -> Map.tryFind armId model.DeclNames)
    |> Set.ofList
```

and below it:

```fsharp
/// The name prefixes under which declarations exist for arms alone: each tagged union's `Cases`
/// module, each anonymous tagged arm's own path, and each union the exclusive-arm fold declares as
/// one interface.
let private armRoots (model: ShapeModel) (arms: Set<string>) : string list =
    let unionRoots =
        model.DeclNames
        |> Map.toList
        |> List.choose (fun (typeId, name) ->
            match Map.tryFind typeId model.Types with
            | Some facts when not (Map.isEmpty (taggedCaseNames model facts)) -> Some $"{name}.Cases."
            | Some facts ->
                match exclusiveArmShape model facts with
                | ExclusiveArmsFold _ -> Some $"{name}."
                | NotExclusiveArms
                | ExclusiveArmsDecline _ -> None
            | None -> None)

    unionRoots @ [ for arm in arms -> $"{arm}." ]

/// The name a declaration is written under; `None` for the `Exports` container.
let private declName (decl: FsDecl) =
    match decl with
    | FsInterface d -> Some d.Name
    | FsStringEnum d -> Some d.Name
    | FsTaggedUnion d -> Some d.Name
    | FsEnum d -> Some d.Name
    | FsAbbrev d -> Some d.Name
    | FsDelegateType d -> Some d.Name
    | FsPhantom d -> Some d.Name
    | FsMeasure d -> Some d.Name
    | FsExports _ -> None
```

Change `prune` to take `(arms: Set<string>) (roots: string list)` first. Update its doc comment
to "every unread delegate, unread tagged-union arm interface and unread declaration under an arm
root". Then change three things inside it:
- candidates become:
  ```fsharp
      let candidates =
          decls
          |> List.choose (fun decl ->
              match decl, declName decl with
              | FsDelegateType d, _ when not (Set.contains d.Name read) -> Some d.Name
              | FsInterface i, _ when Set.contains i.Name arms && not (Set.contains i.Name read) -> Some i.Name
              | _, Some name when
                  not (Set.contains name read) && roots |> List.exists (fun root -> name.StartsWith root)
                  ->
                  Some name
              | _ -> None)
          |> Set.ofList
  ```
- filter becomes `decls |> List.filter (fun decl -> declName decl |> Option.forall (fun name -> not (Set.contains name unread)))`
- recursion: `|> prune arms roots exports (dropped @ List.ofSeq unread)`

The loop repeats until nothing is unread, so `Owner2` goes once `Task3` has, and a nested
`Owner2.Tags` follows on the next round.

In `dropOrphanDelegates`:

```fsharp
                    let arms = taggedArmNames model

                    let delegates =
                        model.Decls
                        |> List.choose (function
                            | FsDelegateType d -> Some d.Name
                            | _ -> None)
                        |> Set.ofList

                    let decls, dropped =
                        prune arms (armRoots model arms) (exportNames ctx model) [] model.Decls
```

and the finding map:

```fsharp
                        |> List.map (fun name ->
                            if Set.contains name delegates then
                                Finding.make name DropOrphanDelegates.DelegateReachedNoReference
                            elif Set.contains name arms then
                                Finding.make name DropOrphanDelegates.TaggedArmReachedNoReference
                            else
                                Finding.make name DropOrphanDelegates.ArmDeclarationReachedNoReference)
```

A declaration under an arm root that is self-recursive reads its own name and is never unread.
That residue is accepted; Task 5's corpus measure shows whether it occurs.

Leave the existing export-retention rule (`retained`) in place. An export's only declaration is never dropped.

Leave the pass name `drop-orphan-delegates` unchanged. It appears in every manifest. Add a
`//FOR-REVIEW` line above `dropOrphanDelegates` noting the rename to `drop-orphan-declarations` is deferred.

- [ ] **Step 4: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"` → Expected: PASS.
Read both lab golden diffs. In `TaggedArmLab.fs`, no `Recovery3` should remain anywhere. In
`SharedTagLab.fs`, only the listed arms and their companion modules should be removed.

- [ ] **Step 5: Gates and commit**

Run: `dotnet build Xantham.slnx` and `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"`.
Expected: both green. A run-gate or golden reference to a removed `…2`/`…3` type is a compile
error here. Fix it by moving the reference to the DU case or its `Create<Case>`.

```
rtk git add src/Xantham.Generator tests/Xantham.Generator.Tests
rtk git commit -m "fix(generator): drop unread tagged-union arm interfaces (#158)"
```

---

### Task 5: Corpus, measures, records

**Files:**
- Modify: every golden under `tests/Xantham.Generator.Tests/golden/` that moves (regenerated)
- Modify: `docs/.ai/plans/generator-architecture.md` (new section at end), `docs/.ai/plans/generator-type-mapping.md:146` and §4.5(2), `docs/.ai/footguns.md`
- Possibly modify: `src/Xantham.Fable.Core.TS/Fable.Core.TS.fs`, `src/Xantham.Fable.Node/Node.fs` (Step 3)

- [ ] **Step 1: Regenerate and gate the whole corpus**

```
dotnet fsi build.fsx -- test --update
dotnet fsi build.fsx -- findings > tests/.scratch/tagged-arm-findings-after.txt
```

Compare against Task 1 Step 0's baseline. Report `DD002`, `DT005` and `DT006` counts per fixture,
plus tier counts and `git diff --stat` for the goldens. Do not open large goldens. Grep them for
`static member Create[A-Z]` and sample at most 200 diff lines. A moved count you cannot explain
from this plan goes back to the user (`.claude/rules/generator-fixtures.md`, "Hand back what you
cannot explain").

- [ ] **Step 2: Catalogue compatibility assessment**

Record the reasoning in the architecture section (Step 4). Proposed outcome: **no**
`CatalogCompatibility.current` bump. The canonical API hash of tagged unions and interfaces is
unchanged. An earlier producer's catalogue still owns arm interfaces it declared, and its emitted
file still declares them. A newer producer owns fewer declarations, which orphan delegates already
do. Task 3 renames nested declarations (`Recovery3.Type` → `Recovery.Cases.Aborted.Type`). A
consumer takes those names from the producer's catalogue, so a consumer pairs with the emitted file
of the producer it read. Check `footguns.md` "Declaration catalogs" for whether a declaration name
counts as catalogue identity (`identityVersion`). If it does, bump `identityVersion` and record why.
If any catalog suite fails in Step 1, bump nothing. Stop and hand the failure back (Review Focus 1).

- [ ] **Step 3: Published bindings** (confirm with the user before running)

```
dotnet fsi build.fsx -- generate --only compiler-lib
dotnet fsi build.fsx -- generate --only node-lib
```

Report `git diff --stat` and the `DD002`/`DT005` counts for each. Core.TS has 1 tagged union and
Node has 4 (all with named, exported arms, so `DD002` is expected to be 0 there). Commit only after
the user agrees.

- [ ] **Step 4: Records**

Append to `generator-architecture.md`:

```markdown
## Tagged-union arms (2026-10-15, #158)

Both folds compare members with `Spec.sameShape`: one id, or plain anonymous objects, unions and
applications of one target that pair structurally. The checker gives every inline object literal
its own id, so the earlier id comparison dropped `owner: { name: string }` from a tagged fold and
refused the exclusive-arm fold over a shared inline `meta`. An exclusive-folded union's arm
declarations nest under the folded interface (`Choice.Meta`).

`detect-tagged-unions` writes the alias's JSDoc on the DU and each arm member's JSDoc, tags and
literal value on its case field; render writes them as one `<param>` per field, empty where a
field has no text (FS3390 rejects a case documenting some fields). A case with an optional or a
literal field has a `Create<Case>` static member, the arm interface's `Create` moved onto the DU:
an Emit writing the tag and literal fields, the remaining fields bound as parameters (`DT005`);
where `Create<Case>` is a case of the same union, the case has none (`DT006`). A same-named
member is FS0023. An anonymous arm's own declarations nest under `<Union>.Cases.<Case>`
(`synthesize-anonymous` reads the case names from `Spec.taggedCaseNames`, the source
`detect-tagged-unions` names cases from); a submodule spelled like the case itself is
unreachable from expressions (FS0039), and a union with a case named `Cases` keeps the arm-name
path. `drop-orphan-delegates` then prunes each unread interface named for an
anonymous arm of a declared DU (`DD002`), then every unread declaration under an arm root - a
DU's `Cases` module, a dropped arm's path, an exclusive-folded interface - transitively (`DD003`);
named arms, erased fallbacks and arms read elsewhere stay. The catalogue API hash is unchanged: <Step 2's recorded outcome>.
```

Replace `<Step 2's recorded outcome>` with the actual outcome from Step 2 before committing.

In `generator-type-mapping.md`, extend the `[<TypeScriptTaggedUnion "tag">]` row at `:146` with
"; `Create<Case>` builds a case with optional fields omittable and literal fields written". Add one
sentence to §4.5(2): an anonymous arm interface nothing reads is not declared.

In `footguns.md`, add under the generator section:

```markdown
- **Inline object literals never share a type id.** Two arms spelling `owner: { name: string }`
  identically carry two ids, so comparing member type ids calls them different. Compare with
  `Spec.sameShape` wherever "the same type" means "reads as one F# type".
- **A union case's XML doc documents all of its fields or none.** FS3390 checks `<param name>`
  on union cases against field names and requires every field once one is documented; an empty
  `<param>` satisfies it. A static member named like a case of its union is FS0023, which is why
  case helpers are `Create<Case>`.
- **A submodule spelled like a union case is unreachable from expressions.** With
  `type Recovery = | Aborted …` and `module Recovery = module Aborted = type Type`,
  `Recovery.Aborted.Type.User` resolves `Recovery.Aborted` to the case: FS0039, and FS1127 in a
  pattern. Type annotations still resolve, so a compile gate over declarations alone misses it.
  Case declarations nest under `Recovery.Cases.Aborted` instead.
```

- [ ] **Step 5: Full gate**

Run: `dotnet fsi build.fsx -- test --run-gate`
Expected: all suites green and the run gate passing. Report the pass counts.

- [ ] **Step 6: Commit**

```
rtk git add tests/Xantham.Generator.Tests/golden docs/.ai
rtk git commit -m "chore(goldens): regenerate for tagged-union arm removal (#158)"
```

---

## Self-review notes

- Spec coverage: union docs (T1), field docs and tags (T1), literal remark (T1), optional-field
  construction (T2), literal-field construction (T2), the issue's prune (T3), the issue's
  `Extract` keep test (T3, `Event2`), goldens (T3/T4), and the issue's optional pass rename
  (deferred, `//FOR-REVIEW` in T3).
- `FsTaggedCase.Create` and `FsTaggedField.{Docs,Tags,Literal}` names match across Tasks 1–3.
  `caseCreate` is used by render and by EmitGroups.
- Nested declarations move under `<Union>.Cases.<Case>` (Task 3). Residue kept on purpose: a
  union with a case named `Cases` keeps the arm path (`Shelf2.Size`) after its arms are dropped.
- Folds lost members spelled as identical inline objects (type-id comparison). Task 0 fixes both
  folds. Declarations only a dropped or folded arm read (`Run`'s `Result`/`Failure`, the second
  `Owner`, `Choice.Meta2`) are pruned in Task 4 under the arm roots.
- Accepted residue: a self-recursive declaration under an arm root reads itself and survives.
