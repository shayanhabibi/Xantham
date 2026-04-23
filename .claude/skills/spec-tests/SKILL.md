---
disable-model-invocation: false
user-invocable: true
---

# Spec Test Skill

You are helping write spec tests for the Xantham project using **Expecto** (dotnet) or **Fable.Mocha** (Fable/JS). Follow these patterns exactly.

## Core Philosophy

Spec tests should make the **input → output contract immediately readable**. Every test case should answer "given this input, I expect this output" without the reader needing to trace boilerplate. Achieve this through:

1. Small private helpers that name the construction pattern, not the mechanics
2. Inline operators scoped inside `testList` that pair input/output visually
3. `testTheory` for tabular cases over the same function
4. `Flip.Expect.equal` to keep data flowing left-to-right

---

## Pattern 1: Module-level helpers → `(actual, expected)` tuple

For tests where constructing the input is multi-step, write a helper that returns `(actual, expected)` so the assertion line is always `||> Flip.Expect.equal ""`.

```fsharp
// Helper produces (actual, expected) — caller just pipes to ||> Flip.Expect.equal
let testRender (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender ctx ref
    |> testTypeRef expectedTypeText

// Usage — the assertion is always the same one-liner
testCase "noop" <| fun _ ->
    primitive TypeKindPrimitive.Void
    |> testRender "unit"
    ||> Flip.Expect.equal ""
```

Keep helper names short and verb-first. The expected value is always the **first** argument so it reads "render expecting `X`".

---

## Pattern 2: Scope-local inline operators for tabular input/output

Inside a `testList`, define `inline` operators that encode the transformation intent. This makes a column of pairs scannable without repeating the constructor or expected wrapping on every row.

```fsharp
testList "Name.Case" [
    // (==>) means: original transforms to a Modified name
    // (<=>) means: original is preserved as a Source name
    let inline (==>) a b = a, modifiedCase a b
    let inline (<=>) a b = a, sourceCase b

    testTheory "Case.pascal" [
        "pascal_case"  ==> "PascalCase"
        "pascal-case"  ==> "PascalCase"
        "pascalCase"   ==> "PascalCase"
        "PASCAL_CASE"  <=> "PASCAL_CASE"   // upper snake case preserved
        "_pascal_case" ==> "_pascalCase"
    ] <| fun (original, expected) ->
        Name.Pascal.create original |> Flip.Expect.equal "" expected
]
```

Rules for operators:
- Define them with `let inline` at the **top of the nearest enclosing `testList`**, not at module scope.
- Choose glyphs that hint at the relationship: `==>` for transformation, `<=>` for identity/preservation, `->` for mapping, etc.
- The operator produces the tuple `(input, expected)` consumed by the `testTheory` body.

---

## Pattern 3: `testTheory` for multiple cases of the same function

Prefer `testTheory` over multiple `testCase` blocks whenever you are calling the same function with different inputs.

```fsharp
testTheory "Case.camel" [
    "camel_case"   ==> "camelCase"
    "camel-case"   ==> "camelCase"
    "camelCase"    <=> "camelCase"
    "CAMEL_CASE"   <=> "CAMEL_CASE"
    "_camel_case"  ==> "_camelCase"
] <| fun (original, expected) ->
    Name.Camel.create original |> Flip.Expect.equal "" expected
```

The body is one line. All variance lives in the data rows.

---

## Pattern 4: Private construction helpers at module top

Extract repeated DU construction into named private helpers. Keep names as short as the semantics allow.

```fsharp
// Good — names the concept, not the mechanics
let private modified original modified_ = Name.Modified(original, modified_)
let private modifiedCase<[<Measure>] 'U> original modified: Name<'U> =
    Name.Modified(original, modified) |> Case.addMeasure
let private source original = Name.Source original
let private sourceCase<[<Measure>] 'U> original: Name<'U> =
    Name.Source original |> Case.addMeasure

// Good — short alias for commonly used construction
let inline private primitive p = ResolvedType.Primitive p
```

Never inline the DU constructor call into a test row if it appears more than twice — always extract it.

---

## Pattern 5: Flip convention

Always use `Flip.Expect.equal` so the value under test flows left-to-right and the assertion is at the end:

```fsharp
// Single value — pipe into Flip
someValue |> Flip.Expect.equal "message" expected

// Tuple from helper — use ||>
someHelper expectedText input
||> Flip.Expect.equal "message"
```

Never write `Expect.equal "msg" expected actual` (wrong argument order — hard to read in diffs).

---

## Pattern 6: Builder chains for structured inputs

For complex inputs (call signatures, unions, interfaces), use fluent builder chains. Each step should be on its own line, indented one level from the previous.

```fsharp
testCase "primitive parameter and return type" <| fun _ ->
    primitive TypeKindPrimitive.String
    |> CallSignature.create
    |> CallSignature.withParameters [
        primitive TypeKindPrimitive.Integer
        |> Parameter.create "para"
    ]
    |> List.singleton
    |> CallSignature.wrap
    |> TypeLiteral.addMember
    |> funApply TypeLiteral.empty
    |> TypeLiteral.wrap
    |> testRender "int -> string"
    ||> Flip.Expect.equal ""
```

The final two lines are always `|> testRender "expected"` then `||> Flip.Expect.equal ""`.

---

## Pattern 7: Inline comments on non-obvious cases

Add a short inline comment on a data row **only** when the expected output would surprise a reader. One line max.

```fsharp
"PASCAL_CASE" <=> "PASCAL_CASE"   // upper snake case is preserved
```

Never comment the obvious ones.

---

## Handling unknown expected outputs

When the expected output for a given input is not known or not obvious, use one of two strategies depending on whether the user is available to answer.

### Strategy A: Ask

If the behaviour is ambiguous or semantically non-trivial (e.g. a name casing edge case, a type-resolution rule), **ask before writing the test**. Provide the concrete input and ask what the expected output should be:

> "What should `Name.Pascal.create` produce for `__leading_double_under`? I want to make sure the test reflects the intended contract."

Only ask about cases that genuinely need a decision. Don't ask about cases you can derive from the existing tests or from the source code.

### Strategy B: Placeholder

If the user is not available to answer, or if there are many uncertain cases, write the test with a `TODO` placeholder and mark it pending so it compiles and is visible but does not fail the suite.

For `testTheory` rows, use a recognisable sentinel and add a `// TODO` comment:

```fsharp
testTheory "Case.pascal" [
    "pascal_case"       ==> "PascalCase"
    "__leading_double"  ==> "???"          // TODO: confirm expected output
] <| fun (original, expected) ->
    Name.Pascal.create original |> Flip.Expect.equal "" expected
```

For a `testCase` where the whole expected value is unknown, use `ptestCase` (pending) so the suite reports it as skipped rather than failing:

```fsharp
ptestCase "edge case with leading double underscore" <| fun _ ->
    // TODO: confirm what Name.Pascal.create should produce here
    Name.Pascal.create "__leading_double"
    |> Flip.Expect.equal "" "???"
```

For a `testList` group where all cases are uncertain, use `ptestList`:

```fsharp
ptestList "Name.Pascal edge cases" [
    // TODO: confirm expected outputs for these edge cases
    testTheory "leading underscores" [
        "__foo"  ==> "???"
        "___bar" ==> "???"
    ] <| fun (original, expected) ->
        Name.Pascal.create original |> Flip.Expect.equal "" expected
]
```

Rules:
- Use `"???"` as the placeholder sentinel — it is visually obvious and will fail loudly if a pending test is accidentally promoted.
- Always pair a placeholder with a `// TODO: confirm ...` comment describing what needs to be decided.
- Prefer `ptestCase`/`ptestList` over commenting out — commented-out tests are invisible; pending tests show up in the run summary.
- Replace every placeholder before marking the task done; pending tests are scaffolding, not a finished spec.

---

## Checklist before finishing

- [ ] All `testTheory` rows use a scope-local operator — no naked tuple literals
- [ ] Every module-level helper returns `(actual, expected)` or a typed `Name<'U>`
- [ ] Every assertion uses `Flip.Expect.equal` or `||> Flip.Expect.equal`
- [ ] No DU constructor appears more than twice without a named helper
- [ ] Builder chains are one-step-per-line with consistent indentation
- [ ] Operators are defined with `let inline` inside the nearest `testList`, not at module scope
- [ ] Test names are short and describe the scenario, not the function under test
