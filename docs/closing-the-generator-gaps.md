# Closing the Generator Gaps: Technical Outline

**Context**: This document provides specific, actionable guidance for completing the Xantham SimpleGenerator to the point where it can replace Glutinum for Fidelity.CloudEdge.

## Understanding the Two-Level System

Before addressing any specific gap, the architecture must be understood correctly. There are **two distinct rendering levels**, and the `obj` fallbacks in each have different causes and different solutions.

### Level 1: TypeResolver (`resolvePattern`)

This is where types are **resolved and prerendered**. It produces a `Render` struct with two fields:

- `ShortCircuit`: A function `KeyPathKind -> TypeRefRender` that produces a minimal type reference (used when the type is referenced, and also serves as the cycle-break cache)
- `Full`: A lazy `TypeMaybePathedRender` that produces the complete type definition with all members

The ShortCircuit is cached **before** the Full is evaluated. This is the cycle-breaking mechanism: if type A references type B which references type A, the second encounter of A finds its ShortCircuit already cached and returns it without recursing.

### Level 2: `typeBuilder` (TypeResolver.fs, lines 529-563)

This is where `TypeRender` discriminated union cases are dispatched to produce actual F# AST nodes (via Fabulous.AST). The fallback at lines 556-562:

```fsharp
| TypeLiteral _
| Variable _
| TypeParameter _
| Function _
| TypeAlias _
| TypeReference _
| Tuple _ -> Ast.InterfaceEnd("None") {}
```

This is the **second bottleneck**. Even when TypeResolver correctly produces a `TypeRender.TypeLiteral` or `TypeRender.Tuple`, the `typeBuilder` doesn't know how to convert that into an F# type definition. It emits a placeholder empty interface instead.

### Level 3: CircuitBreaker (`renderPathType`)

This is the **third level**, used only when a type reference needs to be rendered inline (e.g., as a property type, parameter type, or return type) and the type is being visited recursively. The `obj` fallbacks here are acceptable for cycle-breaking; the real rendering happens in TypeResolver.

**Conclusion**: The work needed is concentrated in **Level 2** (`typeBuilder`). Level 1 (TypeResolver) already has resolution logic for most constructs. Level 3 (CircuitBreaker) is intentionally minimal.

---

## Gap 1: TypeLiteral → F# Type Definition

### Current State

**TypeResolver** (Level 1): Complete. Lines 377-386 produce `TypeRender.TypeLiteral` with a full `TypeLiteralRender` containing prerendered members.

**typeBuilder** (Level 2): Falls through to `Ast.InterfaceEnd("None") {}`.

**TypeLiteralRender.fs**: Has `collectMembersAsAbstracts`, `collectMembers`, `collectMembersAndOverloads` -- all functional.

### What to Build

TypeLiterals are anonymous object types in TypeScript (`{ foo: string; bar: number }`). In F#/Fable, these should become interfaces with abstract members, identical in structure to how named interfaces are rendered.

Add a case to `typeBuilder`:

```fsharp
| TypeLiteral typeLiteralRender ->
    let name =
        renderPath.Value
        |> KeyPath.popQualifier
        |> snd
        |> ValueOption.get
        |> getPascalName genCache
        |> unbox<Name>
    Ast.TypeDefn(Name.Case.valueOrModified name) {
        for memberWidget in
            TypeLiteralRender.collectMembersAsAbstracts genCache typeLiteralRender renderPath do
            memberWidget
    }
    |> TypeDefn.attribute (Ast.Attribute "Interface")
    |> TypeDefn.attribute (Ast.Attribute "AllowNullLiteral")
```

The pattern mirrors `InterfaceRender.renderInterfaceTypeDefn` exactly. TypeLiterals and interfaces are structurally identical in F#; the only difference is that TypeLiterals need a synthesized name (which the Path system already provides via the parent context).

### Estimated Scope

Small. The infrastructure is complete. This is wiring: dispatch the existing `TypeLiteralRender` through the same rendering path that interfaces use.

---

## Gap 2: Tuple → F# Type Abbreviation

### Current State

**TypeResolver** (Level 1): Complete. Lines 392-401 produce `TypeRender.Tuple` via `TupleRender.prerender` and `TupleRender.toTypeRender`.

**typeBuilder** (Level 2): Falls through to placeholder.

**TupleRender.fs**: Has `toTypeDefnAbbrev` (line 110) that produces a complete `Ast.Abbrev` with tuple element types. This function already exists and is unused.

### What to Build

Add a case to `typeBuilder`:

```fsharp
| Tuple tupleRender ->
    TupleRender.toTypeDefnAbbrev genCache tupleRender renderPath
```

That's it. The function already exists at `TupleRender.fs:110-120`. It produces a type abbreviation like:

```fsharp
type MyTuple = string * int * bool
```

For optional-element tuples, it wraps optional elements in `option`:

```fsharp
type MyTuple = string * int * bool option
```

### Estimated Scope

Trivial. One line in `typeBuilder`. The rendering function is already written.

### Additional Consideration

There is a `failwith "todo - tuple for type alias"` at `TypeAliasRender.fs:365`. This blocks tuple types that appear as the underlying type of a type alias. Remove the `failwith` and the existing code below it will work; the prerender, `toTupleRender`, and `toTypes` calls are already written on lines 366-382.

---

## Gap 3: TypeAlias → F# Type Definition

### Current State

**TypeResolver** (Level 1): Complete. `TypeAliasRender.createRender` (lines 79-438) handles every underlying type variant: TypeLiteral, Interface, Class, Union, Intersection, TypeReference, Enum, TypeParameter, Tuple, Conditional, Literal, EnumCase.

**typeBuilder** (Level 2): Falls through to placeholder.

**TypeAliasRender.fs**: The `prerender` function (lines 13-74) produces a `TypeAliasRender` struct with `Name`, `TypeParameters`, and `UnderlyingType` (a `TypeAliasUnderlyingType` DU).

### What to Build

The `TypeAliasUnderlyingType` DU determines how the alias renders:

```fsharp
type TypeAliasUnderlyingType =
    | Interface of InterfaceRender
    | Class of ClassRender
    | TypeLiteral of TypeLiteralRender
    | Union of UnionRender
    | TypeReference of TypeReferenceRender
    | CallSignature of CallSignatureRender
```

Each case maps to a different F# output form:

| UnderlyingType | F# Output |
|:---------------|:----------|
| `Interface` | `type Alias = InterfaceName` (abbreviation) or inline interface if anonymous |
| `Class` | `type Alias = ClassName` (abbreviation) |
| `TypeLiteral` | Interface type definition with the alias name |
| `Union` | String enum or DU with `[<StringEnum>]` |
| `TypeReference` | `type Alias = ReferencedType` (abbreviation), possibly with type arguments |
| `CallSignature` | Delegate type or function type abbreviation |

Add a case to `typeBuilder`:

```fsharp
| TypeAlias typeAliasRender ->
    let name = Name.Case.valueOrModified typeAliasRender.Name
    let typeParams =
        typeAliasRender.TypeParameters
        |> Array.map (TypeParameterRender.toWidget >> Ast.TyparDecl)
    match typeAliasRender.UnderlyingType with
    | TypeAliasUnderlyingType.Interface interfaceRender ->
        InterfaceRender.renderInterfaceTypeDefn genCache interfaceRender renderPath
        // Override the name with the alias name
    | TypeAliasUnderlyingType.TypeLiteral typeLiteralRender ->
        Ast.TypeDefn(name) {
            for memberWidget in
                TypeLiteralRender.collectMembersAsAbstracts genCache typeLiteralRender renderPath do
                memberWidget
        }
        |> TypeDefn.attribute (Ast.Attribute "Interface")
    | TypeAliasUnderlyingType.Union unionRender ->
        EnumRender.toTypeDefn unionRender
    | TypeAliasUnderlyingType.TypeReference typeRefRender ->
        let targetType =
            TypeRefRender.toWidget genCache renderPath typeRefRender.Type
        let targetType =
            match typeRefRender.TypeArguments with
            | [||] -> targetType
            | args ->
                Ast.AppPrefix(targetType,
                    args |> Array.map (TypeRefRender.toWidget genCache renderPath))
        Ast.Abbrev(name, targetType)
    | TypeAliasUnderlyingType.CallSignature callSigRender ->
        // Render as delegate or function abbreviation
        CallSignatureRender.toTypeDefn name callSigRender renderPath
    | TypeAliasUnderlyingType.Class classRender ->
        ClassRender.renderTypeDefn genCache classRender renderPath
```

### Estimated Scope

Moderate. The dispatch logic needs to handle 6 underlying type variants. However, each variant delegates to an existing render module. The work is orchestration, not new rendering logic.

### Key Detail

`TypeAliasRender.createRender` already handles the complex union decomposition (literal enums, erased unions, mixed cases) at lines 189-316. The resolution is done; only the final AST emission is missing in `typeBuilder`.

---

## Gap 4: Variable → F# Let Binding or Val

### Current State

**TypeResolver** (Level 1): Complete. Lines 265-279 produce `TypeRender.Variable`.

**typeBuilder** (Level 2): Falls through to placeholder.

### What to Build

TypeScript `declare var` / `declare const` / `declare let` become F# values. In a Fable context, these are typically:

```fsharp
[<Import("variableName", "module-name")>]
let variableName: VariableType = jsNative
```

Add a case to `typeBuilder`:

```fsharp
| Variable variableRender ->
    VariableRender.toLetBinding genCache variableRender renderPath
```

The `VariableRender` struct contains the type reference; the rendering needs to produce a `let` binding with appropriate Fable import attributes. Check whether `VariableRender` already has a `toLetBinding` or similar; if not, implement one following the pattern in `FunctionRender` (which faces the same challenge of producing a member-level binding from a type-level render).

### Estimated Scope

Small. The variable's type is already resolved; this is mostly about emitting the correct Fable interop attributes.

---

## Gap 5: Function → F# Let Binding or Delegate

### Current State

**TypeResolver** (Level 1): Complete. Lines 318-375 produce `TypeRender.Function` with full parameter and return type resolution. The ShortCircuit already renders correct function signatures (curried for <3 params, delegate for >=3).

**typeBuilder** (Level 2): Falls through to placeholder.

### What to Build

Top-level `declare function` in TypeScript becomes either:

1. A `let` binding with a function type (for simple signatures)
2. A delegate type definition (for complex signatures used as callbacks)

```fsharp
// Simple:
[<Import("fetchData", "@cloudflare/workers")>]
let fetchData (url: string) (options: RequestInit): JS.Promise<Response> = jsNative

// Delegate:
type FetchHandler = delegate of url: string * options: RequestInit -> JS.Promise<Response>
```

Add a case to `typeBuilder`:

```fsharp
| Function functionRender ->
    FunctionRender.toTypeDefnOrBinding genCache functionRender renderPath
```

### Estimated Scope

Moderate. The parameter rendering and return type resolution are complete. The gap is deciding between delegate vs let binding and emitting the correct AST. `FunctionRender.prerender` already exists; it needs a counterpart `toTypeDefn` or `toBinding`.

---

## Gap 6: TypeParameter → F# Type Parameter Declaration

### Current State

**TypeResolver** (Level 1): Complete. Lines 282-294 render type parameters as `'T` style type variable references.

**typeBuilder** (Level 2): Falls through to placeholder.

### Analysis

TypeParameters should not appear as top-level type definitions. They appear as constraints and declarations on interfaces, classes, functions, and type aliases. If a `TypeRender.TypeParameter` reaches `typeBuilder`, it indicates an unexpected state in the module tree construction.

### Recommendation

This is likely a non-issue for practical output. TypeParameters are consumed by the renderers that own them (InterfaceRender, ClassRender, etc.) via `TypeParameterRender.prerender`. If instances leak to `typeBuilder`, add a guard that skips them:

```fsharp
| TypeParameter _ -> ()  // Consumed by parent type, not rendered standalone
```

### Estimated Scope

Trivial. This is either a no-op skip or a diagnostic investigation into why TypeParameters appear at the top level.

---

## Gap 7: TypeReference → F# Type Abbreviation

### Current State

**TypeResolver** (Level 1): Complete. Lines 127-152 resolve TypeReferences with full type argument handling.

**typeBuilder** (Level 2): Falls through to placeholder.

### Analysis

A standalone `TypeRender.TypeReference` reaching `typeBuilder` means a type reference appeared at a position that requires a type definition. This happens when a type alias resolves to a type reference with arguments (e.g., `type MyArray = Array<string>`).

### What to Build

```fsharp
| TypeReference typeRefRender ->
    let name =
        renderPath.Value
        |> KeyPath.popQualifier
        |> snd
        |> ValueOption.get
        |> getPascalName genCache
        |> unbox<Name>
    let targetType =
        TypeRefRender.toWidget genCache renderPath typeRefRender.Type
    let targetType =
        match typeRefRender.TypeArguments with
        | [||] -> targetType
        | args ->
            Ast.AppPrefix(targetType,
                args |> Array.map (TypeRefRender.toWidget genCache renderPath))
    Ast.Abbrev(Name.Case.valueOrModified name, targetType)
```

### Estimated Scope

Small. This is a type abbreviation emission.

---

## Prioritized Work Order

Based on frequency in `@cloudflare/workers-types` and dependency relationships:

### Phase 1: Quick Wins (1-2 days)

1. **Tuple in typeBuilder**: One-line fix; `toTypeDefnAbbrev` already exists.
2. **Tuple in TypeAliasRender**: Remove the `failwith` at line 365.
3. **TypeParameter in typeBuilder**: Skip or guard; not a real gap.

### Phase 2: High-Impact Completions (3-5 days)

4. **TypeLiteral in typeBuilder**: Wire through `collectMembersAsAbstracts` following the InterfaceRender pattern. Produces interfaces for all anonymous object types.
5. **TypeAlias in typeBuilder**: Dispatch on `TypeAliasUnderlyingType` variants. Each variant delegates to existing infrastructure. This is the highest-impact single item because type aliases are pervasive in the Cloudflare SDK.
6. **TypeReference in typeBuilder**: Type abbreviation emission. Straightforward once TypeAlias is working, as many TypeAlias cases resolve to TypeReference internally.

### Phase 3: Remaining Constructs (2-3 days)

7. **Variable in typeBuilder**: Let bindings with Fable import attributes.
8. **Function in typeBuilder**: Delegate type definitions or let bindings. More complex due to the signature rendering decisions (curried vs tupled vs delegate).

### Phase 4: CircuitBreaker Improvements (1-2 days)

After the typeBuilder gaps are closed, revisit CircuitBreaker for improved inline type references:

9. **TypeParameter in CircuitBreaker**: Render as `'T` using the type parameter name from the `PathTypeParameter`.
10. **Tuple in CircuitBreaker**: Render as inline tuple type `string * int * bool` using element types from the path.
11. **TypeLiteral in CircuitBreaker**: Render as `obj` or the path name; inline expansion is impractical in the circuit breaker context.

These improve the quality of type references that appear in member signatures during cycle-breaking, but they are polish items; the Phase 1-3 work is what closes the functional gap.

---

## Validation Strategy

After each phase:

1. Run the extractor against `@cloudflare/workers-types`:
   ```bash
   npm run start -- node_modules/@cloudflare/workers-types/index.d.ts
   ```

2. Run the SimpleGenerator against the extracted JSON.

3. Count the `InterfaceEnd("None")` placeholders in the output. This is the direct measure of `typeBuilder` fallbacks.

4. Attempt `dotnet build` on the generated output (within a test Fable project). Compilation errors reveal type ordering issues or attribute gaps that need attention.

5. Compare generated type names and member counts against the Glutinum output from Fidelity.CloudEdge's `Generated.fs` (714KB). This gives a concrete diff of coverage.

The target: zero `InterfaceEnd("None")` placeholders, and type coverage parity with Glutinum's output (modulo the intentional `obj` from cycle-broken references, which Xantham handles more cleanly through its CircuitBreaker naming).
