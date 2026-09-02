# Fable 5 workarounds for consumers of generated bindings

Six places where a generated binding gives you less than the `.d.ts` declared, because of what
Fable 5 emits or what its runtime library supports. Each entry carries the TypeScript, the F# the
generator emits for it, the JavaScript Fable produces, and F# you can write instead.

Every claim here was compiled and run against this repository's pins - the `fable` tool 5.0.0,
`Fable.Core` 5.2.0, `fable-library-js` 5.0.0 - and every workaround is executed by the run gate
(`tests/Xantham.Generator.RunGate`) against `tests/fixtures/fable-workaround-lab/index.js`. The
two FABLE warnings that gate prints are entries 4 and 5 reporting themselves.

## Scope

Three different things make a binding lossy, and only the second is here.

1. **F# has no form.** Rank-2 types, an abbreviation carrying an unused type variable, structural
   subtyping, singleton literal types. Fable changes none of these.
2. **Fable 5's compilation model.** The loss is in the emitted JavaScript or in what the Fable
   runtime library supports.
3. **The generator has not got to it.** A gap, not a limit.

The last sections name the candidates sorted into 1 and 3, with the reason.

Entries are ordered by corpus incidence, largest first, over the 23 fixtures the generator was
gated against before this document; `fable-workaround-lab` is written to demonstrate these issues
and is excluded from every count. Each count names its own measure, because the measures differ:
some losses are counted by the finding catalogue, others by the shapes present in the committed
goldens.

---

## 1. `null`, `undefined` and an absent property are one F# value

**Incidence: 8,951 sites** - `TR032` (null/undefined hoisted to option) 5,141 plus `MB003`
(optional member as option) 3,810, from `dotnet fsi build.fsx -- findings`.

### What TypeScript declares

```ts
export interface Slot {
    value: string | null;
}

/** Three slots: a string, an explicit `null`, and an absent property. */
export declare function slots(): Slot[];
```

### What the binding emits

```fsharp
[<Interface>]
type Slot =
    abstract value: string option with get, set
    [<ParamObject; Emit("$0")>]
    static member Create (?value: string) : Slot = jsNative
```

### What Fable 5 does with it

`None` compiles to `undefined`, and `Option.isSome` is a loose `!= null` test, so all three
JavaScript states - `"a"`, `null`, and no property at all - arrive as `Some "a"`, `None` and
`None`. Reading is one-way: JavaScript that distinguishes them, as `slots()` does, sees a
distinction the F# side no longer holds. Writing is one-way in the other direction: a `Create`
call with the argument omitted emits `{}`, so a declaration asking for `null` receives an absent
property.

### The workaround

A value that is `None` to F# and `null` to JavaScript:

```fsharp
let asNull: string option = emitJsExpr () "null"

// Reaches the property as `null`, which the `.d.ts` declared and `None` cannot express.
FableWorkaroundLab.Slot.Create(?value = asNull)

// For a member with no synthesized `Create`, build the literal:
let slot: FableWorkaroundLab.Slot = !!createObj [ "value" ==> (null: obj) ]
```

Reading direction: ask JavaScript, since F# no longer holds the answer.

```fsharp
let isExplicitNull (slot: FableWorkaroundLab.Slot) : bool =
    emitJsExpr slot "$0.value === null"
```

### What it costs

`asNull` reads back as `None` at every later F# use, so the distinction survives exactly as long
as the value stays inside the expression that built it. `isExplicitNull` names the property as a
string, so a rename in the `.d.ts` moves past it silently.

---

## 2. `=` on a generated interface is a deep JavaScript comparison

**Incidence: 1,397 interface types** in the committed goldens - every generated interface, since
the comparison is a property of the type rather than of any one declaration.

### What TypeScript declares

```ts
export interface Ok {
    value: string;
}

/** A fresh object every call. */
export declare function fresh(): Ok;

/** A fresh object every call, holding a reference to itself. */
export declare function cyclic(): Ok;
```

### What the binding emits

```fsharp
[<Interface>]
type Ok =
    abstract value: string with get, set
```

### What Fable 5 does with it

`a = b` compiles to `equals(a, b)` from `fable-library-js/Util.js`, which walks both objects
property by property. Two distinct JavaScript objects with the same fields compare equal, and a
pair of self-referencing objects recurses until the stack is exhausted:

```
RangeError: Maximum call stack size exceeded
    at equalObjects (.../fable-library-js.5.0.0/Util.js:345:26)
    at equals      (.../fable-library-js.5.0.0/Util.js:385:68)
```

The same F# compiles to reference equality on .NET, so this behaviour appears only after Fable.

### The workaround

```fsharp
// Identity: compiles to `a === b`.
obj.ReferenceEquals(first, second)

// A field-by-field comparison you chose the fields for.
let sameSlot (a: FableWorkaroundLab.Slot) (b: FableWorkaroundLab.Slot) = a.value = b.value
```

### What it costs

`obj.ReferenceEquals` answers a different question from the one `=` looks like it asks, so a
reader has to know why it is there. A hand-written field comparison is fixed at the fields you
listed and does not follow the `.d.ts`.

Structural containers inherit the problem: `List.contains`, `Set`, `Map` keys, `distinct` and
`groupBy` all reach `equals`, so a collection built over a cyclic JavaScript object throws inside
the collection operation.

---

## 3. An interface you implement in F# is a class instance, not a plain object

**Incidence: 1,245 abstract members with a function type**, across the 1,397 interface types in
the goldens. Wave four lane O carries a method into the `[<ParamObject>]` `Create` as a
delegate-typed parameter, which gains 176 declarations a `Create` they had none of; 190 still get
none, each reported under `SP003`. The rest of this section holds for those 190, and the cost
below is what the `Create` inherits.

### What TypeScript declares

```ts
export interface Listener {
    name: string;
    notify(count: number): string;
}

/** `JSON.stringify` of the listener, then its own enumerable keys, then `notify(1)`. */
export declare function invite(listener: Listener): string;
```

### What the binding emits

```fsharp
type Listener =
    abstract name: string with get, set
    abstract notify: count: float -> string
```

### What Fable 5 does with it

An object expression compiles to `new (class { get name() {...} notify(count) {...} })()`. Member
access works, because the members are on the prototype. Everything that reads *own* properties
finds an empty object: `invite` above reports `{}` for `JSON.stringify` and `[]` for
`Object.keys`. Object spread, `Object.assign`, `structuredClone` and any JavaScript that copies a
configuration object behave the same way.

### The workaround

An anonymous record, cast to the interface:

```fsharp
let listener: FableWorkaroundLab.Listener =
    !!{| name = "lit"
         notify = System.Func<float, string>(fun count -> $"lit:{count}") |}
```

`invite listener` reports `{"name":"lit"}`, own keys `name,notify`, and `notify(1)` returning
`lit:1`.

### What it costs

`!!` is an unchecked cast, so the compiler stops checking that the record covers the interface: a
missing member, a misspelled one, or a wrong argument count is a runtime `undefined is not a
function`. A method becomes a function-valued property, which loses `this` - write the record's
own fields into the closure rather than reading `this` inside `notify`.

### What wave four changed

`Create` carries a method as the delegate a function-valued property of the same signature
already carried, so the record above is reached under the typechecker rather than past it:

```fsharp
let listener =
    FableWorkaroundLab.Listener.Create("lit", System.Func<float, string>(fun count -> $"lit:{count}"))
```

`invite listener` reports the same three values, and a missing or mistyped member is now a
compile error. `SP002` reports every method carried in. The delegate still receives no `this`,
and a method that binds type parameters of its own generalises them onto `Create`, so one call
picks one instantiation - both left for a later wave.

The cast stays the route for the 190 declarations `SP003` reports: an index signature (132),
more members than the 24-parameter `Create` budget (32), an overloaded method (25), or no
members of its own (1).

---

## 4. An erased union arm that is an interface is never selected

**Incidence: 1,101 `U2`-`U9` sites** in the committed goldens. Of the 847 whose arguments are
written without nesting, 720 end in an arm that is not a Fable-testable primitive and 127 do not.

### What TypeScript declares

```ts
export interface Ok { value: string }
export interface Err { reason: string }
export type Outcome = Ok | Err;

/** Returns an `Err` when `fail`, an `Ok` otherwise. */
export declare function run(fail: boolean): Outcome;
```

### What the binding emits

```fsharp
type Outcome = U2<Err, Ok>

[<Import("run", "fable-workaround-lab")>]
static member run (fail: bool) : Outcome = jsNative
```

### What Fable 5 does with it

`U2` is `[<Erase>]`, so a case match is a type test on the arm's type. Fable tests a primitive
with `typeof` and an array with `Array.isArray`. An interface this run declares has no runtime
witness, so the test folds to the constant `false` and the branch is deleted. What remains is the
other branch, taken unconditionally:

```
warning FABLE: Cannot type test (evals to false): FableWorkaroundLab.Ok
```

The match below reads `Err` for the `Ok` value that `run false` returns, and for every other value
as well:

```fsharp
match FableWorkaroundLab.Exports.run false with
| U2.Case1 err -> ()   // always this one
| U2.Case2 ok -> ()    // unreachable
```

It is a warning, so the binding compiles and the build passes.

### The workaround

Discriminate on something JavaScript can see, then cast:

```fsharp
let readOutcome (outcome: FableWorkaroundLab.Outcome) =
    if emitJsExpr outcome "\"value\" in $0" then
        Choice1Of2(unbox<FableWorkaroundLab.Ok> outcome)
    else
        Choice2Of2(unbox<FableWorkaroundLab.Err> outcome)
```

A union whose arms are all primitives - 127 of the measured sites - matches as written and needs
none of this.

### What it costs

The discriminator is yours to keep correct: nothing checks that `"value"` separates the arms, and
an arm added to the `.d.ts` compiles into a `U3` whose third case the function folds into one of
the two it knows. Writing a union stays exact - `!^value` upcasts with no test - so only the read
direction needs the helper.

---

## 5. A downcast to a generated interface is `false`

**Incidence: 208 `inherit` edges** in the committed goldens (`SI005`, 80 findings). Every
generated interface can be the target of a downcast; the inherit edges are where the `.d.ts`
gives a reason to try one.

### What TypeScript declares

```ts
export interface Shape { area: number }
export interface Circle extends Shape { radius: number }

/** One `Shape` that is a `Circle` and one that is not. */
export declare function shapes(): Shape[];
```

### What the binding emits

```fsharp
[<Interface>]
type Circle =
    inherit Shape
    abstract radius: float with get, set
    abstract area: float with get, set
```

### What Fable 5 does with it

The `inherit` edge is real to F# and erased in JavaScript, so an upcast is free and correct - the
run gate proves an upcast is the identical object. The downcast has nothing to test: the emitted
object carries no F# type tag, and `shape :? Circle` compiles to the literal `false` with the same
`Cannot type test` warning. `match shape with :? Circle as c -> ... | _ -> ...` therefore always
takes the second branch.

### The workaround

Narrow on a member the extension adds:

```fsharp
let asCircle (shape: FableWorkaroundLab.Shape) : FableWorkaroundLab.Circle option =
    if emitJsExpr shape "\"radius\" in $0" then Some !!shape else None
```

Where the JavaScript class is imported and `EmitConstructor`-bound, `instanceof` is available and
is stronger, because it follows the prototype chain:

```fsharp
let nodeClass: obj = import "Node" "inherit-lab"
let isNode (value: obj) : bool = emitJsExpr (value, nodeClass) "$0 instanceof $1"
```

### What it costs

Presence of a property is a weaker claim than the type test it replaces: an unrelated object
carrying `radius` passes. `instanceof` is exact but needs a class to test against, so it does not
apply to an interface with no runtime counterpart. Both name the JavaScript side as a string.

---

## 6. A settable static and a mutable global bind read-only

**Incidence: 2 sites** - `SC003` (settable static emitted read-only), both in `statics-lab`, none
in the four npm rungs. The mutable-global half is counted by no finding code.

### What TypeScript declares

```ts
export declare class Budget {
    constructor(spent: number);
    readonly spent: number;
    /** Assignable from JavaScript. */
    static limit: number;
}
```

and, in a globals package, `declare var counter: number;`.

### What the binding emits

```fsharp
[<Import("Budget.limit", "fable-workaround-lab")>]
static member limit: float = jsNative
```

Get-only. The global reads the same way, through `[<Global("counter")>]`.

### What Fable 5 does with it

A setter on an `[<Import>]` or `[<Global>]` static member compiles the assignment as a *call*.
Given a member declared `with get () = jsNative and set (_: float) = jsNative`,
`Counter.tick <- 8.0` emits

```js
Counter.tick(8);
```

which is a `TypeError` at run time, and `Exports.counter <- 5.0` emits `counter(5)`. The generator
emits get-only rather than a setter that compiles to that.

### The workaround

Reach the object the static hangs off, and set the property on it:

```fsharp
let budgetClass: obj = import "Budget" "fable-workaround-lab"
budgetClass?limit <- 250.0
// FableWorkaroundLab.Budget.limit now reads 250.0

emitJsStatement 55.0 "globalThis.counter = $0"
// GlobalsLab.Exports.counter now reads 55.0
```

Both emit the assignment the declaration asked for: `Budget.limit = 250` and
`globalThis.counter = 55`.

### What it costs

`?` is untyped, so the property name and the value type are both unchecked - `budgetClass?limit <-
"wrong"` compiles. The name is a string on the F# side, so a rename in the `.d.ts` moves past it.
Reading still goes through the binding, so only the write loses its type.

---

## Also Fable's, not written up

**`symbol` and `unique symbol` have no `Fable.Core` 5.2.0 binding** (`TR041` 5, `TR042` 6; 11
sites). The binding widens to `obj`, and the run gate proves a real JavaScript symbol survives the
round trip through it - `flags-lab` reads `typeof` back as `"symbol"`. The workaround and the
binding are the same thing, so there is nothing to show.

---

## Category 1: F# has no form

Rejected, with the count each would otherwise have ranked at.

| Finding | Sites | Why it is not Fable's |
| --- | --- | --- |
| `TR006` string literal widened to `string` | 1,212 | F# has no singleton literal type. A union of them becomes a `StringEnum`; a lone one has nowhere to go. |
| `TR008` `any` to `obj`, `TR009` `unknown` to `obj` | 714 | No top type that narrows. |
| `TP002` constraint dropped | 276 | F# constraints are nominal; a structural `extends` has no F# spelling. |
| `TR045`/`TR046` conditional types | 248 | No type-level conditional. |
| `SA002` phantom computation | 196 | An abbreviation may not carry a type variable its right-hand side does not use. |
| `SI003` intersection flattened | 113 | No structural intersection type. |
| `TR037` template literal to `string` | 68 | No template-literal type. |
| `SC002` static dropped on a name collision | 3 | F# admits a static beside an instance member only for method-over-method (FS0441, FS0434, FS3214). |

## Category 3: the generator has not got to it

| Finding | Sites | Why it is a gap |
| --- | --- | --- |
| `MB002` symbol-keyed member dropped | 12 | `[<Emit("$0[Symbol.iterator]()")>]` on a renamed member reaches it. Verified against these pins: a `[Symbol.toStringTag]` read and a `[Symbol.iterator]()` call both landed. F# has no identifier form for the key, so any emission must rename - a generator decision, not a Fable limit. |
| `SI001` hybrid loses call signatures | 10 | `docs/plans/generator-type-mapping.md` §4.4 already names the mapping: an `[<Emit "$0($1...)">]` `Invoke` member beside the properties. |

## Suspected, unproven

**`TR024`, extra type arguments dropped at a `Fable.Browser.*` binding** (147 sites). The pinned
Fable binding declares fewer type parameters than the TypeScript `lib` type it stands for, so the
arguments have nowhere to go. The loss is in the shipped binding packages rather than in what
Fable compiles, and the two are close enough that sorting it confidently would need a reading of
which arities the `Fable.Browser.*` family fixed deliberately. It is category 2 if the package set
counts as Fable, category 3 if the generator could bind the type itself; the count is large enough
that the question is worth settling.

**Nested options.** `Some None` compiles to a value whose `JSON.stringify` is `undefined` while
`Option.isSome` is `true`, which would be a trap for a doubly-optional member. No golden in the
corpus contains `option option`, so the case does not arise and is not written up.
