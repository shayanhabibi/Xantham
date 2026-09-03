## 1. `null`, `undefined` and an absent property are one F# value

**Incidence: 8,951 sites** - `TR032` (null/undefined hoisted to option) 5,141 plus `MB003`
(optional member as option) 3,810, from `dotnet fsi build.fsx -- findings`.

## 2. `=` on a generated interface is a deep JavaScript comparison

**Incidence: 1,397 interface types** in the committed goldens - every generated interface, since
the comparison is a property of the type rather than of any one declaration.

[//]: # (ANSWER)

These are known limitations. It is up to the consumer to handle the difference between
`null` and `undefined` on the JS side, and when consuming objects from JS. Not us.

We should not care any further about this. Keep the finding as is, but do not assign
any work towards resolving something that can't be resolved.

This is similarly the case for #2.

[//]: # (ANSWER END)

---

## 3. An interface you implement in F# is a class instance, not a plain object

**Incidence: 1,245 abstract members with a function type**, across the 1,397 interface types in
the goldens. An interface declaring one of them is not plain data, so it gets no `[<ParamObject>]`
`Create` and has to be built by hand.

[//]: # (ANSWER)
1. Create the param objects helper anyway.

We can leave further consideration of this to a later date. The lack of `this` inside the function
can be handled later. Other than that, the object expression compiling to a class still works well enough.

[//]: # (ANSWER END)

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

[//]: # (ANSWER)

Again, this is a known limitation of erased unions. Not up to us to resolve.
There are helpers we can provide, but they can come later.

[//]: # (ANSWER END)

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

[//]: # (ANSWER)
Again, known limitation. Most users just `:?>` after having made sure themselves by some other means.
Not our issue.

[//]: # (ANSWER END)

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

[//]: # (ANSWER)

The one issue that we can actually easily do something about, and SHOULD do something about.

### Should Emit

```fsharp
[<Import("Budget", "fable-workaround-lab")>]
type Budget(spent: float) =
    member val spent: float = spent
    static member val limit: float = JS.undefined with get,set
```

```fsharp
[<Global("globalThis")>]
type Globals =
    static member val counter: float = JS.undefined with get,set
```

### What Fable 5 does with it

```fsharp
Budget.limit <- 5.
Globals.counter <- 5.
```
```ts
import { Budget } from "fable-workaround-lab"
Budget.limit = 5.0;
globalThis.counter = 5.0;
```

[//]: # (ANSWER END)
---

## Summary

The primary litmus test is cloudflare. Three is an extremely ambitious ask.
Also, most of the time, we don't end up creating these interfaces or functions, but instead have them handed to us,
or we are calling them. In those situations, we have access to fable helpers like `jsThis` etc.

It might be pertinent to temporarily retire THREE as a fixture.