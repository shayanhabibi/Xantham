# Xantham.Fable.Core

A small Fable utility library providing F# representations of TypeScript type-system idioms that have no direct equivalent in standard F#. All types are erased at runtime — they exist only for the compiler and carry zero overhead in the emitted JavaScript.

---

## Contents

- [`keyof<'T>`](#keyoft) — TypeScript `keyof T`
- [`typekeyof<'T, 'ReturnType>`](#typekeyoft-returntype) — TypeScript `keyof T` with value-type retention
- [`proptypekey<'T, 'ReturnType>` and `proptypelock<'T>`](#proptypekeyt-returntype--proptypelockt) — TypeScript `T[keyof T]` (union of all property value types)
- [`PropertyRecord<'T, 'K>`](#propertyrecordt-k) — index-signature objects
- [Active patterns: `KeyIs` / `TypeKeyIs`](#active-patterns-keyis--typekeyis)
- [Helper modules: `KeyOf` / `TypeKeyOf`](#helper-modules-keyof--typekeyof)

---

## `keyof<'T>`

### TypeScript idiom

```typescript
type Keys = keyof MyObj   // "field1" | "field2" | ...
function get<T>(obj: T, key: keyof T): unknown { ... }
```

`keyof T` produces a string-literal union of the property names of `T`. It is used to constrain a value to only valid property names of a given type.

### F# representation

`keyof<'T>` is an erased wrapper over `string` that the compiler constrains to properties of `'T`. At runtime it is just a plain string.

```fsharp
// Construction — use the keyof helper function
let key = keyof<MyObj> _.FieldName   // keyof<MyObj>, value = "FieldName"

// Usage as a field type
type Accessor<'T> = { Key: keyof<'T> }

// Accessing the underlying string
key.Value  // "FieldName"

// Reading a property from an object
KeyOf.item key myObj   // obj option
```

### Full example: generic property getter

```fsharp
type Config = {
    Timeout: int
    Retries: int
    Verbose: bool
}

let describe (key: keyof<Config>) =
    printfn "Config key: %s" key.Value

describe (keyof<_> _.Timeout)   // "Config key: Timeout"
describe (keyof<_> _.Verbose)   // "Config key: Verbose"
```

---

## `typekeyof<'T, 'ReturnType>`

### TypeScript idiom

```typescript
// Indexed access type — retains the value type at key K
type Val = T[K]

function getTyped<T, K extends keyof T>(obj: T, key: K): T[K] { ... }
```

`keyof<'T>` loses the value type at that key (returning `obj option`). `typekeyof<'T, 'ReturnType>` is `keyof` with the value type preserved — equivalent to TypeScript's `K extends keyof T` + `T[K]` together.

At runtime it is still a plain string; the `'ReturnType` parameter is purely a compile-time witness.

### F# representation

```fsharp
// Construction
let key = typekeyof<Config, int> _.Timeout   // typekeyof<Config, int>

// Accessing with the correct return type (no casting needed)
TypeKeyOf.item key config   // int

// Erasing the return type to get a plain keyof
let plainKey: keyof<Config> = TypeKeyOf.box key
```

### Full example: type-safe property mirror

```fsharp
type Vec2 = { X: float; Y: float }

let scale (factor: float) (key: typekeyof<Vec2, float>) (v: Vec2) : Vec2 =
    let current = TypeKeyOf.item key v
    // emitted JS: v[key] * factor — no runtime overhead
    match key with
    | TypeKeyIs "X" -> { v with X = current * factor }
    | TypeKeyIs "Y" -> { v with Y = current * factor }
    | _ -> v

let doubleX = scale 2.0 (typekeyof<Vec2, float> _.X)
```

### SRTP usage for cross-type property access

`typekeyof` is designed to work with SRTP constraints, enabling generic property accessors that work across multiple types sharing the same property name:

```fsharp
// A generic accessor for any type with an 'ofValue' property
type OfValueAccessor =
    static member inline create<
        ^T, ^ReturnType when ^T:(member ofValue: ^ReturnType)
    >(?_: ^T): typekeyof<^T, ^ReturnType>
        = unbox "ofValue"

type Box = { ofValue: int }
type Crate = { ofValue: string }

// Both resolved correctly by the compiler:
OfValueAccessor.create<Box, int>()     // typekeyof<Box, int>
OfValueAccessor.create<Crate, string>() // typekeyof<Crate, string>
```

---

## `proptypekey<'T, 'ReturnType>` + `proptypelock<'T>`

### TypeScript idiom

```typescript
// T[keyof T] — a value that is *some* property of T, type unknown until unlocked
type AnyPropOf<T> = T[keyof T]

// A heterogeneous property bag where you carry the witness separately:
declare function readProp<T, K extends keyof T>(obj: T, key: K): T[K]
```

When a value could be any of the property value types of `T` — a union `T["a"] | T["b"] | T["c"]` — but the specific key isn't known until runtime, TypeScript tracks this through generic constraints. In F# the closest equivalent is `obj`, losing all type information.

`proptypelock<'T>` + `proptypekey<'T, 'ReturnType>` model this pattern. A `proptypelock<'T>` is an opaque value of *some* property of `T`. A `proptypekey<'T, 'ReturnType>` is a witness that, given a `proptypelock<'T>`, returns the concrete `'ReturnType`. The lock and key together perform the same role as TypeScript's `K extends keyof T` / `T[K]` pairing.

### Construction with `PropTypeBuilder.proptypekey`

```fsharp
// Single property — exact type
let nameKey = proptypekey (fun (p: Person) -> p.Name)   // proptypekey<Person, string>

// Multiple properties — union type (up to 8, producing U2..U8)
let nameOrAge = proptypekey (fun (p: Person) -> p.Name) (fun p -> p.Age)
// proptypekey<Person, U2<string, int>>
```

### Locking and unlocking

```fsharp
// Lock: wrap a concrete value into proptypelock
let locked: proptypelock<Person> = nameOrAge.lock "Alice"

// Unlock: recover the typed value using the witness key
let value: U2<string, int> = nameOrAge.unlock locked
// or via indexer on the lock:
let value2: U2<string, int> = locked[nameOrAge]
```

### Full example: heterogeneous property dispatch

```typescript
// TypeScript original
interface Config {
    host: string
    port: number
    debug: boolean
}
function readConfigProp<K extends keyof Config>(cfg: Config, key: K): Config[K] { ... }
```

```fsharp
type Config = { Host: string; Port: int; Debug: bool }

// Define the multi-type key witness
let configPropKey =
    proptypekey
        (fun (c: Config) -> c.Host)
        (fun c -> c.Port)
        (fun c -> c.Debug)
// : proptypekey<Config, U3<string, int, bool>>

// Lock a value we read at runtime
let locked = configPropKey.lock cfg.Host   // proptypelock<Config>

// Unlock with full type information
let v: U3<string, int, bool> = configPropKey.unlock locked
match v with
| U3.Case1 s -> printfn "host: %s" s
| U3.Case2 n -> printfn "port: %d" n
| U3.Case3 b -> printfn "debug: %b" b
```

---

## `PropertyRecord<'T, 'K>`

### TypeScript idiom

```typescript
// Index signature — an object accessed by property keys of T
interface PropertyRecord<T, K> {
    [key: keyof T]: K
}
```

A dictionary-like object where the valid keys are exactly the property names of `T` and all values share the same type `K`.

### F# representation

`PropertyRecord<'T, 'K>` is an interface with a `[<EmitIndexer>]` getter/setter. It emits plain JS bracket access (`obj[key]`) while keeping the key constrained to `keyof<'T>`.

```fsharp
// Declare a binding for a JS object that stores values by Config keys
[<ImportMember("./config-store.js")>]
let store: PropertyRecord<Config, string> = jsNative

// Type-safe read and write
let host = store[keyof<Config> _.Host]   // string
store[keyof<Config> _.Port] <- "8080"
```

---

## Active patterns: `KeyIs` / `TypeKeyIs`

Pattern match on a `keyof` or `typekeyof` value against a string literal.

```fsharp
let describeKey (key: keyof<Config>) =
    match key with
    | KeyIs "Host"  -> "the host name"
    | KeyIs "Port"  -> "the port number"
    | KeyIs "Debug" -> "the debug flag"
    | _             -> "unknown key"

let describeTypedKey (key: typekeyof<Config, int>) =
    match key with
    | TypeKeyIs "Port" -> "the port number"
    | _                -> "some int field"
```

---

## Helper modules: `KeyOf` / `TypeKeyOf`

Functional-style helpers for working with key values without using extension methods.

### `KeyOf`

| Function | Signature | Description |
|----------|-----------|-------------|
| `KeyOf.value` | `keyof<'T> -> string` | Extract the underlying string |
| `KeyOf.item` | `keyof<'T> -> 'T -> obj option` | Read a property (untyped) |
| `KeyOf.access` | alias for `item` | |
| `KeyOf.fromPropertyKey` | `typekeyof<'T,'R> -> keyof<'T>` | Erase the return type |
| `KeyOf.unsafeUnbox` | `keyof<'T> -> typekeyof<'T,'R>` | Add a return type (unchecked) |
| `KeyOf.tryUnbox` | `('T->'R) -> keyof<'T> -> typekeyof<'T,'R> option` | Checked version using `nameofLambda` |

### `TypeKeyOf`

| Function | Signature | Description |
|----------|-----------|-------------|
| `TypeKeyOf.create` | `('T -> 'R) -> typekeyof<'T,'R>` | Construct from a lambda |
| `TypeKeyOf.value` | `typekeyof<'T,'R> -> string` | Extract the underlying string |
| `TypeKeyOf.box` | `typekeyof<'T,'R> -> keyof<'T>` | Erase the return type |
| `TypeKeyOf.item` | `typekeyof<'T,'R> -> 'T -> 'R` | Read a property (typed) |
| `TypeKeyOf.access` | alias for `item` | |

---

## Summary

| TypeScript idiom | Library type |
|-----------------|-------------|
| `keyof T` (name only) | `keyof<'T>` |
| `K extends keyof T` + `T[K]` (name + value type) | `typekeyof<'T, 'ReturnType>` |
| `T[keyof T]` (heterogeneous value union) | `proptypelock<'T>` + `proptypekey<'T, 'ReturnType>` |
| `{ [key: keyof T]: V }` (index-signature object) | `PropertyRecord<'T, 'V>` |