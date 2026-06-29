module Xantham.Generator.Generator.LibEsSubstitution

// Faithful TS-stdlib (lib.es) -> F#/Fable name mappings. These names have no F#
// definition in the emitted surface, so a by-name reference would dangle (FS0039).
// `substitute name` returns the Fable equivalent's prefix (any type arguments are applied
// by the caller, so `PromiseLike<T>` -> `Promise<'T>` is preserved), or None to leave the
// reference untouched. `obj` is used ONLY for genuinely-dynamic types (`Function`); every
// other entry is a real type.
//
// NB: lib.dom types are no longer in scope (the encoder restricts `lib` to esnext — see
// docs/fixing-methodology.md), so prior lib.dom-only entries (HTMLCollectionOf, Attr) were
// removed as dead. Keep this map to lib.es types that genuinely still appear.
//
// This is the single source of truth consumed by the ResolvedTypePrelude interceptor in
// Render.fs and exercised directly by the generator unit tests.
let substitute (name: string) : string option =
    match name with
    | "Array" -> Some "ResizeArray"                  // JS array -> mutable F# ResizeArray (prefix-swap keeps the element arg)
    | "Error" -> Some "exn"
    | "PromiseLike" -> Some "Promise"                // Fable.Core.JS.Promise (open Fable.Core.JS)
    | "IterableIterator" | "Iterator"
    | "ArrayIterator" | "AsyncIterableIterator"
    | "Iterable" -> Some "seq"                       // Fable maps seq<'T> to a JS iterable
    | "ReadonlyArray" -> Some "System.Collections.Generic.IReadOnlyList"
    | "Function" | "CallableFunction" | "NewableFunction" -> Some "obj"  // dynamic, no Fable equivalent
    | _ -> None
