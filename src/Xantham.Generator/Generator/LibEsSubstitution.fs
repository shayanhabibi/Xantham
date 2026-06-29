module Xantham.Generator.Generator.LibEsSubstitution

// Faithful TS-stdlib (lib.es / lib.dom) -> F#/Fable name mappings. These names have no F#
// definition in the emitted surface, so a by-name reference would dangle (FS0039).
// `substitute name` returns the Fable equivalent's prefix (any type arguments are applied
// by the caller, so `PromiseLike<T>` -> `Promise<'T>` is preserved), or None to leave the
// reference untouched. `obj` is used ONLY for genuinely-dynamic / no-equivalent types
// (`Function`, lib.dom `Attr`); every other entry is a real type.
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
    | "HTMLCollectionOf" -> Some "seq"               // lib.dom array-like collection-of-T (no workers twin)
    | "ReadonlyArray" -> Some "System.Collections.Generic.IReadOnlyList"
    | "Function" | "CallableFunction" | "NewableFunction"
    | "Attr" -> Some "obj"                           // dynamic / lib.dom DOM type with no Fable equivalent in the workers runtime
    | _ -> None
