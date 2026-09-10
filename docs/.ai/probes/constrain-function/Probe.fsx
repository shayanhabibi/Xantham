// Throwaway feasibility probe, not generated binding code.
// Build Xantham.Fable.Core.TS in Debug first. Select one case with --define.
#r "nuget: Fable.Core, 5.2.0"
#r "../../../../src/Xantham.Fable.Core.TS/bin/Debug/net8.0/Xantham.Fable.Core.TS.dll"

open Fable.Core

#if INTERFACE
type Holder<'T when ConstrainFunction<'T>> =
    abstract callback: 'T
#endif

#if METHOD
type Holder =
    abstract keep<'T when ConstrainFunction<'T>> : 'T -> 'T
#endif

#if STATIC_INLINE
type Holder =
    static member inline keep<'T when ConstrainFunction<'T>> (fn: 'T) = fn
#endif

#if ANNOTATION
let inline keep (fn: ConstrainFunction< ^T>) = fn
let result = keep (fun (x: int) -> x + 1) 1
assert (result = 2)
#endif

#if JS_FUNCTION
let inline keep (fn: ConstrainFunction< ^T>) = fn
let result = keep (Unchecked.defaultof<JS.Function>)
#endif

#if TS_FUNCTION
let inline keep (fn: ConstrainFunction< ^T>) = fn
let result = keep (Unchecked.defaultof<TS.Es.Function>)
#endif

#if DELEGATE
let inline keep (fn: ConstrainFunction< ^T>) = fn
let result = keep (System.Func<int, int>(fun x -> x))
#endif

#if NONFUNCTION
let inline keep (fn: ConstrainFunction< ^T>) = fn
let result = keep 42
#endif
