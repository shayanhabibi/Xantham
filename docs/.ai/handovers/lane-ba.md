# Lane BA — a named F# delegate behaves as `System.Func`/`System.Action` does

Wave twelve, branch `worktree-gen-wave12-ba`, forked at `6141eb2`. No generator change.

`type TickHandler = delegate of x: float * y: float -> string` is the spelling that works, measured in every position `callbackGoldenForms` covers — parameter plain, `unit`-returning and as a named abbreviation, `ParamObject` literal and method-shaped `ParamObject` parameter, interface member, property, method return, both nestings, a factory crossing outward — at arities 2, 3 and 4, in both directions. **Every position matches `Func`/`Action` exactly; none differs.** Arity 4 runs against runtime-only `callFour`/`makeFour` added to `callback-function-lab/index.js` (`index.d.ts` untouched, so no golden and no tier moves), with a `Func` control beside it.

The names are an F#-side affordance only: `handler.Invoke(x = 1.0, y = 2.0)` compiles, and the
emitted JavaScript carries the binders of whichever lambda built the function, never `x`/`y`.

Run gate 257 → 283 checks, `test` green, tiers `exact 495, ergonomic 1552, widened 782, escape 193` and `RT001` 3 all unchanged. Measurement commit `642830f`, this handover the one after. Nothing unexplained.
