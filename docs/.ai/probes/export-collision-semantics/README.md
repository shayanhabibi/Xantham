# Export collision compiler checkpoint

Run `rtk dotnet build src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj`, then
`rtk python docs/.ai/probes/export-collision-semantics/probe.py`.
An optional substring selects cases. The runner references the built consumer support
assemblies and Fable.Core 5.2.0; it checks exact compiler diagnostic codes, not just failure.
`results.json` records the complete 22-case run on 2026-09-11.

Observed F# semantics:

- String/float parameters and different explicit generic arities remain selectable.
- Return-only, constraints-only, alpha-renamed generics, recursively applied aliases,
  optional versus explicit option, and rest versus explicit array produce FS0438.
- Both float and string units of measure erase for declaration collision checks. Keep
  measures in the full semantic key used for duplicate proof; erase them only in the CLR key.
- Distinct named delegate declarations remain nominally distinct. Delegate abbreviations
  expand to their underlying System.Func type.
- A single tuple parameter differs from two parameters.
- A nullary method differs from an explicit named unit parameter; both compile and
  are selectable with `f()` and `f(x = ())` respectively.
- Optional-only and rest-only overloads with different element types compile as declarations
  but their omitted/empty calls produce FS0041. Named optional calls can select each overload,
  which does not repair their shared ambiguous omitted call.
- A property and method sharing a name produce FS0434. Explicit methods matching getter
  and setter compiled names produce FS0438.

Canonicalization must therefore have separate compiled and full semantic forms. It must
substitute alias arguments before expanding nested forms, alpha-normalize bound variables,
preserve generic arity and nominal delegates, and terminate alias cycles symbolically.
Duplicate proof additionally requires all occurrence provenance and unchanged binding,
result, optional/rest/named-call contract, constraints, and mutability.

The first harness run had an invalid support-library namespace open; it was corrected before
collecting these results. No failed setup run is counted as compiler collision evidence.
