# Xantham.Generator.Myriad

Opt-in Myriad contracts and typed operations from Xantham's resolved TypeScript declarations.
`LiteralUnions.create identity workspace selections` returns a `ProjectionExtension`
for `Pipeline.generateProjectedWith` or `Pipeline.runProjectedWith`. Each
`LiteralUnionSelection` identifies a package and declaration path, plus a companion
module and type name. The workspace is a caller-owned directory for temporary Myriad
input files; the adapter removes each input directory after generation.

The adapter invokes the published Myriad `IMyriadGenerator` contract before Shape.
It accepts finite string literal sets with optional `number`, `null` and `undefined`
arms. Xantham owns source resolution, supported-arm validation and declaration
provenance; Myriad emits the selected companion. Unsupported declarations produce
diagnostics. Raw bindings and declaration catalogs retain their existing owners.

Each companion contains an ordinary discriminated union, `encode : Value -> obj`,
`decode : obj -> Result<Value, string>`, and the `Decoded|Invalid` active pattern.
Replace `Value` with the selected type name. The codecs target Fable 5 and preserve
JavaScript `null` and `undefined` as separate cases. Matching uses ordinary nested
DU patterns, for example `Decoded (Value.Number number)` or `Decoded Value.Null`.

Decoding validates membership. Equal or overlapping string sets can accept the
same JavaScript primitive; decoding does not recover its declaration of origin.
An application can keep an explicit outer DU when branch provenance matters.

Case names use Xantham's literal naming and collision allocation. Typed arms keep
`Number`, `Null` and `Undefined`; colliding string cases receive numeric suffixes.
Generated DU member names also reserve `Tags`, Fable's `ToString` intrinsic and each
case's `Is<Case>` member.
Module and type names must be unquoted ASCII identifiers accepted by Xantham's naming
helpers; type names must start with an uppercase letter. Output filenames
are `<ModuleName>.fs`; each module exposes the selected type and codec functions.
The adapter records selections under the reserved extension configuration key
`xantham.myriad.literal-unions` so output provenance includes its projection choices.

`LiteralUnionGenerator` is also a Myriad plugin named `xantham-literal-unions`.
Its JSON input has `formatVersion: 1`, `moduleName`, `typeName`, `stringLiterals`,
and `otherArms` (each entry is `number`, `null` or `undefined`). The Xantham adapter
creates this input from its resolved snapshot and seals the resulting companion
to the selected declaration.

`Operations.create identity workspace selections` generates typed instance-method calls.
Each `OperationSelection` identifies the package, receiver declaration path, method and
parameter, plus the emitted receiver type, module, input type and function names. Set
`FieldName = None` for the complete argument, or select one optional field of an object
whose other fields are optional. The generated function accepts the input contract and
performs encoding internally. Other arguments and the result retain the actual SDK types.
Xantham verifies the receiver against its final emitted owner before compiling the output.

Operation contracts support strings, numbers, booleans, string literals, null, undefined,
arrays, data records and unions of those shapes. Required literal fields become automatic
discriminators; payload records contain the remaining typed fields. Optional record fields
use an outer option: `None` omits the property, while `Some Undefined` includes an own
property containing undefined. Selected-field operations use the same outer option.
Null remains a separate DU case. Encoders are private; calling an operation requires no
boxing, casts or manually constructed JavaScript objects.

Use `Operations.createShared` to give several operations the first selection's input
contract. Subsequent modules expose F# aliases to that owner. Every selection must have
the same resolved shape and type name; a mismatch rejects generation. Each operation
retains its own source and artifact provenance. This allows one Pi input value to pass
to both submit and prompt without weakening the type.

Recursive, generic, indexed, callable and opaque payloads, computed/symbol keys and
unsupported source-reference forms produce diagnostics. Overloaded or generic methods
also reject. Operation generation preserves the resolved value contract; it does not
claim support for TypeScript's `exactOptionalPropertyTypes` write rules. The plugin is
`xantham-operations`; Xantham supplies its authenticated JSON shape and signature input.
