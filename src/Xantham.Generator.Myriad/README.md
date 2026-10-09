# Xantham.Generator.Myriad

Opt-in Myriad companions for Xantham's resolved TypeScript union declarations.
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
