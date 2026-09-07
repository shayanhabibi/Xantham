# Configuration types to JSON Schema

A runnable proposal for keeping JSON field names, shapes, and descriptions on a
configuration input type. Run it from the Xantham repository root:

```sh
git clone --branch descriptions https://github.com/shayanhabibi/FSharp.Data.JsonSchema.git ../Xantham.SchemaExample.Dependency
git -C ../Xantham.SchemaExample.Dependency checkout --detach 1a4035c1f2de8c4fb97726fb1ed5dbe58c8bf6cb
dotnet run --project tools/schema-contract-example -p:JsonSchemaSource="$(realpath ../Xantham.SchemaExample.Dependency)" -- --out /tmp/xantham-config-example.schema.json
```

An existing checkout can be supplied through `JsonSchemaSource`. The commit above
is the tested baseline; the description work is not yet a published package.
The dependency stays outside Xantham. This example is outside the main solution
and does not replace `xantham schema`, change `xantham.json`, or alter its loader.

## What to review

`Model.fs` defines four JSON input fields: `module`, `types`, `resolveNoInfer`, and
`groups`. Their descriptions live on the fields as `DescriptionAttribute` values.
There is no second table of field names or descriptions. The JSON input model
represents omission before defaults are applied; the generator's internal model
can continue to store a concrete Boolean and an empty map after loading.
The mapped arity's range is likewise declared once with `RangeAttribute`.

The model also declares the alternatives the current JSON format accepts:

- A group action is `"ship"`, `"reference"`, or `"widen"`, or a mapping object.
- A mapped name is a string or an object with `name` and optional `arity`.
- An omitted `types` field differs from an explicitly empty array.

`SchemaContract.fs` calls the library's generator and applies a serialization
policy through typed schema objects. Field names and union alternatives come from
the types. It shares the library's naming policy (lowercase the first letter),
and uses omittable non-null option fields and unwrapped single-payload union cases.
These are explicit conventions of this example;
other encodings require their own declared policy. No JSON Schema text is
assembled by the adapter.

The adapter also demonstrates three gaps in the pinned library's output path:
carrying the analyzer's descriptions through the NJsonSchema translator,
preserving a map definition's value schema, and distinguishing absent record
options from nullable values. Those are candidates for library improvements;
the example keeps their treatment visible in one file.

`Checks.fs` validates instances against the saved-and-reloaded schema and invokes
Xantham's existing loader, checking the loaded values as well as acceptance. A
separate described record proves that a new field needs no schema-key-table edit.
Known differences from the permissive loader are checked and reported separately.
For example, the loader accepts a negative arity, while this input contract
declares a nonnegative count bounded by `Int32.MaxValue`.
This four-field example is not a complete replacement schema or a proposal to
change which configurations the production CLI accepts.

For adoption, make the input contract the authority, settle the existing
loader/schema disagreements, and promote the reusable policy hooks into the
library. Tests should check accepted JSON and descriptions, rather than requiring
a particular arrangement of `$ref` definitions.
Exposing translation of a `SchemaDocument` would let the policy operate on the
Core representation before output generation, without repeating type analysis.
