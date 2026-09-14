# Explicit class implements contracts

Branch: `fix/class-implements-catalog`; integration base: `ad4d0e9`.

The selected CloudEdge AI consumer exposed two missing subtype relationships: Workers AI's
`WorkersAIChatLanguageModel` explicitly implements `LanguageModelV4`, and AI Search's
`AISearchChatLanguageModel` explicitly implements `LanguageModelV3`. Their generated methods
already used the catalog owner's options/results, but the generated class instance interfaces
could not upcast to those model interfaces.

The minimal failing declaration is:

```typescript
export interface Options { text: string; }
export interface Model<T> { generate(options: Options): T; }
export declare class Client implements Model<string> {
    generate(options: Options): string;
}
```

The regression separates the model's producer from the class's consumer through an authenticated
declaration catalog, then compiles the F# upcast. Before this change it failed with FS0193.

`TypeFacts.ImplementedTypes` records the explicit contracts separately from `BaseTypes`.
Resolve reads typed class heritage nodes and asks `getTypeFromTypeNode` for each contract.
`getTypeAtLocation` is unsuitable here: it adds an internal trailing `this` argument, making
`Model<string>` appear to have two arguments. No general type-argument filtering was added.
The shape and catalog dependency walks retain the implemented types; the existing interface
inheritance guard and subsequent catalog application handle their declaration ownership.

The accepted delivery boundary preserves entrypoint constructors and lifecycle hooks.
Source or target entrypoint classes decline the new interface inheritance edge with SI002.
Their `extends` bases, `Error` representation, and optional hook decisions are unchanged.
Generic constraint inference also retains its existing declared-base/intersection proof;
raw implements metadata does not prove an edge that the final class representation declines.

`class-implements-lab` tests fixed and constrained generic interfaces, rejects a structural
lookalike upcast, and rejects an invalid generic argument. `class-implements-entrypoint-lab`
compiles subclass constructors, an optional `IFetchHandler`, and an `Error` upcast; it also
declines an ordinary class implementing an abstract class-shaped target.

Validation:

- The targeted six tests pass in both generation and comparison phases.
- Corpus update: 890 generator tests and 90 wire tests pass in both phases.
- Only Workers Types changed among existing goldens: two queue strategy inheritance edges
  and four newly reachable branded contract interfaces, adding 26 generated lines.
- Workers Types tiers: exact 412 unchanged; ergonomic 1068 → 1072; widened 369 unchanged;
  escape 111 unchanged. SI005 55 → 57; SI002 0 → 4; SP001 1301 → 1305; RA005 4 → 8;
  TR008 167 → 168; TR024 118 → 119. The four entrypoint contracts are the visible declined
  edges; `QueuingStrategy<any>` accounts for the newly traversed `any` argument.
- All other existing fixture finding counts are unchanged. The ordinary new lab has zero
  widened/escape symbols. No new finding codes were introduced.
- The actual six-library AI probe regenerated both provider owners, OpenAI-compatible support,
  AI Gateway, Workers AI and AI Search. The unchanged typed consumer compiles with those six
  assemblies on .NET 8 in Release, with zero warnings/errors. Fresh FCS checks of that consumer
  and the generator are clean, also with zero warnings/errors. Runtime inference was not run.
- Mandatory unfiltered `dotnet fsi build.fsx -- test`: 890 generator and 90 wire tests pass,
  zero failures/errors. The Fable run-gate command then passes all 461 runtime checks; its
  existing type-test and Node module warnings remain. Fixtures were installed fresh in this
  worktree and the compiler was the explicitly required cached TypeScript 7.1 pin.

Logs during this run: `/tmp/xantham-implements-lab-before.log`,
`/tmp/xantham-implements-lab-type-node.log`, `/tmp/xantham-implements-corpus-update.log`,
`/tmp/xantham-implements-full-gate.log`, `/tmp/xantham-implements-run-gate.log`, and
`/tmp/xantham-implements-actual-ai.log`.
The actual AI probe is retained under CloudEdge
`artifacts/one-shot-20260913/ai-shared-delivery/composition-implements-probe/` with its frozen
tool payload, exact generator configs, source fingerprints and compilation result.

Final change: 26 files, including two small tracked labs and their goldens. The existing
Workers Types binding adds 26 lines; other existing binding files are unchanged.
