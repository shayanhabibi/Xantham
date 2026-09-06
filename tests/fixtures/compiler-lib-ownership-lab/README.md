# Compiler-library ownership

The default fixture adds application declarations to ES5. Its aliases and `WindowLike.self`
intersection belong to the entry module; the compiler's `Date` and other shared declarations
remain in the core group. The golden compile gate checks their references together.

The live tests also select `clean.d.ts`, whose empty export contributes no declarations, and
`referenced-augmentation.d.ts`, whose otherwise empty root loads a `DateConstructor` augmentation.
Only the clean program earns compiler-core ownership of its synthetic `GlobalThis`.

`dom-enriched.json` records a further, unsupported configuration:

```sh
xantham generate tests/fixtures/compiler-lib-ownership-lab \
  --config tests/fixtures/compiler-lib-ownership-lab/dom-enriched.json \
  -o /tmp/compiler-lib-ownership-dom
```

The DOM library's `Window.self` is `Window & typeof globalThis`. With application globals in
scope, its generated core declaration can therefore depend on the entry's global object.
Splitting that enriched scope into reusable bindings needs further dependency and augmentation
handling. The clean-scope certificate does not certify this configuration, and this profile
is excluded from the lab's default golden configuration.
