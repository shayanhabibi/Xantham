---
paths:
    - src/Xantham.Decoder/**
---

**`ArenaInterner`** (`Types/Arena.Interner.fs`): Resolves flat `TypeKey`-keyed maps into a lazy object graph — following a reference = forcing a `Lazy<ResolvedType>`. Cycles broken by lazy boundaries (construction never forces outgoing lazies). Export map shelled eagerly; nested types deferred. `(|Resolve|)` active pattern for ergonomic lazy forcing.
