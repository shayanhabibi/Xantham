// Item 1a (wave fifteen): a package whose entry reaches content only through a
// `/// <reference lib="X" />` directive - the same shape as pointing the generator at the
// compiler's own `lib.dom.d.ts` directly (lane DA's recon). The directive resolves against the
// compiler's own bundled copy, never against this package, so every symbol it brings into scope
// classifies as `CompilerLib` (`Grouping.classify`) and none reaches `harvest-globals`'s `ours`.
// `"lib": []` in `xantham.json` suppresses the implicit default lib, so the DOM names in scope
// below come from this directive alone, not from an ambient default.
/// <reference lib="dom" />
