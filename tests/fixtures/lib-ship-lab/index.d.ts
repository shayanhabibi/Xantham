// Item 1f (wave fifteen): the compiler-lib group carries a `Ship` disposition end to end. This
// package declares nothing itself - no import, no export, no declaration - so every name this
// run produces comes from `lib.scripthost.d.ts`, loaded because `xantham.json`'s `"lib"` names
// only it. A global script (no module keyword) reaches `harvest-globals`, not `harvest-exports`;
// `harvest-globals` used to keep only symbols classified `EntryPackage` (`Grouping.classify`),
// so a name whose first declaration is the compiler's own lib file never reached it - until this
// package's own `"typescript/lib": "ship"` widens the admitted set to match.
