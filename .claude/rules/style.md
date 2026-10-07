---
paths: "**/*.fs"
---

# Style

- Follow fantomas 8.0.0-beta-001 defaults
- Exceptions:
  - Generated *.fs files
    - This only applies if the cost on performance for creating styled *.fs files does not warrant its application
    - In this case, ensure the generated file is ignored in `.fantomasignore`
    - `.fantomasignore` lists the `*.generated.fs` layers, the generated bindings
      (`src/Xantham.Fable.Core.TS/Fable.Core.TS.fs`, `src/Xantham.Fable.Node/Node.fs`) and the
      golden corpus. On CI the `format` stage runs `fantomas --check`, so a generated file that
      is not listed fails the build
  - Expecto test files