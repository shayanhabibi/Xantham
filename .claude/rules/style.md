---
paths: "**/*.fs"
---

# Style

- Follow fantomas 8.0.0-beta-001 defaults
- Exceptions:
  - Generated *.fs files
    - This only applies if the cost on performance for creating styled *.fs files does not warrant its application
    - In this case, ensure the generated file is ignored in `.fantomasignore`
  - Expecto test files