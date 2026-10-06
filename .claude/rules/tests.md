---
paths:
  - "tests/**"
  - "tools/**/*.fs*"
---

# Test scratch files

Tests create scratch files and directories inside the repository, under the gitignored
`tests/.scratch/`, and delete them when the test ends. The system temp directory
(`Path.GetTempPath`, `Path.GetTempFileName`) is off limits: on the Linux CI `XANTHAM_TSGO_EXE` is
unset, `Tsc.locate` finds the compiler only by walking up from the package, and generation from a
temp directory fails.

- In `tests/Xantham.Generator.Tests`, bind `Scratch.directory "<prefix>"` with `use`. It returns a
  fresh `tests/.scratch/<prefix>-<guid>` and deletes it recursively on disposal.
- Elsewhere, build the path from `__SOURCE_DIRECTORY__` to `tests/.scratch` and delete what the
  test created in a `finally`.
- A scratch MSBuild project inherits the repository's `Directory.Build.*`. Write empty
  `<Project />` copies beside it when it should build as an external consumer.
