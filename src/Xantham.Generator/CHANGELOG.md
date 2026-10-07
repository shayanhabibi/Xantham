---
name: Xantham.Generator
last_commit_released: 3bd9ab836f8f14315a88adecadbfca619ade78fc
include:
  - ../Xantham.TypeScript.Wire/**
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Generator.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.1.0 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
