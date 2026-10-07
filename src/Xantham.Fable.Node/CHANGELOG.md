---
name: Xantham.Fable.Node
last_commit_released: 3bd9ab836f8f14315a88adecadbfca619ade78fc
include:
  - ../Xantham.Fable.Core/**
  - ../Xantham.Fable.Core.TS/**
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Fable.Node.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.1.1 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
