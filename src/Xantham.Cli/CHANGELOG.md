---
name: xantham
last_commit_released: 3bd9ab836f8f14315a88adecadbfca619ade78fc
include:
  - ../Xantham.TypeScript.Wire/**
  - ../Xantham.Generator/**
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Cli.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.2.0 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
