---
name: Xantham.Generator.Myriad
last_commit_released: 45be2390733cd09856372480adffccf16725c3c8
include:
  - ../Xantham.TypeScript.Wire/**
  - ../Xantham.Generator/**
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Generator.Myriad.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.1.0 - 2026-10-09

Initial package: opt-in Myriad union companions from resolved TypeScript facts.
