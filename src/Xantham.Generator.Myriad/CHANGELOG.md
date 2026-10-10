---
last_commit_released: a48389cf5e9f499b0302abbd506bcdb5d6d21534
name: Xantham.Generator.Myriad
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

## 0.2.0 - 2026-10-10

### 🚀 Features

* *(generator)* Support Myriad projections ([a48389c](https://github.com/shayanhabibi/Xantham/commit/a48389cf5e9f499b0302abbd506bcdb5d6d21534))

### 🐞 Bug Fixes

* *(generator)* Make compiler library catalogue reuse portable and stable (#114) ([c778cb9](https://github.com/shayanhabibi/Xantham/commit/c778cb9cf9f9d4867258628d794397b6fb416342))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/45be2390733cd09856372480adffccf16725c3c8..a48389cf5e9f499b0302abbd506bcdb5d6d21534)</small></strong>

## 0.1.0 - 2026-10-09

Initial package: opt-in Myriad union companions from resolved TypeScript facts.
