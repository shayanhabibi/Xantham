---
last_commit_released: a48389cf5e9f499b0302abbd506bcdb5d6d21534
name: xantham
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

## 0.3.0 - 2026-10-10

### 🚀 Features

* *(generator)* Portable declaration catalogue compatibility (#111) ([ccd7d79](https://github.com/shayanhabibi/Xantham/commit/ccd7d7937ec37fe541b00fddf448794da6695fe8))
* *(generator)* Brotli catalogue transport (#112) ([2a1aafe](https://github.com/shayanhabibi/Xantham/commit/2a1aafe65e649b63de62dc021f166869d3fd09c5))
* *(generator)* Support Myriad projections ([a48389c](https://github.com/shayanhabibi/Xantham/commit/a48389cf5e9f499b0302abbd506bcdb5d6d21534))

### 🐞 Bug Fixes

* *(generator)* Preserve nullable alias catalogue identities (#109) ([1d2bd62](https://github.com/shayanhabibi/Xantham/commit/1d2bd62c548f64650075ea308c6845335f5d1b4c))
* *(generator)* Preserve catalog constraints and source ownership (#113) ([45be239](https://github.com/shayanhabibi/Xantham/commit/45be2390733cd09856372480adffccf16725c3c8))
* *(generator)* Make compiler library catalogue reuse portable and stable (#114) ([c778cb9](https://github.com/shayanhabibi/Xantham/commit/c778cb9cf9f9d4867258628d794397b6fb416342))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/3bd9ab836f8f14315a88adecadbfca619ade78fc..a48389cf5e9f499b0302abbd506bcdb5d6d21534)</small></strong>

## 0.2.0 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
