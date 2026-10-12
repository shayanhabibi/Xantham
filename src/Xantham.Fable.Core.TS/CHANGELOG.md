---
last_commit_released: 2f70620ce9950438f4043662a5f964151d9614fb
name: Xantham.Fable.Core.TS
include:
  - ../../tools/fable-core-ts-input/**
  - ../../tools/package-core-ts-catalog.fsx
  - ../Xantham.Fable.Core/**
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Fable.Core.TS.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.2.2 - 2026-10-12

### 🐞 Bug Fixes

* *(generator)* Preserve source contracts and shard CI validation (#123) ([2f70620](https://github.com/shayanhabibi/Xantham/commit/2f70620ce9950438f4043662a5f964151d9614fb))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/0d9626867755b04d294c4313cd8a1ec299ccf543..2f70620ce9950438f4043662a5f964151d9614fb)</small></strong>

## 0.2.1 - 2026-10-11

### 🐞 Bug Fixes

* *(core-ts)* Ship authenticated Brotli declaration catalogue (#120) ([ad60b48](https://github.com/shayanhabibi/Xantham/commit/ad60b4824298c580d19b2aafc7048bfc80a2eded))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/78de2edf0c5370b8251f12ac47b81273d5639312..f8ccbd9fcc59ac4a36adaafa3b31c24ece8aaec5)</small></strong>

## 0.2.0 - 2026-10-10

### 🚀 Features

* *(generator)* Support Myriad projections ([a48389c](https://github.com/shayanhabibi/Xantham/commit/a48389cf5e9f499b0302abbd506bcdb5d6d21534))

### 🐞 Bug Fixes

* *(generator)* Preserve nullable alias catalogue identities (#109) ([1d2bd62](https://github.com/shayanhabibi/Xantham/commit/1d2bd62c548f64650075ea308c6845335f5d1b4c))
* *(generator)* Preserve catalog constraints and source ownership (#113) ([45be239](https://github.com/shayanhabibi/Xantham/commit/45be2390733cd09856372480adffccf16725c3c8))
* *(generator)* Make compiler library catalogue reuse portable and stable (#114) ([c778cb9](https://github.com/shayanhabibi/Xantham/commit/c778cb9cf9f9d4867258628d794397b6fb416342))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/3bd9ab836f8f14315a88adecadbfca619ade78fc..a48389cf5e9f499b0302abbd506bcdb5d6d21534)</small></strong>

## 0.1.1 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
