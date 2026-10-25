---
last_commit_released: 687a15f013a1ea6531f0f51c5b015c416949c039
name: Xantham.Fable.Core
include:
  - ../../Directory.Build.props
  - ../../Directory.Build.targets
  - ../../global.json
updaters:
  - xml:
      file: Xantham.Fable.Core.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

## 0.3.0-beta.1 - 2026-10-25

### 🏗️ Breaking changes

* *(generator)* Prune tagged-union arms and separate shared tags with CaseTest (#160) ([4bb45d91](https://github.com/shayanhabibi/Xantham/commit/4bb45d915a626579a971bf09b767eb2469562063))
* *(generator)* Carry tagged-union interface arms with InlineInterface (#164) ([cd720b9c](https://github.com/shayanhabibi/Xantham/commit/cd720b9c5788238cbd5e16a61aa98ae8d6f5b34b))
* *(generator)* Preserve generic callable interface contracts (#169) ([c50ec1ea](https://github.com/shayanhabibi/Xantham/commit/c50ec1ea0aa2520a92e011242bce30df09828a7c))
* *(generator)* Close seven fidelity gaps from symbol to private members (#173) ([687a15f0](https://github.com/shayanhabibi/Xantham/commit/687a15f013a1ea6531f0f51c5b015c416949c039))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/0d9626867755b04d294c4313cd8a1ec299ccf543..687a15f013a1ea6531f0f51c5b015c416949c039)</small></strong>

## 0.2.0 - 2026-10-10

### 🚀 Features

* *(generator)* Support Myriad projections ([a48389c](https://github.com/shayanhabibi/Xantham/commit/a48389cf5e9f499b0302abbd506bcdb5d6d21534))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/3bd9ab836f8f14315a88adecadbfca619ade78fc..a48389cf5e9f499b0302abbd506bcdb5d6d21534)</small></strong>

## 0.1.1 - 2026-10-07

Baseline: verified release published to the temporary Cloudsmith feed.
Earlier history remains in Git.
