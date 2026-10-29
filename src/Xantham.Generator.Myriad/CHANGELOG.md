---
last_commit_released: 4fc08942ab1374f77bee22bdad079993aebaf5c1
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

## 0.3.0-beta.3 - 2026-10-29

### 🏗️ Breaking changes

* *(generator)* Recover generic methods from merged declarations (#175) ([569a8a05](https://github.com/shayanhabibi/Xantham/commit/569a8a059a0c8b3fcfbea56e49461544ca96dc89))

### 🐞 Bug Fixes

* *(generator)* Recover ambient values and precise overload returns (#176) ([d0200f85](https://github.com/shayanhabibi/Xantham/commit/d0200f8510dd76c8576481ac94964c0d6549f916))
* *(generator)* Preserve exact keys and source ownership contracts (#177) ([e7ba6ab2](https://github.com/shayanhabibi/Xantham/commit/e7ba6ab213ae4d17f31e44735afb837d3a052758))
* *(generator)* Specialize dependent constraints and key catalogue identities by bound (#179) ([87e82faa](https://github.com/shayanhabibi/Xantham/commit/87e82faa9db8eb1492d36d6ac439eb107046e9ce))
* *(generator)* Validate projections against supplied support assemblies ([259b9786](https://github.com/shayanhabibi/Xantham/commit/259b978625e6718de3d5c17fa01f7dffcaf65b35))
* *(generator)* Preserve source union alias equivalence ([5f331cb7](https://github.com/shayanhabibi/Xantham/commit/5f331cb71977b4aa6ebfc0aba79f4e7f364e0898))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/687a15f013a1ea6531f0f51c5b015c416949c039..4fc08942ab1374f77bee22bdad079993aebaf5c1)</small></strong>

## 0.3.0-beta.2 - 2026-10-25

### 🏗️ Breaking changes

* *(generator)* Prune tagged-union arms and separate shared tags with CaseTest (#160) ([4bb45d91](https://github.com/shayanhabibi/Xantham/commit/4bb45d915a626579a971bf09b767eb2469562063))
* *(generator)* Preserve fold arms and bind callable exports with their members (#161) ([02532285](https://github.com/shayanhabibi/Xantham/commit/02532285a7a94765bf990c2c3d7207d7251256ff))
* *(generator)* Carry tagged-union interface arms with InlineInterface (#164) ([cd720b9c](https://github.com/shayanhabibi/Xantham/commit/cd720b9c5788238cbd5e16a61aa98ae8d6f5b34b))
* *(generator)* Preserve callable exclusive arms as erased unions (#166) ([d7cfebe8](https://github.com/shayanhabibi/Xantham/commit/d7cfebe8b71e1e9a2322db3df2c997020ac7d2f2))
* *(generator)* Retain named generic callable applications (#167) ([f9660c86](https://github.com/shayanhabibi/Xantham/commit/f9660c86b4e8e03fa03046119f229ee92dab0aa1))
* *(generator)* Preserve declaration-owned alias type parameters (#168) ([9e278bf2](https://github.com/shayanhabibi/Xantham/commit/9e278bf2c76d5012971f536ab2654a7d0e7816be))
* *(generator)* Preserve generic callable interface contracts (#169) ([c50ec1ea](https://github.com/shayanhabibi/Xantham/commit/c50ec1ea0aa2520a92e011242bce30df09828a7c))
* *(generator)* Close seven fidelity gaps from symbol to private members (#173) ([687a15f0](https://github.com/shayanhabibi/Xantham/commit/687a15f013a1ea6531f0f51c5b015c416949c039))

### 🚀 Features

* *(wire)* Migrate to TypeScript 7.1.0-beta and Fickl (#162) ([40a70f03](https://github.com/shayanhabibi/Xantham/commit/40a70f03aee7712a4caf3c388d1600ff11a84b59))

### 🐞 Bug Fixes

* *(generator)* Preserve namespace ownership and recursive callable fidelity (#170) ([8c709082](https://github.com/shayanhabibi/Xantham/commit/8c709082dac4d5289fb6718ae1c488db9962bcb0))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/977355536eba47bff4672d609cccca7523bda9f7..687a15f013a1ea6531f0f51c5b015c416949c039)</small></strong>

## 0.3.0-beta.1 - 2026-10-14

### 🚀 Features

* *(core-ts)* Write Promise as JS.Promise (W3, #134) ([98e3456f](https://github.com/shayanhabibi/Xantham/commit/98e3456f04f7cb35cf602f79655ae719133181d4))
* *(findings)* Grade a faithful any as ergonomic (W1, #132) ([7d99283d](https://github.com/shayanhabibi/Xantham/commit/7d99283ddbd94abd2031cfe29da037df0de05178))
* *(generator)* Write selected callback any returns as unit (W2, #133) ([d3a2a122](https://github.com/shayanhabibi/Xantham/commit/d3a2a122c4782904107e270572f8498b008351b7))
* *(generator)* Record anyReturnAsUnit in the catalog inference profile (W2, #133) ([6350fe4e](https://github.com/shayanhabibi/Xantham/commit/6350fe4e59e760b1d57f1b72c14695f01e7e3f0b))
* *(generator)* Emit well-known symbol members (W4) ([1c8c9daa](https://github.com/shayanhabibi/Xantham/commit/1c8c9daa340a7ee4aec44f2b75b1ae3090bf0c1e))
* *(generator)* Reference Core.TS ECMAScript declarations under widen ([063a023d](https://github.com/shayanhabibi/Xantham/commit/063a023d64f012be9e0c9e0e1d0511c4827169f6))
* *(generator)* Reference Core.TS for standard-library names (W3, #134) ([565e689c](https://github.com/shayanhabibi/Xantham/commit/565e689cb6283d0cc420839f75f89063fea6399e))
* *(generator)* Write literal overloads as Emit-backed methods (W5, #136) ([7b3b20a9](https://github.com/shayanhabibi/Xantham/commit/7b3b20a9e167e8edd6ad95492ccb37878a4c6bf2))
* *(generator)* Literal members as [<Literal>] modules and Create Emits (W6, #137) ([6a4a9cf9](https://github.com/shayanhabibi/Xantham/commit/6a4a9cf9ab679ed375a10278d078d0290f247e28))
* *(generator)* Make writable required single-literal members literal members (W6, #137) ([65ba7da1](https://github.com/shayanhabibi/Xantham/commit/65ba7da19f2996943a712f454175144fc2432848))
* *(generator)* Keep type parameters in scope (W7, #138) ([f15fb73c](https://github.com/shayanhabibi/Xantham/commit/f15fb73c7b6c9181bb373a10f6aafaf6f3f809a5))
* *(generator)* Emit key types for tag maps and value-of aliases (W8, #139) ([dab32780](https://github.com/shayanhabibi/Xantham/commit/dab327807cff7627583f3718410fbba50e7fb1fd))
* *(generator)* Name a tag map's result type parameter 'E (W8, #139) ([38f0eb2a](https://github.com/shayanhabibi/Xantham/commit/38f0eb2adcfbdade0350af0cf60f0aedbf9c4751))
* *(generator)* Resolve conditionals at a bounded check type behind resolveConditionalConstraints (W9, #140) ([d75a0807](https://github.com/shayanhabibi/Xantham/commit/d75a080782de8f2880767adb8bfc235d1194ac2f))
* *(generator)* Refuse a union constraint under resolveConditionalConstraints (W9, #140) ([90761796](https://github.com/shayanhabibi/Xantham/commit/90761796ddbe55bd9ee63b08fcc4c5ac57f0fdef))

### 🐞 Bug Fixes

* *(generator)* Settle W4 symbol members review findings ([dd128439](https://github.com/shayanhabibi/Xantham/commit/dd1284392519687b14a63f811bf8849f2762d790))
* *(generator)* Keep a literal-keyed method's doc to its own overload (W5, #136) ([f503e748](https://github.com/shayanhabibi/Xantham/commit/f503e7484b92cdb381ef86a55786698ea4f6f749))
* *(generator)* Classify namespace functions as methods for customization (W7, #138) ([6e218656](https://github.com/shayanhabibi/Xantham/commit/6e2186566e76ab94acac2ca80a57ff585cb50efa))
* *(generator)* Emit PropertyRecord, bound catalogued maps, scope overload tags (W8, #139) ([9e1882c6](https://github.com/shayanhabibi/Xantham/commit/9e1882c69f1f2b9932081cc09f930f2877f8117d))
* *(generator)* Record a conditional's bound only for an infer branch (W9, #140) ([62164ad4](https://github.com/shayanhabibi/Xantham/commit/62164ad41fcf973dcee1a0069a343a2685e53693))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/2f70620ce9950438f4043662a5f964151d9614fb..977355536eba47bff4672d609cccca7523bda9f7)</small></strong>

## 0.2.1 - 2026-10-12

### 🐞 Bug Fixes

* *(generator)* Preserve source contracts and shard CI validation (#123) ([2f70620](https://github.com/shayanhabibi/Xantham/commit/2f70620ce9950438f4043662a5f964151d9614fb))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/0d9626867755b04d294c4313cd8a1ec299ccf543..2f70620ce9950438f4043662a5f964151d9614fb)</small></strong>

## 0.2.0 - 2026-10-10

### 🚀 Features

* *(generator)* Support Myriad projections ([a48389c](https://github.com/shayanhabibi/Xantham/commit/a48389cf5e9f499b0302abbd506bcdb5d6d21534))

### 🐞 Bug Fixes

* *(generator)* Make compiler library catalogue reuse portable and stable (#114) ([c778cb9](https://github.com/shayanhabibi/Xantham/commit/c778cb9cf9f9d4867258628d794397b6fb416342))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/45be2390733cd09856372480adffccf16725c3c8..a48389cf5e9f499b0302abbd506bcdb5d6d21534)</small></strong>

## 0.1.0 - 2026-10-09

Initial package: opt-in Myriad union companions from resolved TypeScript facts.
