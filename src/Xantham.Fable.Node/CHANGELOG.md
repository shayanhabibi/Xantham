---
last_commit_released: 4fc08942ab1374f77bee22bdad079993aebaf5c1
name: Xantham.Fable.Node
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

## 0.4.0-beta.3 - 2026-10-29

### 🏗️ Breaking changes

* *(generator)* Recover generic methods from merged declarations (#175) ([569a8a05](https://github.com/shayanhabibi/Xantham/commit/569a8a059a0c8b3fcfbea56e49461544ca96dc89))

### 🐞 Bug Fixes

* *(generator)* Recover ambient values and precise overload returns (#176) ([d0200f85](https://github.com/shayanhabibi/Xantham/commit/d0200f8510dd76c8576481ac94964c0d6549f916))
* *(generator)* Preserve exact keys and source ownership contracts (#177) ([e7ba6ab2](https://github.com/shayanhabibi/Xantham/commit/e7ba6ab213ae4d17f31e44735afb837d3a052758))
* *(generator)* Specialize dependent constraints and key catalogue identities by bound (#179) ([87e82faa](https://github.com/shayanhabibi/Xantham/commit/87e82faa9db8eb1492d36d6ac439eb107046e9ce))
* *(generator)* Validate projections against supplied support assemblies ([259b9786](https://github.com/shayanhabibi/Xantham/commit/259b978625e6718de3d5c17fa01f7dffcaf65b35))
* *(tests)* Align runtime probes with pinned Fickl compiler ([4fc08942](https://github.com/shayanhabibi/Xantham/commit/4fc08942ab1374f77bee22bdad079993aebaf5c1))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/687a15f013a1ea6531f0f51c5b015c416949c039..4fc08942ab1374f77bee22bdad079993aebaf5c1)</small></strong>

## 0.4.0-beta.2 - 2026-10-25

### 🏗️ Breaking changes

* *(generator)* Prune tagged-union arms and separate shared tags with CaseTest (#160) ([4bb45d91](https://github.com/shayanhabibi/Xantham/commit/4bb45d915a626579a971bf09b767eb2469562063))
* *(generator)* Preserve fold arms and bind callable exports with their members (#161) ([02532285](https://github.com/shayanhabibi/Xantham/commit/02532285a7a94765bf990c2c3d7207d7251256ff))
* *(generator)* Carry tagged-union interface arms with InlineInterface (#164) ([cd720b9c](https://github.com/shayanhabibi/Xantham/commit/cd720b9c5788238cbd5e16a61aa98ae8d6f5b34b))
* *(generator)* Retain named generic callable applications (#167) ([f9660c86](https://github.com/shayanhabibi/Xantham/commit/f9660c86b4e8e03fa03046119f229ee92dab0aa1))
* *(generator)* Preserve declaration-owned alias type parameters (#168) ([9e278bf2](https://github.com/shayanhabibi/Xantham/commit/9e278bf2c76d5012971f536ab2654a7d0e7816be))
* *(generator)* Preserve generic callable interface contracts (#169) ([c50ec1ea](https://github.com/shayanhabibi/Xantham/commit/c50ec1ea0aa2520a92e011242bce30df09828a7c))
* *(generator)* Close seven fidelity gaps from symbol to private members (#173) ([687a15f0](https://github.com/shayanhabibi/Xantham/commit/687a15f013a1ea6531f0f51c5b015c416949c039))

### 🚀 Features

* *(wire)* Migrate to TypeScript 7.1.0-beta and Fickl (#162) ([40a70f03](https://github.com/shayanhabibi/Xantham/commit/40a70f03aee7712a4caf3c388d1600ff11a84b59))

### 🐞 Bug Fixes

* *(generator)* Preserve namespace ownership and recursive callable fidelity (#170) ([8c709082](https://github.com/shayanhabibi/Xantham/commit/8c709082dac4d5289fb6718ae1c488db9962bcb0))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/977355536eba47bff4672d609cccca7523bda9f7..687a15f013a1ea6531f0f51c5b015c416949c039)</small></strong>

## 0.4.0-beta.1 - 2026-10-14

### 🚀 Features

* *(core-ts)* Select on* handler returns as unit (W2.2, #133) ([282c4092](https://github.com/shayanhabibi/Xantham/commit/282c40928a111fe564eb43dfde417938196a492a))
* *(core-ts)* Emit well-known symbol members (W4, #135) ([437c8659](https://github.com/shayanhabibi/Xantham/commit/437c8659a0adb33512d82012a9e4b7051d5690d1))
* *(core-ts)* Write Promise as JS.Promise (W3, #134) ([98e3456f](https://github.com/shayanhabibi/Xantham/commit/98e3456f04f7cb35cf602f79655ae719133181d4))
* *(core-ts)* Write literal overloads as Emit-backed methods (W5, #136) ([a6d10814](https://github.com/shayanhabibi/Xantham/commit/a6d108144ce78d8916ab6e3de8208b45a7ab86f0))
* *(core-ts)* Declare literal members as [<Literal>] modules (W6, #137) ([6d8da2fa](https://github.com/shayanhabibi/Xantham/commit/6d8da2fa9fa8fcc55f4646ed1c93a8266453777c))
* *(core-ts)* Keep type parameters in scope (W7, #138) ([13e59910](https://github.com/shayanhabibi/Xantham/commit/13e599101850811604f60c58bc6a2216801bc6bd))
* *(core-ts)* Emit key types for tag maps (W8, #139) ([90c35bc1](https://github.com/shayanhabibi/Xantham/commit/90c35bc1d1ad4482e3024a47434dc008def807ba))
* *(node)* Resolve conditionals at their bound in Xantham.Fable.Node (#151) ([87f0367d](https://github.com/shayanhabibi/Xantham/commit/87f0367dd29cae0388f8c58eecf6f2aa7fceb71e))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/336800e22243e089e80b6ce406c6f257424569c3..977355536eba47bff4672d609cccca7523bda9f7)</small></strong>

## 0.3.0 - 2026-10-12

### 🚀 Features

* Expose 'globalThis' from Fable.Core (#125) ([336800e](https://github.com/shayanhabibi/Xantham/commit/336800e22243e089e80b6ce406c6f257424569c3))

<strong><small>[View changes on Github](https://github.com/shayanhabibi/Xantham/compare/2f70620ce9950438f4043662a5f964151d9614fb..336800e22243e089e80b6ce406c6f257424569c3)</small></strong>

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
