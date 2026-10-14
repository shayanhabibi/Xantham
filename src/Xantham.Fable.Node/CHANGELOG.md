---
last_commit_released: 977355536eba47bff4672d609cccca7523bda9f7
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
