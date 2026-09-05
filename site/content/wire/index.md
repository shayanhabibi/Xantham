---
title: Usage
order: 0
---

How to consume the package: install it, get a compiler talking, call the API, read what comes
back. For *navigating* the AST once you have it, see [`wire-navigation.md`].

:::steps
1. Install
2. Get a compiler talking
3. Call the api
:::

## Install

```bash frame=terminal
dotnet add package Xantham.TypeScript.Wire
npm install typescript@7.1.0-dev.20260830.1
```

:::caution
The compiler is **not bundled**. Wire runs the Go `tsc` binary shipped in the `typescript` npm
package as `tsc --api`. The protocol is unversioned, so the npm pin must match the version the
package was generated against — mismatches surface as decode failures, not as a version error.
:::
