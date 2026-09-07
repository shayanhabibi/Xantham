---
title: Configuration
order: 1
---

Xantham will detect a `xantham.json` file that is adjacent to its target `package.json`.
When the file is not found, Xantham will fallback to its default settings.

The schema follows the internal configuration that the generator uses, and is generated
directly from the source.

Otherwise, you can view the reference documentation for the generator configuration.

::::tabs
:::tab "Object"

```json wrap
{
  "$schema": "https://raw.githubusercontent.com/shayanhabibi/Xantham/master/xantham.schema.json"
}
```

:::
:::tab "Property"

```text wrap
"$schema": "https://raw.githubusercontent.com/shayanhabibi/Xantham/master/xantham.schema.json"
```

:::
:::tab "Schema URL"

```text wrap
https://raw.githubusercontent.com/shayanhabibi/Xantham/master/xantham.schema.json
```

:::
::::
