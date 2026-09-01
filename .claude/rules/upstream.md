---
paths:
    - "tools/tsc-ast/**/*"
---

# Vendored upstream sources — fetch on demand, never commit

`tools/tsc-ast/upstream/` holds `microsoft/TypeScript` sources pulled from the commit pinned in
`upstream.json`. **The lock is committed; the tree is not.** It is gitignored, so it is absent
from a fresh clone and from every agent worktree, which carry tracked files only.

- Vendor it when you need it: `dotnet fsi tools/generate-wire.fsx -- sync tsc-ast`. This hits the
  network and is never run implicitly — `generate ast` fails with that command in the message
  rather than fetching behind your back.
- `generate ast` needs it. `generate proto` does not; it reads the shipped schema out of the
  `typescript` npm package instead.
- Do not borrow another checkout's `upstream/`, and do not copy it between worktrees. Each
  checkout vendors its own, verified against `upstream.lock.json`.
- Do not add the tree to git, and do not hand-edit anything under it. Files there are
  byte-for-byte upstream and are digest-checked on the next sync.
- To move the pin, edit `ref` in `upstream.json`, re-sync, and commit the resulting
  `upstream.lock.json` alongside whatever regenerated output the new sources produce.
- `--check` reports drift instead of overwriting; use it to confirm a tree still matches the lock.
