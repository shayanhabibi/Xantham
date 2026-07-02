#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Xantham ARITY-AGREEMENT gate.
#
# WHY: the decl arity of a generated type and the arity of every reference to it
# are produced by DIFFERENT machinery (the decl by the alias/typedefn renderers;
# the refs by the TypeAliasArity padding, the cdc0108 typeArguments fallback, and
# the general-args arm). A change that moves one side without the others emits
# `Name<obj,obj>` against `type Name = ...` (or vice versa) — the exact class
# that regressed 511→707 in the reverted phantom-typar attempt, and one the
# semantic gate is blind to. This gate makes decl/ref arity agreement MECHANICAL.
#
# Method: parse the generated surface; for every `type X<...> =` / `type X =`
# declaration record its arity; for every textual application `X<...>` count its
# top-level args. A simple name with MULTIPLE decls of DIFFERING arity (twins:
# lib.dom/workers, multi-version packages) is UNVERIFIABLE and skipped (reported).
# Method-level typar lists (`abstract m<'T> :`, `member m<'T>`) are excluded.
# Pre-existing disagreements are baselined PER NAME (scripts/arity-gate.baseline);
# the gate FAILS only on new names or a name's disagreement count increasing.
#
# Run:  scripts/arity-gate.sh [path/to/generated-surface.fs]
#   With no argument, regenerates the surface from the committed IR.
# Exit 0 iff no disagreement beyond the committed baseline.
# ─────────────────────────────────────────────────────────────────────────────
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BASELINE="$REPO/scripts/arity-gate.baseline"

SURFACE="${1:-}"
if [ -z "$SURFACE" ]; then
  WORK="$(mktemp -d)"
  SURFACE="$WORK/surface.fs"
  dotnet build "$REPO/src/Xantham.Generator/Xantham.Generator.fsproj" -c Debug >/dev/null 2>&1 \
    || { echo "FATAL: generator build failed"; exit 2; }
  dotnet run --no-build --project "$REPO/src/Xantham.Generator" -c Debug 1>"$SURFACE" 2>/dev/null
fi

python3 - "$SURFACE" "$BASELINE" <<'PY'
import re, sys, os

surface_path, baseline_path = sys.argv[1], sys.argv[2]
text = open(surface_path).read()

# ── declarations ─────────────────────────────────────────────────────────────
# `type Name<...> =` / `and Name<...> =` — Fantomas 7.0.1 may wrap the typar
# list across lines, so match across newlines; the `<...>` never contains `=`
# (F# typar lists carry no defaults) so [^=] is a safe span.
decl_re = re.compile(r'\b(?:type|and)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(<[^=]*?>)?\s*=', re.S)

def typar_arity(span: str) -> int:
    # count top-level commas in `<...>`, ignoring the `when` constraint tail
    inner = span[1:-1]
    when = inner.find(' when ')
    if when >= 0: inner = inner[:when]
    depth = 0; commas = 0
    for ch in inner:
        if ch == '<': depth += 1
        elif ch == '>': depth -= 1
        elif ch == ',' and depth == 0: commas += 1
    return commas + 1 if inner.strip() else 0

decls: dict[str, set[int]] = {}
for m in decl_re.finditer(text):
    name, span = m.group(1), m.group(2)
    decls.setdefault(name, set()).add(typar_arity(span) if span else 0)

verifiable = {n: next(iter(a)) for n, a in decls.items() if len(a) == 1}
ambiguous  = {n: sorted(a)     for n, a in decls.items() if len(a) > 1}

# ── applications ─────────────────────────────────────────────────────────────
# `Name<...>` with `->`-aware bracket matching (arrow `>` is not a closer).
# Exclusions: the decl site itself (preceded by type/and), method typar lists
# (preceded by abstract/member/val + optional inline), erased-union U{n} heads.
app_re = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*<')
skip_prefix_re = re.compile(
    r'(?:\btype\s+|\band\s+|\babstract\s+(?:member\s+)?|\bmember\s+(?:inline\s+)?|\bval\s+|\bstatic\s+member\s+(?:inline\s+)?)$')

def app_arity(pos: int) -> int | None:
    # pos = index of '<'; returns top-level arg count or None if unbalanced
    depth = 0; commas = 0; i = pos
    n = len(text)
    while i < n:
        c = text[i]
        if c == '-' and i + 1 < n and text[i+1] == '>':
            i += 2; continue
        if c == ':' and i + 1 < n and text[i+1] == '>':   # subtype constraint :>
            i += 2; continue
        if c == '<': depth += 1
        elif c == '>':
            depth -= 1
            if depth == 0:
                return commas + 1
        elif c == ',' and depth == 1:
            commas += 1
        elif c == '\n' and depth == 0:
            return None
        i += 1
    return None

disagreements: dict[str, int] = {}
checked = 0
for m in app_re.finditer(text):
    name = m.group(1)
    if name not in verifiable: continue
    if re.match(r'U\d+$', name): continue
    head = text[max(0, m.start()-40):m.start()]
    if skip_prefix_re.search(head): continue
    arity = app_arity(m.end() - 1)
    if arity is None: continue
    checked += 1
    if arity != verifiable[name]:
        disagreements[name] = disagreements.get(name, 0) + 1

# ── baseline compare ─────────────────────────────────────────────────────────
baseline: dict[str, int] = {}
if os.path.exists(baseline_path):
    for line in open(baseline_path):
        line = line.strip()
        if not line or line.startswith('#'): continue
        k, _, v = line.partition('=')
        baseline[k] = int(v)

print(f"decls: {sum(len(a) for a in decls.values())} ({len(verifiable)} verifiable names, "
      f"{len(ambiguous)} ambiguous-twin names skipped)")
print(f"applications checked: {checked}")
print(f"disagreeing names: {len(disagreements)} (baseline: {len(baseline)})")

new_names  = {n: c for n, c in disagreements.items() if n not in baseline}
worsened   = {n: (c, baseline[n]) for n, c in disagreements.items() if n in baseline and c > baseline[n]}

fail = False
if new_names:
    fail = True
    print("\nNEW decl/ref arity disagreements (not in baseline):")
    for n, c in sorted(new_names.items()):
        print(f"  {n}: decl arity {verifiable[n]}, {c} disagreeing application(s)")
if worsened:
    fail = True
    print("\nWORSENED disagreements:")
    for n, (c, b) in sorted(worsened.items()):
        print(f"  {n}: {b} -> {c}")

if not os.path.exists(baseline_path):
    with open(baseline_path, 'w') as f:
        f.write("# Xantham arity-gate baseline — pre-existing decl/ref arity disagreements PER NAME.\n")
        f.write("# The gate fails on NEW names or a name's count increasing. Shrink as fixes land.\n")
        for n, c in sorted(disagreements.items()):
            f.write(f"{n}={c}\n")
    print(f"\nNo baseline — wrote current {len(disagreements)} disagreement(s) to {baseline_path}.")
    sys.exit(0)

print("\nARITY GATE:", "FAIL" if fail else "PASS")
sys.exit(1 if fail else 0)
PY
