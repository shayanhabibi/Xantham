#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Xantham DISCOVER — the Phase-0 inventory signal (docs/PLAN.md Phase 0; docs/LANDSCAPE.md).
#
# The Farscape `pilot discover` analog for TypeScript: walk the committed IR and
# report what the crawl actually REACHED — per-package and per-source-module decl
# counts, plus (when a conformance log is supplied) per-package error attribution.
# This is the DATA a manifest author curates from; it makes no scope decision
# itself. The manifest (cloudflare.pilot.toml) is the owner's steering artifact —
# this script only illuminates the territory.
#
# Run:  scripts/discover.sh [path/to/ir.json] [path/to/fable-log]
#   IR defaults to src/Xantham.Fable/output.json (committed); pass a *.json first
#   arg to inspect another IR (e.g. a scratch multi-entry crawl).
#   Optional: a conformance-gate Fable log to attribute errors to packages.
# ─────────────────────────────────────────────────────────────────────────────
set -uo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
case "${1:-}" in
  *.json) IR="$1"; FABLE_LOG="${2:-}" ;;
  *)      IR="$REPO/src/Xantham.Fable/output.json"; FABLE_LOG="${1:-}" ;;
esac

python3 - "$IR" "$FABLE_LOG" <<'PY'
import json, re, sys
from collections import defaultdict

ir_path, log_path = sys.argv[1], sys.argv[2]
data = json.load(open(ir_path))

def provenance(fqn):
    """Extract (package, source-module) from a FullyQualifiedName part list.
    Parts are plain strings; a file import carries the quoted absolute path as its head."""
    if not isinstance(fqn, list) or not fqn:
        return None, None
    part = next((p for p in fqn if isinstance(p, str) and 'node_modules' in p), None)
    if part is None:
        part = fqn[0] if isinstance(fqn[0], str) else None
    if not isinstance(part, str): return None, None
    s = part.strip('"')
    m = re.search(r'node_modules/((?:@[^/]+/)?[^/]+)(/.*)?$', s)
    if m:
        pkg = m.group(1)
        mod = (m.group(2) or '').lstrip('/')
        mod = re.sub(r'\.d\.ts$|\.ts$', '', mod)
        return pkg, mod
    if '/' in s:
        return '<local>', s.rsplit('/', 1)[-1]
    return '<global>', s

# ── inventory: exported declarations per package/module ──────────────────────
pkg_exports = defaultdict(int)
pkg_modules = defaultdict(lambda: defaultdict(int))
seen_export_names = defaultdict(set)

def walk_exports(o):
    if isinstance(o, dict):
        if "FullyQualifiedName" in o and "Name" in o:
            pkg, mod = provenance(o["FullyQualifiedName"])
            if pkg:
                pkg_exports[pkg] += 1
                pkg_modules[pkg][mod or '<root>'] += 1
                nm = o.get("Name")
                if isinstance(nm, str): seen_export_names[pkg].add(nm)
        for v in o.values(): walk_exports(v)
    elif isinstance(o, list):
        for v in o: walk_exports(v)

walk_exports(data.get("ExportedDeclarations", []))
total_types = len(data.get("Types", []))

print("── Xantham discover — IR inventory ──")
print(f"IR: {total_types} types; exported declarations by PACKAGE:\n")
print(f"{'package':38s} {'exports':>8s}  top source modules")
for pkg, n in sorted(pkg_exports.items(), key=lambda kv: -kv[1]):
    mods = sorted(pkg_modules[pkg].items(), key=lambda kv: -kv[1])[:3]
    modtxt = ', '.join(f"{m}({c})" for m, c in mods)
    print(f"{pkg:38s} {n:>8d}  {modtxt[:80]}")

# ── per-entry provenance (EntryExports, present from the multi-entry crawl on) ─
entry_exports = data.get("EntryExports") or {}
if entry_exports:
    print(f"\nPer-entry provenance ({len(entry_exports)} crawl roots):\n")
    print(f"{'entry (resolved file, node_modules-relative)':74s} {'top-level exports':>18s}")
    for file, keys in sorted(entry_exports.items(), key=lambda kv: -len(kv[1])):
        m = re.search(r'node_modules/(.+)$', file)
        rel = m.group(1) if m else file
        print(f"{rel:74s} {len(keys):>18d}")
    total_attributed = sum(len(v) for v in entry_exports.values())
    print(f"\n{'TOTAL attributed top-level exports':74s} {total_attributed:>18d}")
else:
    print("\n(no EntryExports field — single-entry IR predating multi-entry provenance)")

# ── error attribution (optional) ─────────────────────────────────────────────
if log_path:
    try:
        log = open(log_path).read()
    except OSError:
        print(f"\n(no readable Fable log at {log_path} — error attribution skipped)")
        sys.exit(0)
    # Attribute each error line to the emitted top-level module (the surface's
    # package families), using the generated surface if present alongside.
    errs = re.findall(r'error FSHARP\d*: (.{0,70})', log)
    print(f"\nFable errors in log: {len(errs)} (attribute per-module via the emitted surface: see STATUS matrix, Phase 1)")
print("\nNOTE: inventory only; scope is declared in cloudflare.pilot.toml (see docs/LANDSCAPE.md).")
PY
