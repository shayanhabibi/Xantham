#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Xantham IR OPEN-TWIN sentinel.
#
# WHY: the encoder can emit an INSTANTIATED object literal as a byte-identical
# copy of its OPEN generic declaration (members still pointing at TypeParameter
# keys) — `JsonSchemaType = JSONSchema.Interface` came out identical to the open
# JSONSchema object body, so the decoder's structural compress (correctly, given
# the IR) merged them and a paramless alias claimed the generic body. This
# sentinel measures that class in the IR itself, located BY EXPORT NAME (keys
# are renumbered by encoder changes — never hardcode them).
#
# Metrics:
#   CONFLATED          1 if JsonSchemaType's body is byte-identical to the open
#                      JSONSchema object member (the exemplar defect), else 0
#   OPEN_TWIN_GROUPS   count of byte-identical TypeLiteral shape-groups (size>1)
#                      whose members directly reference a TypeParameter node
#   OPEN_TWIN_KEYS     total keys across those groups
#
# Gate vs scripts/ir-sentinel.baseline: no metric may INCREASE. After the
# stage-1 encoder fix, ratchet CONFLATED to 0 and the group count down.
#
# Run:  scripts/ir-sentinel.sh [path/to/output.json]
# ─────────────────────────────────────────────────────────────────────────────
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IR="${1:-$REPO/src/Xantham.Fable/output.json}"
BASELINE="$REPO/scripts/ir-sentinel.baseline"

python3 - "$IR" "$BASELINE" <<'PY'
import json, sys, os

ir_path, baseline_path = sys.argv[1], sys.argv[2]
data = json.load(open(ir_path))
types = dict((p[0], p[1]) for p in data["Types"] if isinstance(p, list) and len(p) == 2)

def kind(v): return list(v.keys())[0] if isinstance(v, dict) and len(v) == 1 else None
tp_keys = {k for k, v in types.items() if kind(v) == "TypeParameter"}

# ── exemplar: JsonSchemaType vs the open JSONSchema object member ────────────
aliases = []
def walk(o):
    if isinstance(o, dict):
        if "TypeParameters" in o and "Type" in o and "Name" in o and "FullyQualifiedName" in o:
            aliases.append(o)
        for v in o.values(): walk(v)
    elif isinstance(o, list):
        for v in o: walk(v)
walk(data)

def named(name, ntypars=None):
    out = [a for a in aliases if a.get("Name") == name]
    if ntypars is not None:
        out = [a for a in out if len(a.get("TypeParameters") or []) == ntypars]
    return out

conflated = 0
js = named("JSONSchema", 2)          # the generic json-schema-typed alias
jst = named("JsonSchemaType", 0)     # the paramless MCP alias
if js and jst:
    body = types.get(js[0]["Type"])
    if body is not None and kind(body) == "Union":
        members = body["Union"]["Types"] if isinstance(body["Union"], dict) else body["Union"]
        open_lits = [k for k in members if k in types and kind(types[k]) == "TypeLiteral"]
        inst = types.get(jst[0]["Type"])
        for k in open_lits:
            if inst is not None and types[k] == inst:
                conflated = 1
    print(f"exemplar: JSONSchema<2> found={bool(js)}, JsonSchemaType<0> found={bool(jst)}, CONFLATED={conflated}")
else:
    print(f"exemplar: JSONSchema<2> found={bool(js)}, JsonSchemaType<0> found={bool(jst)} — exemplar check skipped")

# ── global: byte-identical TypeLiteral groups with direct TypeParameter refs ─
groups = {}
for k, v in types.items():
    if kind(v) != "TypeLiteral": continue
    groups.setdefault(json.dumps(v, sort_keys=True), []).append(k)

def has_direct_tp(v):
    for m in v["TypeLiteral"]["Members"]:
        mk = kind(m)
        body = m[mk] if mk else m
        if isinstance(body, dict) and body.get("Type") in tp_keys:
            return True
    return False

open_twin_groups = [ks for shape, ks in groups.items() if len(ks) > 1 and has_direct_tp(json.loads(shape))]
open_twin_keys = sum(len(g) for g in open_twin_groups)
print(f"OPEN_TWIN_GROUPS={len(open_twin_groups)}  OPEN_TWIN_KEYS={open_twin_keys}")
for g in sorted(open_twin_groups, key=len, reverse=True)[:5]:
    print(f"  group size {len(g)}: keys {sorted(g)[:8]}{'...' if len(g) > 8 else ''}")

# ── baseline ─────────────────────────────────────────────────────────────────
metrics = {"CONFLATED": conflated, "OPEN_TWIN_GROUPS": len(open_twin_groups), "OPEN_TWIN_KEYS": open_twin_keys}
baseline = {}
if os.path.exists(baseline_path):
    for line in open(baseline_path):
        line = line.strip()
        if not line or line.startswith('#'): continue
        k, _, v = line.partition('=')
        baseline[k] = int(v)
    fail = False
    for k, v in metrics.items():
        b = baseline.get(k, 0)
        status = "OK" if v <= b else "FAIL"
        if v > b: fail = True
        print(f"{k}: {v} vs baseline {b} — {status}")
    print("IR SENTINEL:", "FAIL" if fail else "PASS")
    sys.exit(1 if fail else 0)
else:
    with open(baseline_path, 'w') as f:
        f.write("# Xantham IR open-twin sentinel baseline — encoder identity-conflation metrics.\n")
        f.write("# No metric may increase; ratchet DOWN as the encoder fix lands (stage 1: CONFLATED=0).\n")
        for k, v in metrics.items():
            f.write(f"{k}={v}\n")
    print(f"No baseline — wrote current metrics to {baseline_path}.")
    sys.exit(0)
PY
