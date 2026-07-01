#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Xantham SEMANTIC golden gate.
#
# WHY: the FS-error count (does the generated F# compile?) is NECESSARY but NOT
# SUFFICIENT for the goal — usable, idiomatic TS->F#/Fable bindings. A build can
# reach 0 errors and still ship bindings that are unusable: TS callbacks emitted
# as nominal `Invoke` interfaces an F# lambda cannot satisfy, static-sides leaking
# as `Prototype` members, and machine-smash public names (`SharedLiterals.Lit25`,
# 60-char concatenations). The error gate is structurally BLIND to all of these —
# they compile. This gate measures the things that actually determine usability.
#
# It is the STEERING SIGNAL the project's own memory warns the FS-count is not.
# A fix is real progress only if it moves these metrics in the right direction
# (or holds them while dropping FS-errors) — not if it merely shuffles which
# error class is reported.
#
# Run:  scripts/golden-gate.sh [path/to/staged-ir.json]
#   default IR: the workers-types staged IR in the session scratchpad.
# Exit 0 iff every REQUIRED check passes against the committed golden baseline.
# ─────────────────────────────────────────────────────────────────────────────
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GEN_INPUT="$REPO/src/Xantham.Fable/output.json"
# Default IR = the committed production IR (the fresh agents@0.17.1 surface). Pass a path to gate a
# different staged IR. (Previously defaulted to a session-scratch cf-staged.json that no longer
# exists — harmless but produced noisy `cp: cannot stat`.)
IR="${1:-$GEN_INPUT}"
WORK="$(mktemp -d)"
GOLDEN="$REPO/scripts/golden-gate.baseline"   # committed baseline of the metrics

fail=0
note() { printf '%-52s %s\n' "$1" "$2"; }
req()  { # name actual budget(<=)  — REQUIRED: actual must be <= budget
  local n="$1" a="$2" b="$3"
  if [ "$a" -le "$b" ]; then note "$n" "OK   ($a <= $b)"; else note "$n" "FAIL ($a  > $b)"; fail=1; fi
}

echo "── Xantham semantic golden gate ──"
echo "IR: $IR"
echo

# ── 0. build generator ───────────────────────────────────────────────────────
dotnet build "$REPO/src/Xantham.Generator/Xantham.Generator.fsproj" -c Debug >/dev/null 2>&1 \
  || { echo "FATAL: generator build failed"; exit 2; }

# ── 1. regenerate TWICE (determinism is a hard requirement) ──────────────────
gen() { # $1 = out file
  [ "$IR" -ef "$GEN_INPUT" ] || cp "$IR" "$GEN_INPUT"   # skip self-copy when IR is the committed IR
  dotnet run --no-build --project "$REPO/src/Xantham.Generator" -c Debug 1>"$1" 2>/dev/null
}
gen "$WORK/run1.fs"
gen "$WORK/run2.fs"
git -C "$REPO" checkout -- "$GEN_INPUT" 2>/dev/null || true   # restore tracked output.json
OUT="$WORK/run1.fs"

if diff -q "$WORK/run1.fs" "$WORK/run2.fs" >/dev/null; then
  note "determinism (regen x2 byte-identical)" "OK"
else
  note "determinism (regen x2 byte-identical)" "FAIL — output is non-deterministic"
  fail=1
fi

lines=$(wc -l < "$OUT")
note "generated lines" "$lines"
echo

# ── 2. SEMANTIC metrics (the count-blind class) ──────────────────────────────
# (a) TS callbacks emitted as nominal `Invoke` interfaces used in PARAMETER
#     position — an F# lambda cannot be passed. The single most usability-breaking
#     defect. A callback should be an F# function type `('T -> 'R)`.
callback_iface=$(grep -cE 'abstract Invoke:' "$OUT")

# (b) static-sides / constructor-sides leaking as `Prototype`-typed members.
prototype_members=$(grep -cE ': [A-Za-z.]*Prototype\b' "$OUT")

# (c) machine-smash public names: `SharedLiterals.Lit<N>` numbered refs +
#     pathological >40-char type names (declaration-derived names are short/legible).
lit_numbered=$(grep -coE 'SharedLiterals\.Lit[0-9]+' "$OUT")
smash_names=$(grep -coE 'type [A-Za-z0-9]{40,}' "$OUT")

# (d) synthesized-home penetration of the PUBLIC surface: every SharedLiterals./
#     LiteralUnions. reference in a member signature is a machine name the consumer
#     sees. Track it — ideally these are an internal detail, not public API.
synth_public=$(grep -coE '(SharedLiterals|LiteralUnions)\.[A-Za-z0-9_]+' "$OUT")

echo "Semantic metrics (lower is better; these are what 'usable' means):"
note "callbacks as nominal Invoke interfaces" "$callback_iface"
note "static-sides as Prototype members"      "$prototype_members"
note "SharedLiterals.Lit<N> machine names"     "$lit_numbered"
note "smash type names (>40 char)"             "$smash_names"
note "synth-home refs in public surface"       "$synth_public"
echo

# ── 3. gate against the committed baseline (ratchet: must not regress) ────────
if [ -f "$GOLDEN" ]; then
  # shellcheck disable=SC1090
  source "$GOLDEN"
  echo "Gate vs committed baseline ($GOLDEN) — REQUIRED to not regress:"
  req "callbacks-as-Invoke"      "$callback_iface"    "${BL_CALLBACK_IFACE:-9999}"
  req "Prototype members"        "$prototype_members" "${BL_PROTOTYPE:-9999}"
  req "Lit<N> machine names"     "$lit_numbered"      "${BL_LIT_NUMBERED:-9999}"
  req "smash names"              "$smash_names"       "${BL_SMASH:-9999}"
  req "synth-home public refs"   "$synth_public"      "${BL_SYNTH_PUBLIC:-9999}"
else
  echo "No baseline at $GOLDEN — writing current metrics as the initial golden."
  cat > "$GOLDEN" <<EOF
# Xantham semantic golden baseline — the ratchet for the semantic gate.
# Lower each as fixes land; the gate FAILS if any metric regresses above these.
BL_CALLBACK_IFACE=$callback_iface
BL_PROTOTYPE=$prototype_members
BL_LIT_NUMBERED=$lit_numbered
BL_SMASH=$smash_names
BL_SYNTH_PUBLIC=$synth_public
BL_LINES=$lines
EOF
  echo "Baseline written. Re-run to gate against it."
fi
echo

if [ "$fail" -eq 0 ]; then echo "GOLDEN GATE: PASS"; exit 0
else echo "GOLDEN GATE: FAIL"; exit 1; fi
