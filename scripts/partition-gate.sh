#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Xantham PARTITION GATE — the per-area L1 signal (docs/PLAN.md Phase 1;
# docs/GOALS.md Done(L1, area)). Emits the partitioned units from the committed
# IR + recipe, builds each unit in publish order, and reports OWN-FILE error
# counts per unit against the committed baseline.
#
# Measurement semantics (honest): a unit's own-file count is exact when every
# unit it references produced an assembly; while an upstream unit fails to
# ASSEMBLE (currently: Zod's 2 Fantomas-7.0.1-floor parse sites — the class the
# opaque-handle policy deletes), downstream counts are parse-stage-only and the
# gate marks them "(blocked: <upstream>)" rather than claiming a clean 0.
#
# Run: scripts/partition-gate.sh          (workdir under scripts/.partition-work)
# Exit: non-zero if any unit's own-file count exceeds its baseline.
# ─────────────────────────────────────────────────────────────────────────────
set -uo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="$REPO/scripts/.partition-work"
BASELINE="$REPO/scripts/partition-gate.baseline"

rm -rf "$WORK"
mkdir -p "$WORK"

dotnet build "$REPO/src/Xantham.Generator/Xantham.Generator.fsproj" -c Debug >/dev/null 2>&1 \
  || { echo "generator build failed"; exit 2; }
dotnet run --no-build --project "$REPO/src/Xantham.Generator" -c Debug -- \
  --recipe "$REPO/cloudflare.pilot.toml" --out-dir "$WORK" >"$WORK/emit.log" 2>"$WORK/emit.err" \
  || { echo "emission failed:"; tail -5 "$WORK/emit.err"; exit 2; }

echo "── Xantham partition gate — per-unit own-file FS errors ──"
grep "^pooled into" "$WORK/emit.err" || true

# shellcheck disable=SC1090
source "$BASELINE"

fail=0
declare -A produced
units=$(grep "^unit: " "$WORK/emit.log" | sed 's/^unit: \([^ ]*\) ->.*/\1/')
for unit in $units; do
  log="$WORK/$unit.build.log"
  dotnet build "$WORK/$unit/$unit.fsproj" -c Debug >"$log" 2>&1
  # Own-file = EVERY generated file in the unit dir (the unit body AND ErasedUnions.fs) —
  # not the linked support library (hand-maintained repo source).
  own=$(grep -E " error " "$log" | grep -oE "$unit/($unit|ErasedUnions)\.fs\([0-9]+,[0-9]+\): error [A-Z0-9]+" | sort -u | wc -l | tr -d ' ')
  # blocked: a referenced unit that never produced an assembly
  blocked=""
  for ref in $units; do
    [ "$ref" = "$unit" ] && break
    if [ "${produced[$ref]:-no}" = "no" ] && grep -q "$ref" "$WORK/$unit/$unit.fsproj"; then
      blocked=" (blocked: $ref — parse-stage count only)"
    fi
  done
  if [ -f "$WORK/$unit/bin/Debug/net10.0/$unit.dll" ]; then produced[$unit]=yes; else produced[$unit]=no; fi
  bl_var="BL_$(echo "$unit" | tr '.' '_' | tr '[:lower:]' '[:upper:]')"
  bl=${!bl_var:-0}
  verdict="OK"
  if [ "$own" -gt "$bl" ]; then verdict="FAIL (> $bl)"; fail=1; fi
  printf "%-34s own-file errors: %4s  baseline: %4s  %s%s\n" "$unit" "$own" "$bl" "$verdict" "$blocked"
done

if [ "$fail" -eq 0 ]; then echo "PARTITION GATE: PASS"; else echo "PARTITION GATE: FAIL"; fi
exit $fail
