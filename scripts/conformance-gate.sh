#!/usr/bin/env bash
# ── Xantham TRANSPILE-CONFORMANCE gate ────────────────────────────────────────
# WHY: the FS-error harness (`dotnet build` of the generated F#) only proves the
# bindings TYPE-CHECK under .NET's F# compiler. That compiler UNDERCOUNTS: in the
# giant `module rec` surface, its name resolution can absorb a broken reference
# (a same-named in-scope type), so a defect that a real Fable consumer WOULD hit
# is silently accepted. Fable's own frontend (Fantomas.FCS) does not mask these —
# it is the TRUER conformance signal. This gate transpiles the generated bindings
# through Fable (F# -> JS) and counts the errors Fable reports.
#
# Two error kinds:
#   FSHARP — F# frontend errors on the generated bindings (arity, undefined names,
#            unbound typars). These are the binding defects to drive down.
#   FABLE  — Fable-specific "construct not supported" (mostly in the runtime support
#            lib Library.fs, e.g. KeyOf.item). Reported separately, not the target.
#
# Run:  scripts/conformance-gate.sh
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GEN_INPUT="$REPO/src/Xantham.Fable/output.json"
LIBRARY="$REPO/src/Xantham.Fable.Core/Library.fs"
GOLDEN="$REPO/scripts/conformance-gate.baseline"
WORK="$(mktemp -d)"

echo "── Xantham transpile-conformance gate ──"

# ── 0. build generator + regenerate the bindings ─────────────────────────────
dotnet build "$REPO/src/Xantham.Generator/Xantham.Generator.fsproj" -c Debug >/dev/null 2>&1 \
  || { echo "FATAL: generator build failed"; exit 2; }
dotnet run --no-build --project "$REPO/src/Xantham.Generator" -c Debug 1>"$WORK/gen.fs" 2>/dev/null

# ── 1. assemble the Fable transpile project (support lib + wrapped bindings) ──
{
  echo "module rec CloudflareWorkersTypes"; echo
  echo "open System"; echo "open Fable.Core"; echo "open Fable.Core.JS"; echo "open Fable.Core.JsInterop"; echo
  cat "$WORK/gen.fs"
} > "$WORK/workersTypes.wrapped.fs"
cat > "$WORK/Conformance.fsproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup>
  <ItemGroup>
    <Compile Include="$LIBRARY"><Link>XanthamFableCoreLibrary.fs</Link></Compile>
    <Compile Include="workersTypes.wrapped.fs" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Fable.Core" Version="5.0.0-beta.3" />
  </ItemGroup>
</Project>
EOF

# ── 2. Fable transpile + count ────────────────────────────────────────────────
dotnet fable --cwd "$WORK" -o "$WORK/out" --lang js --noCache 2>&1 \
  | sed -E 's/\x1b\[[0-9;]*m//g' > "$WORK/fable.log"

# FSHARP errors on the generated bindings (exclude the support-lib file).
fsharp_bindings=$(grep -E "error FSHARP" "$WORK/fable.log" | grep -v "XanthamFableCoreLibrary\|Library.fs" | wc -l | tr -d ' ')
fable_lib=$(grep -cE "error FABLE" "$WORK/fable.log" | tr -d ' ')

printf '%-52s %s\n' "FSHARP errors on generated bindings" "$fsharp_bindings"
printf '%-52s %s\n' "FABLE errors (support lib — informational)" "$fable_lib"
echo

# ── 3. ratchet ────────────────────────────────────────────────────────────────
BL_FSHARP=9999
[ -f "$GOLDEN" ] && . "$GOLDEN"
if [ "$fsharp_bindings" -le "$BL_FSHARP" ]; then
  echo "CONFORMANCE GATE: PASS  ($fsharp_bindings <= $BL_FSHARP)"
  exit 0
else
  echo "CONFORMANCE GATE: FAIL  ($fsharp_bindings > $BL_FSHARP)"
  echo "  top error classes:"
  grep -oE "error FSHARP: The type '[A-Za-z_.<>]+' (is not defined|expects)" "$WORK/fable.log" \
    | sed -E "s/'[A-Za-z_.<>]+'/'X'/g" | sort | uniq -c | sort -rn | head -5 | sed 's/^/    /'
  exit 1
fi
