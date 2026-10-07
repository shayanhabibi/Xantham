# Generator customization execution record

Implementation branch: `agent/generator-customization`, based on the user snapshot `d875384`.
The main checkout's unrelated edits are outside this implementation.

The supported API covers immutable source selection, typed member attributes, explicit property
interop changes, independent property companions, and structured or compiler-validated raw
interface replacements. `Semantic.members` includes materialized function/value exports and
instance methods; `Semantic.properties` restricts companion selection to effective properties.
The example selects actual compiler HTMLElement heritage and generates a property-free Partas
component using `value`, inherited `title`, and readonly `tagName`.

One fresh whole-branch review identified five important gaps. Regression tests reproduced the
missing concrete entrypoint annotations, missing exported-member targets, unqualified producer
type arguments, invalid generic replacement selection, and occupied module namespaces. All
were corrected in one fix pass. The focused customization suite passes 22/22 tests, including
compiled consumers and readonly rejection.

The actual Partas 3.0.0 plugin source is pinned by the checked-in archive and per-file digests;
archive SHA256 is `7f72aebe18c5e243e3dadc1c948cda6b54b38811df0091fd7e3257463d1a08d1`.
The archive is based on `0d43cb5e3d9de95c74e92c89547a1b4439a32979` plus the tested local source,
with SDK/support project adaptations documented in the gate README. Runtime npm dependencies
are lockfile-pinned, including Solid and signals 2.0.0-rc.9.

Fresh FCS checking reports zero errors/warnings. The complete current public surface contains
193 entities; opaque customization contracts expose no internal pass records. The initial API
baseline capture contained only its first 45 entities, so it is not evidence for a complete
signature comparison. Existing public records and entrypoint signatures were retained, including
`DeclarationCatalog.Catalog`; customization variants are optional JSON sidecar metadata.

Existing golden files are unchanged; only the new customization lab adds goldens. Finding
inventory comparison shows all 111 baseline package inventories unchanged.

Final verification: `rtk dotnet build Xantham.slnx` passes; Fantomas and diff whitespace checks
pass. `rtk dotnet fsi build.fsx -- test --quick --run-gate` passes 992 generator tests and 99 Wire
tests, plus 468 original Fable runtime checks, exact Partas plugin JSX, direct source/DLL
getter/setter execution, and one Solid browser runtime test. The final complete suite used
`--quick` after setup and rebuilding a stale Debug support reference assembly; this skips setup,
not tests. Existing site dependency warnings remain unchanged. No existing golden was rewritten.

## Rulings

1. Use Git for Windows bash for skill tooling because PATH bash resolved to WSL. Cost if wrong:
   tooling invocation only.
2. Emit Partas companions under `Partas.Solid` and implement `HtmlElement`. The real plugin
   dropped extensions outside that namespace; `RegularNode` has a competing title extension.
   Cost if wrong: another framework adapter may need explicit receiver casts or plugin support.
3. Put excluded Fable projects in separate directories because its implicit project build fails
   with multiple projects in one directory. Cost if wrong: acceptance harness layout only.
4. Aliases have separate query paths/owners but share canonical declaration identity. Cost if
   wrong: source alias selection behavior.
5. Keep customization annotations beside declarations rather than adding pass-record fields.
   Cost if wrong: catalog hashing must explicitly include annotation metadata.
6. Restrict raw replacement to non-generic interfaces and reject raw catalog production. The
   compilation witness authenticates an export, not arbitrary generic source contracts. Cost
   if wrong: additional raw forms require stronger contract validation.
7. Generate acceptance projects in isolated scratch directories rather than a tracked
   PartasGate.fsproj. Ordinary solution builds must not depend on scratch extraction/generated
   companions. Cost if wrong: gate project XML needs explicit maintenance.
8. Pin the tested Partas source archive, including SDK/support adaptations, because its original
   uncommitted source cannot be reconstructed from a Git revision alone. Cost if wrong: update
   the archive and hashes together when adopting a new Partas source.
9. The reviewer declined to judge pending full-test status; the implementer settles it using
   fresh gate output. Cost if wrong: completion must not be claimed without that output.

## Deferred minor

Removed-member conflicts sometimes name only the latest extension rather than both owners.
The edit still rejects before generated output is written.
