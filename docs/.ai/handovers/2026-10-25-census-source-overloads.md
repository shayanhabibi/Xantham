# Census source-overload lane

Branch: codex/census-source-overloads
Base: 400d96f9b4dfa946b47b4316ecc0e38c11a825a0

## Change

Shape/Exports.fs retains only the first nongeneric direct-function overload with identical source parameter TypeId/Optional pairs. Eligibility requires resolved declaration-source facts and excludes rest and explicit-this signatures. Original expanded ordinals are assigned before filtering. Model/Resolve carry HasThisParameter from SignatureResponse.ThisParameter, so Shape remains pure; Build.signature supplies false for hand-built tests. Record-update signature clones preserve the field. No finding codes or compatibility contract versions changed in this lane.

The focused lab pins a narrow first return, reversed source order with broad first return, identical optional inputs, a later numeric overload retaining ordinal2, and negative boolean literal/generic/receiver/rest collisions. A per-pass test exercises live Harvest/Resolve plus Shape through shapeExports and inspects OwnedExportMember.SignatureOrdinal and numeric parameter/return identity. No runtime check was added: emitted call interop is unchanged; source return selection and provenance are the behavior under test.

## Verification

- First quick command failed NETSDK1004 because the new worktree had no restored assets; this was infrastructure, not RED. Solution restore then succeeded. Existing site NU1608 FSharp.Compiler.Service/FSharp.Core constraint warning remains.
- Meaningful RED: solution build succeeded; focused5 tests ran,3 passed/2 failed/0errored. First return emitted U2 instead of string, and pick ordinals were0,1,2 instead of0,2. Initial test spacing assertions were corrected before recording this RED.
- GREEN: dotnet fsi build.fsx -- test --quick --update --filter "census source overloads". The solution build succeeded;5/5 passed in both write and check phases,0 ignored/0 errored. Miscellaneous wire filter selected0 as expected; generator suite selected5. Logs in main tests/.scratch/census-loop/source-overloads-{red,green,restore,format}.log.
- FCS generator project check clean:0 errors/0 warnings;16 informational below-floor diagnostics. Impact tool: ResolvedSignature is not public API;152 internal sites in10files. A broad workspace query on the cold lane could not analyze6 sibling projects and reported missing references in unrestored test projects; it was not used as absence evidence. Focused actual solution build subsequently succeeded.
- Fantomas: Exports.fs formatted, Model/Resolve/Build unchanged. Other source changes after GREEN were documentation only.
- Tier audit against base: TOTAL WORSE0; only new lab symbols.jsonl present. Final lab7symbols:2exact/1ergonomic/4widened/0escape. Final DO0084 are precisely the literal/generic/receiver/rest controls; pick/optional have no DO008, and reverse keeps its original broad union without a redundant return-merge finding.

## Integration

Expected real subset: Node readlinkSync/mkdtempSync and promises realpath/mkdtemp (at least4 DO008 cases). No Core/Node/full-corpus regeneration or full batch/runtime gate ran in lane; coordinator owns those and compatibility assessment. Never extend this to merely equal emitted F# parameters: distinct source inputs must retain late collision handling.
