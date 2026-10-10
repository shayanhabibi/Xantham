# Branch and release workflow

Feature and dependency pull requests target `develop`. Release pull requests normally
merge `develop` into `master` using a merge commit, preserving shared branch history.

Both branches block deletion and force pushes, including for maintainers. Pull requests
require successful GitHub Actions checks, an up-to-date branch, and resolved review
conversations. No additional approving reviewer is required. Auto-merge is available.

`develop` requires the `test` and `conventional-title` checks. `shayanhabibi` and `houstonhaynes` can bypass its
pull-request and check requirements for direct pushes; push-triggered tests still run.
Write-enabled repository deploy keys also bypass this develop rule so the release
workflow can push version/changelog commits. There is one release deploy key;
its private key is held in the `RELEASE_DEPLOY_KEY` secret in the
`release-preparation` environment. Only master workflows can access that
environment, without a manual approval step; feature workflows cannot read the key.
The history protection and master rules have no bot exception. Other contributors
use pull requests.

Feature PRs use conventional titles and squash merging; release PRs from develop
preserve history with merge commits. Maintainers prepare versions and changelogs
with `/release` on the develop-to-master PR using ShipIt; see
[CONTRIBUTING.md](../CONTRIBUTING.md). The master package
check rejects unchanged versions in affected packages and their dependent packages.

`master` requires `test`, `package`, and `conventional-title`, with no bypass actors. The Publish workflow
packs and validates every pull request to `master` without publishing credentials.
Only the subsequent `master` push (or manual dispatch on `master`) can publish the
verified package artifact to the temporary Cloudsmith feed. Documentation deployment
and golden tagging remain restricted to `master`.

The protection rules are configured in GitHub under Settings → Rules → Rulesets:

- Branch history: force-push and deletion protection for both branches, no bypass.
- Develop integration: PR and test requirements, with the two named user exceptions
  and the deploy-key exception for release preparation.
- Master releases: PR, test, and package requirements, no bypass.

Do not add path filters to required PR checks: every PR must receive their check results.

Documentation and workflow-only changes run quick CI-policy and golden-tooling checks,
without building, packing, publishing, or running the expensive suite. Required check
jobs still report a result. Source, project, script, dependency, fixture, and build-input
changes run full verification; unrecognized files and incomplete diffs do too. Manual
runs always request full verification. A quick-check run is never reused as full-test
evidence and never creates a verified golden tag.

For same-repository `develop` → `master` PRs, `test` can reuse a successful push-triggered or manually dispatched
Test workflow for the exact head commit, provided `master` is its ancestor. This ensures
the proposed merge has the same source tree as the tested commit. If that run is still
pending, the check waits in five-minute intervals; absent, failed, or incompatible evidence
falls back to the full suite. Other PRs run the full suite. Release PR packing builds and
validates packages without duplicating the suite; master publishing retains full validation.

The comment workflow uses `github-actions[bot]` as the commit author and a
repository deploy key for pushes, which trigger normal push and PR CI.
The built-in token reads PR metadata and explicitly dispatches Test, package
verification, and PR title verification when retrying without a new version commit.
Manual package verification on develop cannot publish. The wrapper runs from
protected master. ShipIt runs in a separate read-only job; finalization applies
its patch on a fresh runner without executing tools or hooks from develop.
It checks the maintainer identity and current PR commits, and
only commits approved package project files and changelogs. The trusted policy supports the
six established packages and the incoming `Xantham.Generator.Myriad` package. Land this policy
on master before preparing the first Myriad release; the wrapper always executes master's
scripts against the release checkout. It rejects stale
versions, unexpected file changes, and branches that moved during preparation.
