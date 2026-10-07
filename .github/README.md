# Branch and release workflow

Feature and dependency pull requests target `develop`. Release pull requests normally
merge `develop` into `master` using a merge commit, preserving shared branch history.

Both branches block deletion and force pushes, including for maintainers. Pull requests
require successful GitHub Actions checks, an up-to-date branch, and resolved review
conversations. No additional approving reviewer is required. Auto-merge is available.

`develop` requires the `test` check. `shayanhabibi` and `houstonhaynes` can bypass its
pull-request and check requirements for direct pushes; push-triggered tests still run.
Other contributors use pull requests.

`master` requires `test` and `package`, with no bypass actors. The Publish workflow
packs and validates every pull request to `master` without publishing credentials.
Only the subsequent `master` push (or manual dispatch on `master`) can publish the
verified package artifact to the temporary Cloudsmith feed. Documentation deployment
and golden tagging remain restricted to `master`.

The protection rules are configured in GitHub under Settings → Rules → Rulesets:

- Branch history: force-push and deletion protection for both branches, no bypass.
- Develop integration: PR and test requirements, with the two named user exceptions.
- Master releases: PR, test, and package requirements, no bypass.

Do not add path filters to required PR checks: every PR must receive their check results.
