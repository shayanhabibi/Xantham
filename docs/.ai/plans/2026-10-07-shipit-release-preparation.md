# ShipIt release preparation implementation plan

**Goal:** Maintainers prepare independent package releases with Partas.Build.EasyBuild.ShipIt; contributors use conventional PR titles without editing versions.

**Architecture:** Pin the extension and local tool, seed six package changelogs from the verified Cloudsmith release, and replace numeric bumping with local ShipIt stages. Preserve develop integration, merge commits for master releases, and master-only publishing. Check package changes and transitive project references against the release base before packing.

**Constraints:** AssemblyVersion stays 0.0.0.0. No automatic GitHub initialization or publishing credentials in contributor workflows. Full Git history is required by release validation. Existing quick checks and exact develop test reuse remain.

- [ ] Migrate the tool manifest and update compatible Partas.Build pins; expose local bump and release validation commands.
- [ ] Seed changelogs, including transitive source inputs and shared package metadata.
- [ ] Test release validation for changed dependencies, stale dependent versions, documentation-only changes, and prerelease ordering; wire validation into master package CI.
- [ ] Check conventional PR titles without executing contributor code; document contributor and maintainer steps.
- [ ] Restore tools, verify command behavior and an actual ShipIt dry run, run policy tests and actionlint, and open an integration PR to develop.
