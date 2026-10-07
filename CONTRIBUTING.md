# Contributing

Start feature branches from `develop` and open pull requests into `develop`.
Use a conventional PR title, such as `feat(generator): support a new mapping`,
`fix(wire): correct decoding`, or `docs: explain customization`.
Maintainers squash feature PRs using the PR title as the commit title. Direct
maintainer pushes to develop use conventional commit messages too.

Contributors do not bump package versions, generate release changelogs, or need
publishing credentials. Run `dotnet tool restore`, then use
`dotnet fsi build.fsx -- test` for normal validation. Generator changes also follow
the fixture and golden workflow described in the repository's agent notes.

`feat` and `perf` produce minor releases; `fix` produces patch releases. An `!`
after the type/scope or a `BREAKING CHANGE:` footer signals a breaking release.
Changes such as dependency upgrades or refactors still need a package release
when they change shipped artifacts, even if their commit type does not automatically
advance a version. Maintainers review those cases during release preparation.

## Preparing a release

Release preparation is a maintainer task, handled on the release PR:

1. Open a PR from `develop` into `master`.
2. Comment `/release preview` if you want to inspect the proposed versions first.
   The **Prepare release** workflow summary includes the result and its artifacts
   include the complete release diff.
3. Comment `/release`. The bot runs ShipIt, checks all changed packages and their
   transitive consumers have newer versions, then commits the versions and
   changelogs directly to develop and starts the required CI checks.
4. Review that diff and merge the PR with a **merge commit** once checks pass.
   The master push publishes verified packages to the temporary
   [Cloudsmith feed](https://app.cloudsmith.com/shayanhabibi/r/xantham).
   Verify the Publish workflow and feed versions, then merge master back into develop.

Only `shayanhabibi` and `houstonhaynes` can issue release commands. The workflow
also has a manual **Run workflow** entry: select **master**, then provide the PR
number and preview switch. The comment commands use master automatically.
There is no separate release-preparation branch or PR, and contributors need no
release commands or credentials. `/release` prepares packages; it never merges
the PR or publishes them. If CI dispatch fails after the push, rerun `/release`
to start the checks again without another version commit.

ShipIt calculates six independent package versions from conventional commits.
If validation reports a changed package that was not bumped (for example a
dependency upgrade with a `build` commit), add `force_version: 0.2.0` using the
intended version to that package's changelog front matter on develop, then rerun
the command. ShipIt consumes that override. Review breaking changes explicitly
while versions remain below 1.0. Avoid further source changes after preparing a
release: another preparation can advance versions again.

For local troubleshooting on develop, use `dotnet fsi build.fsx -- bump --dry-run`
to preview, `dotnet fsi build.fsx -- bump` to apply, and
`dotnet fsi build.fsx -- release check` to validate against `origin/master`.
These local commands do not push. The XML updater preserves
`<AssemblyVersion>0.0.0.0</AssemblyVersion>`.

The local tool is EasyBuild.ShipIt 3.1.0, requiring the repository's .NET 10 SDK.
The manifest lives in `.config/dotnet-tools.json`. `build.fsx -- shipit setup`
restores registered tools; `shipit version` and `shipit conventions` inspect them.
On a release-preparation branch, pass `--allow-branch <branch-name>` to preview
explicitly. The default allowed branch is develop, including for dry runs in 3.1.0.

Each `src/*/CHANGELOG.md` starts from the package version and commit verified in
the last Cloudsmith release. ShipIt owns the release history from this point onward.
Its XML updater changes only the project Version. Do not combine it with a separate
numeric version bump, or run upstream `init github`: that setup disables merge
commits used by our branch workflow. The comment workflow updates the existing
develop-to-master PR instead of creating another release PR.
Keep changelogs with LF line endings: ShipIt 3.1.0's front-matter parser does not
recognize CRLF. The repository's `.gitattributes` enforces LF on checkout.

See [.github/branch-workflow.md](.github/branch-workflow.md) for branch protections,
quick CI checks and reuse of develop's successful test run for master release PRs.
