---
paths:
  - "build.fsx"
  - "tools/*.fsx"
---

# Build Pipeline

`build.fsx` is the repository pipeline; `tools/*.fsx` are the finer-grained scripts it routes to.
Both are run as `dotnet fsi <script> -- <command>`.

## Core Libraries

- **Partas.Build** — the `rootCommand`/`command`/`stage`/`input` DSL, plus the `Baked.*` prefabs
  for common inputs and pipelines.
- **Partas.TypeProvider.BuildHelper** — the `Repo` provider: `Repo.Project.*` for project and
  solution paths, `Repo.FileSystem.*` for directories.

## DSL Shape

A stage that reads options is an `input` computation returning a stage. Bind every option with
`let!`/`and!` first, then `return stage "name" { ... }`:

```fsharp
let build = input {
    let! projects = Options.projects
    and! config = Options.config
    return stage "build" {
        quiet
        when' (not (List.isEmpty projects))
        run (cmd $"dotnet build {projects[0]} -c {config} -v q")
    }
}
```

- Conditional execution is `when'`, not a surrounding `if`. `when'` skips a stage at run time; an
  `if` changes which stages exist at all. Use `if` only to pick between shapes (watch vs. build).
- Options are declared with `Input.option<'T>`, given defaults with `Input.def`, and made bindable
  with `InputSpec.ofInput`; reshape them with `InputSpec.map` rather than at the use site.
- `Repo.FileSystem` is a *live* view of the tree — a member only exists if that directory exists
  when the script compiles. Never reach through it into `node_modules`, `bin`, or other generated
  paths; build those as strings off the repository root, or the script stops compiling on a clean
  checkout.

## Worktrees borrow the main checkout's install

`tools/workspace.fsx` is a `#load`-only helper (not a command script) that both `build.fsx` and
`tools/generate-wire.fsx` use to answer "which checkout has the dependencies?".

An agent worktree under `.claude/worktrees/` carries tracked files only, so it has no
`node_modules`. Rather than install the pin twice, `Workspace.ensureTsc` finds the main
checkout's compiler and exports it as `XANTHAM_TSGO_EXE`, which `Tsc.locate` honours ahead of its
parent-directory walk; `npm install` is then skipped and the live tests run against the same
binary the main checkout uses. `typescriptPackage` and `nodeModulesRoot` resolve the generators'
inputs the same way — worktree first, then the main checkout.

- Detection is "`.git` is a file, not a directory", so it does not depend on where the worktree
  sits. The main checkout is found through the worktree's `commondir`.
- Only worktrees get the redirect. In the main checkout `ensureTsc` returns `None` and changes
  nothing, so a pin bump is picked up normally.
- An `XANTHAM_TSGO_EXE` already in the environment always wins.
- Generated output still goes to the worktree — only *inputs* are borrowed.
- Consequence: with the override set, the `tsc` layout test skips itself by design. CI installs
  normally and sets `XANTHAM_REQUIRE_TSC`, so that assertion still runs there.

## General Patterns

- ** DO ** add common repository level commands/tasks to `build.fsx`

Cleaning artifacts, building documentation, building projects, bumping versions, packing to a root folder.
Implemented via stages, and composed into commands/pipelines.
For instance, a common task such as `build` can be composed of stages such as `restore` -> `clean` -> `build`.
A `--quick` flag can be used to skip `restore` and `clean` which is very relevant to most usage (but should not be default).

- ** DO NOT ** bloat the rootCommand with overlapping commands or actions

`build-wire`, `build-tests`, `build-docs` is bad design. Group related actions into commands, and
use arguments, options or sub commands for separation if required. When not provided arguments, the command should
default to the most general and safe defaults.

- ** DO NOT ** make tasks operate implicitly on launch.

Simply launching the script should not perform any actions. Instead, commands should be used to trigger specific actions.

- ** DO ** create localised scripts for finer grain tasks.

See ./tools/generate-wire.fsx
These scripts may require more input, provide less defaults, and be more specific.
We can route commands to these scripts. But the `build.fsx` command should have *default* behaviour
for the repository (ie, we should provide the output directory by default, as we almost always target the same folder).

- ** DO NOT ** make any input required.
- ** DO ** give every option a default that is correct for this repository, so a bare command works.
- ** DO ** use arguments/options only to customise that default behaviour.
- ** DO ** make tasks compose other tasks via stages/pipelines.
