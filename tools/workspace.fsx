/// Locates the installed toolchain when the checkout a script runs in is not the checkout the
/// dependencies were installed into.
///
/// Agents work in linked git worktrees under `.claude/worktrees/`. A worktree carries every
/// tracked file but none of the gitignored trees, so `node_modules/` is absent on a fresh one.
/// Rather than have each agent re-run `npm install` for a pin that is already on disk, the
/// scripts borrow the main checkout's install: the compiler is exported through
/// `XANTHAM_TSGO_EXE`, which `Tsc.locate` honours ahead of its parent-directory walk, and the
/// `typescript` package directory is handed to the generators as a path.
///
/// The other gitignored tree, `tools/tsc-ast/upstream/`, is deliberately *not* borrowed - it is
/// vendored per checkout by `generate-wire.fsx -- sync tsc-ast`, which hits the network. See
/// `.claude/rules/upstream.md`.
module Workspace

open System
open System.IO

[<Literal>]
let TscEnvVar = "XANTHAM_TSGO_EXE"

/// Turns the live suite's "no compiler, so skip" into a failure. The suite skips itself when
/// `Tsc.locate` comes back empty, which is right for a working copy with no `npm install` and
/// wrong anywhere a compiler is known to be present - a run that skipped everything is a green
/// build that tested nothing.
[<Literal>]
let RequireTscEnvVar = "XANTHAM_REQUIRE_TSC"

/// The `.git` entry of a linked worktree is a file holding `gitdir: <path>`; in the main
/// checkout it is a directory. Detection rides on that rather than on the worktree living
/// under `.claude/worktrees/`, so it survives a worktree parked anywhere.
let isLinkedWorktree (root: string) =
    File.Exists(Path.Combine(root, ".git"))

/// The checkout that owns the repository, resolved through the worktree's `commondir`:
/// `<main>/.git/worktrees/<name>/commondir` holds `../..`, and the main working tree is the
/// parent of the common git directory it points at.
let mainCheckout (root: string) : string option =
    let pointer = Path.Combine(root, ".git")

    if not (File.Exists pointer) then
        None
    else
        let gitDir =
            File.ReadAllText(pointer).Trim()
            |> fun text ->
                if text.StartsWith "gitdir:" then
                    Path.GetFullPath(Path.Combine(root, text.Substring(7).Trim()))
                else
                    ""

        let commonDir = Path.Combine(gitDir, "commondir")

        if gitDir = "" || not (File.Exists commonDir) then
            None
        else
            let common = Path.GetFullPath(Path.Combine(gitDir, File.ReadAllText(commonDir).Trim()))
            let checkout = Path.GetDirectoryName common

            if Directory.Exists checkout then Some checkout else None

/// Roots to search, nearest first: the checkout we are running in, then - only when that is a
/// linked worktree - the main checkout whose install we are entitled to borrow.
let searchRoots (root: string) =
    [ yield Path.GetFullPath root
      if isLinkedWorktree root then
          yield! (mainCheckout root |> Option.toList) ]

let private rid =
    let platform =
        if OperatingSystem.IsWindows() then "win32"
        elif OperatingSystem.IsMacOS() then "darwin"
        elif OperatingSystem.IsFreeBSD() then "freebsd"
        else "linux"

    let arch =
        match Runtime.InteropServices.RuntimeInformation.OSArchitecture with
        | Runtime.InteropServices.Architecture.Arm64 -> "arm64"
        | Runtime.InteropServices.Architecture.Arm -> "arm"
        | _ -> "x64"

    $"{platform}-{arch}"

/// Platform package and executable stem, most current layout first. Mirrors `Tsc.locate` in
/// src/Xantham.TypeScript.Wire/Library.fs - keep the two lists in step.
let private layouts = [ $"typescript-{rid}", "tsc"; $"native-preview-{rid}", "tsgo" ]

let private extension = if OperatingSystem.IsWindows() then ".exe" else ""

/// The native `tsc --api` server installed under a checkout, if one is.
let tscExeIn (root: string) =
    layouts
    |> List.tryPick (fun (package, stem) ->
        let path = Path.Combine(root, "node_modules", "@typescript", package, "lib", stem + extension)
        if File.Exists path then Some path else None)

/// The nearest checkout carrying an `npm install`. Node would find it anyway by walking parents
/// out of a worktree, since worktrees are nested under the repository - this makes the choice
/// explicit, and keeps a worktree resolving the same install the main checkout does.
let nodeModulesRoot (root: string) =
    searchRoots root
    |> List.tryFind (fun candidate -> Directory.Exists(Path.Combine(candidate, "node_modules")))
    |> Option.defaultValue (Path.GetFullPath root)

/// The `typescript` package the generators read the shipped schema out of. Falls back to the
/// path under `root` so a missing install still fails with the message it always did.
let typescriptPackage (root: string) =
    searchRoots root
    |> List.tryPick (fun candidate ->
        let path = Path.Combine(candidate, "node_modules", "typescript")
        if Directory.Exists path then Some path else None)
    |> Option.defaultValue (Path.GetFullPath(Path.Combine(root, "node_modules", "typescript")))

/// Exports `XANTHAM_TSGO_EXE` for this process and everything it launches, so the generators,
/// `dotnet test` and the live suite all drive one compiler. Idempotent: a value already in the
/// environment wins, because an agent or CI may have pinned one deliberately.
///
/// Only a worktree gets the redirect. The main checkout has its own install and `Tsc.locate`
/// finds it unaided; pinning the variable there would outlive the next bump of the pin.
///
/// Borrowing also sets `XANTHAM_REQUIRE_TSC`, because once a compiler is known to be on disk a
/// skipped live suite is a broken run rather than an unconfigured one. Export
/// `XANTHAM_REQUIRE_TSC=0` before the command to opt back out.
let ensureTsc (root: string) : string option =
    let requireTsc () =
        if String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable RequireTscEnvVar) then
            Environment.SetEnvironmentVariable(RequireTscEnvVar, "1")
            printfn $"worktree: %s{RequireTscEnvVar}=1 - live tests must run, not skip"

    match Environment.GetEnvironmentVariable TscEnvVar with
    | existing when not (String.IsNullOrWhiteSpace existing) && File.Exists existing ->
        requireTsc ()
        Some existing
    | _ when not (isLinkedWorktree root) -> None
    | _ ->
        match searchRoots root |> List.tryPick tscExeIn with
        | None -> None
        | Some exe ->
            Environment.SetEnvironmentVariable(TscEnvVar, exe)
            printfn $"worktree: borrowing %s{exe}"
            printfn $"worktree: %s{TscEnvVar} exported for this run"
            requireTsc ()
            Some exe
