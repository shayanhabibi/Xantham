/// Resolution of the npm-installed fixture packages, shared by the suites that generate from
/// them. A linked worktree resolves through the main checkout's install, the way
/// `tools/workspace.fsx` does for the compiler.
module Xantham.Generator.Tests.Fixtures

open System.IO

/// The main working tree of a linked worktree: the worktree's `.git` is a file holding
/// `gitdir:`, and `<gitdir>/commondir` points at the common git directory whose parent is the
/// main checkout. `None` where `root` is itself a main checkout.
let mainCheckout (root: string) : string option =
    let pointer = Path.Combine(root, ".git")

    if not (File.Exists pointer) then
        None
    else
        let text = File.ReadAllText(pointer).Trim()

        if not (text.StartsWith "gitdir:") then
            None
        else
            let gitDir = Path.GetFullPath(Path.Combine(root, text.Substring(7).Trim()))
            let commonDir = Path.Combine(gitDir, "commondir")

            if not (File.Exists commonDir) then
                None
            else
                let common =
                    Path.GetFullPath(Path.Combine(gitDir, File.ReadAllText(commonDir).Trim()))

                let checkout = Path.GetDirectoryName common
                if Directory.Exists checkout then Some checkout else None

/// An npm-installed fixture package at `tests/fixtures/<name>/node_modules/<name>`, taken from
/// this checkout's install or the main checkout's.
let npm (root: string) (name: string) : string option =
    [ root; yield! mainCheckout root |> Option.toList ]
    |> List.map (fun checkout -> Path.Combine(checkout, "tests", "fixtures", name, "node_modules", name))
    |> List.tryFind Directory.Exists
