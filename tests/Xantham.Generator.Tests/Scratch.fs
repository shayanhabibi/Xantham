/// Scratch directories for tests, created inside the repository under the gitignored
/// `tests/.scratch`. A scratch package resolves the compiler and the root `node_modules` through
/// the same parent-directory walk as the fixtures under `tests/fixtures`.
module Xantham.Generator.Tests.Scratch

open System
open System.IO

/// `tests/.scratch` under the repository root.
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".scratch"))

/// A fresh, empty directory under `tests/.scratch`, deleted recursively on disposal.
[<Sealed>]
type ScratchDirectory(prefix: string) =
    let path = Path.Combine(root, prefix + "-" + Guid.NewGuid().ToString "N")
    do Directory.CreateDirectory path |> ignore

    member _.Path = path

    interface IDisposable with
        member _.Dispose() =
            if Directory.Exists path then
                Directory.Delete(path, true)

/// Creates a scratch directory named `<prefix>-<guid>`; bind it with `use`.
let directory (prefix: string) = new ScratchDirectory(prefix)
