/// Guards the thing that makes every other live test meaningful: that the compiler was found.
///
/// The live tests skip themselves when `Tsc.locate` comes back empty, which is right for a working
/// copy with no `npm install` and wrong for CI, where a run that skipped everything is a green
/// build that tested nothing. Setting `XANTHAM_REQUIRE_TSC` turns that skip into a failure; the
/// workflow sets it, so a broken `npm install` or a platform-package rename upstream shows up as a
/// red build rather than as silence.
module Xantham.TypeScript.Wire.Tests.Tsc

open System
open System.IO
open Expecto
open Xantham.TypeScript.Wire

let private required =
    match Environment.GetEnvironmentVariable "XANTHAM_REQUIRE_TSC" with
    | null | "" | "0" | "false" -> false
    | _ -> true

[<Tests>]
let tscTests =
    testList "tsc" [
        testCase "the compiler is present when the environment says it must be" <| fun _ ->
            match Tsc.locate __SOURCE_DIRECTORY__, required with
            | None, true ->
                failtest
                    "XANTHAM_REQUIRE_TSC is set and no tsc was found: `npm install` did not run, or the \
                     platform package layout `Tsc.locate` walks for has changed upstream"
            | _ -> ()

        // `Tsc.locate` builds the path from a runtime identifier and a table of package layouts,
        // and a typo in either would be invisible: it would fall through to `None` and skip
        // everything. This is the assertion that the path it built is the one npm actually wrote -
        // under a package named for *this* platform, since a wrong rid finds nothing rather than
        // finding the wrong binary.
        testCase "the located executable is this platform's package" <| fun _ ->
            match Tsc.locate __SOURCE_DIRECTORY__ with
            | None -> skiptest "run `npm install` in tests/Xantham.TypeScript.Wire.Tests"
            | Some _ when not (String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable "XANTHAM_TSGO_EXE")) ->
                // The override is deliberately unconstrained - it points wherever the caller says.
                skiptest "XANTHAM_TSGO_EXE is set, so the package layout says nothing"
            | Some exe ->
                Expect.isTrue (File.Exists exe) $"%s{exe} exists"

                let lib = Path.GetDirectoryName exe
                let package = Path.GetFileName(Path.GetDirectoryName lib)

                Expect.equal (Path.GetFileName lib) "lib" "the executable lives in the package's lib"

                // Both layouts `Tsc.locate` knows: `typescript-<rid>` for the shipped package and
                // `native-preview-<rid>` for the preview one it superseded.
                Expect.isTrue
                    (package.StartsWith("typescript-", StringComparison.Ordinal)
                     || package.StartsWith("native-preview-", StringComparison.Ordinal))
                    $"%s{package} is a platform package `Tsc.locate` knows"

                let platform =
                    if OperatingSystem.IsWindows() then "win32"
                    elif OperatingSystem.IsMacOS() then "darwin"
                    else "linux"

                Expect.stringContains package $"-%s{platform}-" $"%s{package} is named for this platform"
    ]
