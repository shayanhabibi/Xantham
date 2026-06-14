#nowarn FS1104
module Program

open System
open Xantham.TypeScript.Types.Symbol
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha


// ─────────────────────────────────────────────────────────────────────────────
// PROOFS
//
// Every `testCase` below is an executable *proof*: an invariant asserted over a
// corpus of real-world `.d.ts` packages (the fixture list at the bottom of this
// file). The wrappers in `Xantham.TypeScript` lean on these invariants to justify
// operations the F# type system cannot — the `failwith` guards in the `Source`
// model, `.Value` field access on package.json/DU payloads, and the `Ignore`
// fall-through in `XanTagKind.Create`. If a proof here fails, the wrapper it backs
// has an unsound path; the proof ID in the test name says which one.
//
// Proofs carry a stable ID so wrapper XML docs can cite them precisely
// (e.g. `<remarks>Totality proven by XTK-6 (Program.test.fs).</remarks>`). Two groups:
//
//   SF  · Source File Model — invariants the TypeScript compiler guarantees about
//                             source files, which the `Source` / `ExternalModule`
//                             constructors depend on.
//   XTK · Wrapper Totality  — invariants that our classifier wrappers are *total*
//                             over real input (never reach their failure/Ignore case).
//
// The full annotated catalog lives in `src/Xantham.TypeScript/README.md`.
// ─────────────────────────────────────────────────────────────────────────────

// The runner (defined in Spec.fs) provides an interface for creating test suites and tests,
// and feeds each passed fixture to the test function.
Spec.RunnerContext.make "Fable.TypeScript" <| fun suite runner ->
    SourceFile.tests runner
    SourceFile.wrapperTests runner
    NodeDeclaration.tests runner
    TypeChecker.tests runner
    ObjectFlags.tests runner
    TypeFlags.tests runner
    EnumMember.tests runner
    Members.memberSymbols runner
    Parameters.tests runner
    Discovery.edgeCases runner
    Identity.tests runner
    TypeReference.tests runner
    SymbolKind.tests runner
    Discovery.symbolLessTypes runner
    TypeWrapper.tests runner
    Discovery.symbolDiscovery runner
    NodeType.tests runner
    Discovery.modifierDiscovery runner
    Discovery.documentationDiscovery runner
    NodeKind.tests runner
    TypeKind.tests runner
    Identifiers.tests runner
    InlinedProgram.tests runner
    Tracers.tests runner
    TypeParameterWrapper.tests runner
    ClassWrapper.tests runner
    HeritageClauseWrapper.tests runner
    BindingElementWrapper.tests runner
    ParameterWrapper.tests runner
    UniqueESSymbolWrapper.tests runner
    KeyOfWrapper.tests runner
    