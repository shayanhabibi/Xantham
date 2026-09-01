namespace Xantham.Generator

open System.IO
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// How faithfully a generated construct represents its TypeScript source, per
/// `docs/plans/generator-type-mapping.md` §1. Declaration order is severity order: a symbol's
/// tier is the worst tier among its findings, and structural comparison on this type is that
/// "worst".
type Tier =
    /// The F# type accepts and rejects exactly what TypeScript does.
    | Exact
    /// Meaning preserved, spelling made idiomatic - e.g. `null | undefined` hoisted to `option`.
    | Ergonomic
    /// Information TypeScript had was dropped - e.g. a union collapsed to `obj`.
    | Widened
    /// The construct is not represented; the consumer is on their own.
    | Escape

/// One thing a pass had to say about a symbol: a widening, a drop, or an ergonomic rewrite.
/// Findings are the raw material of the fidelity manifest - a silent drop is a bug by
/// definition, so every non-Exact emission produces one of these.
type Finding =
    { /// The pass that produced the finding. Stamped by `Pipeline.runTier`, so passes leave it
      /// empty and cannot misreport themselves.
      Pass: string
      /// The symbol concerned, qualified from the exported name down: `Options.onlyFirst`.
      Symbol: string
      Tier: Tier
      Message: string }

module Finding =
    /// A finding not yet stamped with its pass; the pipeline fold fills `Pass` in.
    let make tier symbol message =
        { Pass = ""; Symbol = symbol; Tier = tier; Message = message }

/// Per-package generator configuration, read from `xantham.json` next to the package manifest
/// when present (decision O4 in `docs/plans/generator-architecture.md`).
type GeneratorConfig =
    { /// Overrides the F# module name otherwise derived from the package name.
      ModuleName: string option }

    static member Default = { ModuleName = None }

module GeneratorConfig =
    let private jsonOptions =
        JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

    /// Loads `<packageDir>/xantham.json`, tolerating comments and trailing commas (the file is
    /// authored by hand). A missing file is the default configuration, not an error.
    let load (packageDir: string) : GeneratorConfig =
        let path = Path.Combine(packageDir, "xantham.json")

        if not (File.Exists path) then
            GeneratorConfig.Default
        else
            use doc = JsonDocument.Parse(File.ReadAllText path, jsonOptions)

            let field name =
                match doc.RootElement.TryGetProperty(name: string) with
                | true, v when v.ValueKind = JsonValueKind.String -> Some(v.GetString())
                | _ -> None

            { ModuleName = field "module" }

/// Everything a pass may reach for, created once per run by `Bootstrap.start`. Passes never
/// create programs; the session here is the only wire access they have.
type Context =
    { /// The bound snapshot and project over the batching mailbox. Pure passes never touch it,
      /// which is what lets their tests fabricate a `Context` without a live compiler.
      Session: Session<TscMailbox>
      Config: GeneratorConfig
      /// Absolute path of the package being generated from.
      PackageDir: string
      /// The `name` field of the package manifest, or the directory name without one.
      PackageName: string
      /// Absolute path of the declaration entry point the program was created over.
      EntryFile: string }

/// What a pass produced: the advanced model, or the model plus the findings that say where the
/// pass fell short of Exact.
type PassOutcome<'Model> =
    | Advanced of 'Model
    | Degraded of 'Model * Finding list

/// A nano-pass: one conceptual transformation over its tier's model, in the pipeline's uniform
/// async shape whether it talks to the compiler or not.
type Pass<'Model> =
    { Name: string
      Run: Context -> 'Model -> Async<PassOutcome<'Model>> }

module Pass =
    /// Lifts a pure rewrite into the pipeline's uniform async shape.
    let pure' name (f: Context -> 'M -> 'M) : Pass<'M> =
        { Name = name
          Run = fun ctx m -> async { return Advanced(f ctx m) } }

// ---------------------------------------------------------------------------------------------
// Tier 1 - Harvest: what the author exported. Wire-driven inventory, no mapping decisions.
// ---------------------------------------------------------------------------------------------

/// A deterministic source-order key parsed from a declaration node handle (`index.kind.path`).
/// The handle is otherwise opaque; only the file path and node index are read, and only for
/// ordering output the way the author ordered source.
type DeclOrder = { File: string; NodeIndex: int }

/// One export of the entry module, aliases already followed to their origin so re-exports
/// appear once under the name they are exported as.
type HarvestedExport =
    { /// The name the entry module exports it under - `"default"` for a default export.
      ExportName: string
      /// The origin symbol (`getAliasedSymbol` applied until stable).
      Symbol: SymbolResponse
      /// `getDocumentationComment`, already rendered to plain text by the wire.
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option }

type HarvestModel =
    { Exports: HarvestedExport list }

    static member Empty: HarvestModel = { Exports = [] }

// ---------------------------------------------------------------------------------------------
// Tier 2 - Resolve: what the checker says everything is. A type table keyed by TypeResponse.Id.
// ---------------------------------------------------------------------------------------------

/// A property or parameter, resolved: the symbol plus the derived facts every shape pass
/// would otherwise re-ask the wire for.
type ResolvedMember =
    { Symbol: SymbolResponse
      Docs: string
      Tags: JSDocTagInfo list
      Optional: bool
      ReadOnly: bool
      TypeId: int }

type ResolvedSignature =
    { Parameters: ResolvedMember list
      ReturnTypeId: int }

/// A `TypeResponse` plus the derived facts of the kinds the skeleton resolves: object members,
/// call signatures, union membership. Everything else stays on the raw response.
type TypeFacts =
    { Response: TypeResponse
      /// Name of the type's own symbol where it has one - only used to report legibly when the
      /// type is widened ("external type RegExp widened to obj").
      SymbolName: string option
      Members: ResolvedMember list
      CallSignatures: ResolvedSignature list
      UnionMembers: int list }

module TypeFacts =
    /// Facts before derivation: the response alone.
    let shallow (response: TypeResponse) =
        { Response = response
          SymbolName = None
          Members = []
          CallSignatures = []
          UnionMembers = [] }

/// The type ids an export resolves to. A symbol can be both a type and a value (a class), so
/// the two are separate fields rather than one.
type ExportTypeIds =
    { Declared: int option
      Value: int option }

type ResolveModel =
    { Harvest: HarvestModel
      /// Export symbol id -> the type ids the checker gave for it.
      ExportTypes: Map<int, ExportTypeIds>
      /// The type table. Closed: every id referenced by a `TypeFacts` is a key here or in
      /// `NotFollowed` - that closure is the tier's invariant.
      Types: Map<int, TypeFacts>
      /// Ids deliberately not resolved, with the reason (depth cutoff), so a reader of the
      /// table can tell "not followed" from "missing".
      NotFollowed: Map<int, string> }

// ---------------------------------------------------------------------------------------------
// Tier 3 - Shape: F#-shaped declarations. The minimal IR the walking skeleton renders.
// ---------------------------------------------------------------------------------------------

/// The F# type written at a reference position. Phase A covers primitives, `option`, and
/// references to declarations this run generates; everything else widens to `FsObj` with a
/// finding saying so.
type FsTypeRef =
    | FsBool
    | FsString
    | FsFloat
    | FsUnit
    | FsObj
    | FsOption of FsTypeRef
    | FsNamed of string

type FsMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      ReadOnly: bool
      Type: FsTypeRef }

type FsParam =
    { Name: string
      Optional: bool
      Type: FsTypeRef }

/// How a value export is bound to its JavaScript module.
type ImportBinding =
    | ImportDefault
    | ImportNamed of string

/// One member of the `Exports` erased type: a top-level exported function.
type FsExportMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Binding: ImportBinding
      Parameters: FsParam list
      Return: FsTypeRef }

type FsInterfaceDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      Members: FsMember list }

type FsDecl =
    | FsInterface of FsInterfaceDecl
    /// The one `Exports` type gathering the module's value exports.
    | FsExports of FsExportMember list

type ShapeModel =
    { Harvest: HarvestModel
      ExportTypes: Map<int, ExportTypeIds>
      Types: Map<int, TypeFacts>
      NotFollowed: Map<int, string>
      /// Export symbol id -> the F# type name this run generates for it. What lets a type
      /// reference to an exported alias come out as `FsNamed` rather than an expansion.
      DeclNames: Map<int, string>
      Decls: FsDecl list }

// ---------------------------------------------------------------------------------------------
// Tier 4 - Render: source text plus the fidelity manifest.
// ---------------------------------------------------------------------------------------------

type RenderModel =
    { ModuleName: string
      PackageName: string
      Decls: FsDecl list
      /// Every finding of every earlier tier, stamped with its pass.
      Findings: Finding list
      /// Rendered output: file name -> content. Written to disk by `Pipeline.run`, not here,
      /// so rendering stays pure.
      Files: (string * string) list }

type TierCounts =
    { Exact: int
      Ergonomic: int
      Widened: int
      Escape: int }

/// What a run reports back: where the fidelity manifest's numbers come from.
type RunReport =
    { ModuleName: string
      OutputFiles: string list
      Findings: Finding list
      Counts: TierCounts }
