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

/// The package boundary a symbol or type originates from, classified from its declaration's
/// file path (decision O7). Resolution depth and reference rendering are decided per group.
type PackageId =
    /// The package being generated.
    | EntryPackage
    /// The compiler's own `lib.*.d.ts` (bundled with the `typescript` npm package).
    | CompilerLib
    /// A dependency, by npm name (`@scope/name` kept whole).
    | Dependency of string
    /// No declaration path to classify by - anonymous and synthetic shapes. Treated as part
    /// of the entry package, which is what they are in practice.
    | Unclassified

/// What the generator does with one group's types (O7). `Map` and `Inline` are decided but
/// not yet built; they arrive with the reference-map machinery.
type GroupDisposition =
    /// Resolve fully and emit the group's declarations. Always the entry package's mode.
    | Ship
    /// Resolve identity only; references render as the group's templated module name, on the
    /// contract that a `ship` run of that group (ours or anyone's) produces those names.
    | Reference
    /// Resolve identity only; references widen to `obj` with a finding. The default for
    /// non-entry groups until the shipped compiler-lib package exists.
    | Widen

/// Per-package generator configuration, read from `xantham.json` next to the package manifest
/// when present (decision O4 in `docs/plans/generator-architecture.md`).
type GeneratorConfig =
    { /// Overrides the F# module name otherwise derived from the package name.
      ModuleName: string option
      /// Disposition per group, keyed as `xantham.json` spells them: npm name for a
      /// dependency, `typescript/lib` for the compiler lib.
      Groups: Map<string, GroupDisposition> }

    static member Default = { ModuleName = None; Groups = Map.empty }

module GeneratorConfig =
    let private jsonOptions =
        JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

    let private parseDisposition (key: string) =
        function
        | "ship" -> Ship
        | "reference" -> Reference
        | "widen" -> Widen
        | other -> failwith $"xantham.json: group {key} has unknown disposition '{other}' (ship|reference|widen)"

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

            let groups =
                match doc.RootElement.TryGetProperty "groups" with
                | true, v when v.ValueKind = JsonValueKind.Object ->
                    v.EnumerateObject()
                    |> Seq.map (fun p -> p.Name, parseDisposition p.Name (p.Value.GetString()))
                    |> Map.ofSeq
                | _ -> Map.empty

            { ModuleName = field "module"; Groups = groups }

    /// The key a group is addressed by under `xantham.json`'s `groups`; `None` for the groups
    /// that are not configurable (the entry package always ships).
    let groupKey =
        function
        | EntryPackage
        | Unclassified -> None
        | CompilerLib -> Some "typescript/lib"
        | Dependency name -> Some name

    /// A group's effective disposition: the entry always ships, everything else is `widen`
    /// unless configured (the default flips to `reference` once the shipped compiler-lib
    /// package exists - O7).
    let disposition (config: GeneratorConfig) (origin: PackageId) =
        match groupKey origin with
        | None -> Ship
        | Some key -> config.Groups |> Map.tryFind key |> Option.defaultValue Widen

/// The naming contract (O7): the deterministic scheme mapping package identities to F# module
/// names. Pinned, because a `reference` group's templated names must be exactly what a `ship`
/// run of that group produces - independently generated packages have to agree on every name
/// here. Renaming anything below is a breaking change to every shipped binding.
module Naming =
    let private capitalize (part: string) =
        string (System.Char.ToUpperInvariant part[0]) + part.Substring 1

    let private segments (text: string) =
        text.Split([| '-'; '_'; '.' |], System.StringSplitOptions.RemoveEmptyEntries)

    /// One path segment of a package name, PascalCased: `workers-types` -> `WorkersTypes`.
    let pascalSegment (text: string) =
        segments text |> Array.map capitalize |> String.concat ""

    /// A package's module name: `@scope/pkg-name` -> `Scope.PkgName`.
    let packageModule (packageName: string) =
        packageName.TrimStart('@').Split('/') |> Array.map pascalSegment |> String.concat "."

    /// The compiler-lib group's module.
    [<Literal>]
    let CompilerLibModule = "TypeScript.Lib"

    /// The module a group's declarations live in (or are templated to live in).
    let groupModule (entryPackageName: string) =
        function
        | EntryPackage
        | Unclassified -> packageModule entryPackageName
        | CompilerLib -> CompilerLibModule
        | Dependency name -> packageModule name

    /// The DU case name for a string-literal union member: PascalCased over separator
    /// segments (`"utf-8"` -> `Utf8`), prefixed when the result cannot start an F# case.
    /// Pinned like the module scheme - StringEnum case names are part of a binding's surface.
    let enumCaseOfString (text: string) =
        let cleaned =
            text
            |> Seq.map (fun c -> if System.Char.IsLetterOrDigit c then c else '-')
            |> System.String.Concat

        match pascalSegment cleaned with
        | "" -> "Empty"
        | name when System.Char.IsLetter name[0] -> name
        | name -> "N" + name

    /// The DU case name for a numeric-literal union member (D12): `1` -> `N1`,
    /// `1.5` -> `N1_5`, `-1` -> `NMinus1`.
    let enumCaseOfNumber (value: float) =
        let text =
            value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        "N" + text.Replace("-", "Minus").Replace(".", "_")

    /// The name a default export falls back to when its symbol is itself named `default`:
    /// the package name's last segment, camelCased (`ansi-regex` -> `ansiRegex`).
    let defaultExport (packageName: string) =
        let last = packageName.TrimStart('@').Split('/') |> Array.last

        segments last
        |> Array.mapi (fun i part ->
            if i = 0 then
                part.Substring(0, 1).ToLowerInvariant() + part.Substring 1
            else
                capitalize part)
        |> String.concat ""

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
      /// The signature's last parameter is a rest parameter (`...args`).
      HasRest: bool
      ReturnTypeId: int }

/// A `TypeResponse` plus the derived facts of the kinds the skeleton resolves: object members,
/// call signatures, union membership. Everything else stays on the raw response.
type TypeFacts =
    { Response: TypeResponse
      /// The group the type's own symbol is declared in (O7). Meaningful for object types;
      /// primitives and unions stay `Unclassified`, which dispositions as the entry group.
      Origin: PackageId
      /// Name of the type's own symbol where it has one - what a `reference` emission
      /// templates with, and what a widening finding names.
      SymbolName: string option
      Members: ResolvedMember list
      CallSignatures: ResolvedSignature list
      ConstructSignatures: ResolvedSignature list
      /// `extends` bases of an interface or class instance type, by id.
      BaseTypes: int list
      /// Type arguments of a generic reference, resolved for *every* group - an external
      /// `Array<T>` carries entry-package types that must still be reached (O7 note).
      TypeArguments: int list
      UnionMembers: int list }

module TypeFacts =
    /// Facts before derivation: the response alone.
    let shallow (response: TypeResponse) =
        { Response = response
          Origin = Unclassified
          SymbolName = None
          Members = []
          CallSignatures = []
          ConstructSignatures = []
          BaseTypes = []
          TypeArguments = []
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

/// The F# type written at a reference position. Phase B covers primitives, `option`, arrays,
/// delegates (D5) and references to declarations this run generates; everything else widens to
/// `FsObj` with a finding saying so.
type FsTypeRef =
    | FsBool
    | FsString
    | FsFloat
    | FsUnit
    | FsObj
    | FsOption of FsTypeRef
    | FsArray of FsTypeRef
    /// A callback as a delegate (D5): parameter types and return. Renders as
    /// `System.Action`/`System.Func` so the arity is guaranteed at the Fable boundary.
    | FsDelegate of FsTypeRef list * FsTypeRef
    | FsNamed of string

/// A literal payload carried by a StringEnum case (D12: mixed literal unions keep their
/// non-string members as `[<CompiledValue>]` cases).
type FsLiteral =
    | LitString of string
    | LitNumber of float
    | LitBool of bool

type FsPropertyMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      ReadOnly: bool
      Type: FsTypeRef }

type FsParam =
    { Name: string
      Optional: bool
      /// A rest parameter; static emissions render `[<ParamArray>]`, abstract members read as
      /// a plain array (attribute syntax is not available there).
      Rest: bool
      Type: FsTypeRef }

type FsMethodMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Parameters: FsParam list
      Return: FsTypeRef }

/// An interface member. Overloads are consecutive `FsMethod` entries sharing a name -
/// overloaded abstract members are legal F#.
type FsMember =
    | FsProperty of FsPropertyMember
    | FsMethod of FsMethodMember

/// How a value export is bound to its JavaScript module.
type ImportBinding =
    | ImportDefault
    | ImportNamed of string

/// What one member of the `Exports` erased type is.
type FsExportBody =
    /// A top-level exported function; overloads are consecutive members sharing a name.
    | ExportFunction of FsParam list * FsTypeRef
    /// An exported value (`const`/`let`, or a namespace object): a get-only property.
    | ExportValue of FsTypeRef
    /// A class constructor: `[<EmitConstructor>]`, so `Exports.Name(...)` is `new Name(...)`.
    | ExportConstructor of FsParam list * FsTypeRef

/// One member of the `Exports` erased type.
type FsExportMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Binding: ImportBinding
      Body: FsExportBody }

type FsInterfaceDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      /// Base interfaces (`extends`, or a class base) - rendered as `inherit` lines.
      Inherits: FsTypeRef list
      Members: FsMember list
      /// `[<ParamObject; Emit("$0")>]` Create overloads for plain-data interfaces (D3) -
      /// parameter lists mirroring the members, so consumers never hand-build objects.
      CreateOverloads: FsParam list list }

/// One case of a `[<StringEnum>]` DU. `CompiledName` carries the literal when it differs from
/// the case name; `CompiledValue` carries a non-string literal (D12).
type FsUnionCase =
    { Name: string
      CompiledName: string option
      CompiledValue: FsLiteral option }

type FsStringEnumDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      Cases: FsUnionCase list }

/// A numeric TS enum as an F# enum - `type E = A = 1` (§4.7).
type FsEnumDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      Cases: (string * int) list }

/// A type abbreviation: an exported alias whose right side is a reference, not a shape of its
/// own (callback aliases to delegates, alias-of-alias, primitive aliases).
type FsAbbrevDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      Target: FsTypeRef }

type FsDecl =
    | FsInterface of FsInterfaceDecl
    | FsStringEnum of FsStringEnumDecl
    | FsEnum of FsEnumDecl
    | FsAbbrev of FsAbbrevDecl
    /// The one `Exports` type gathering the module's value exports.
    | FsExports of FsExportMember list

type ShapeModel =
    { Harvest: HarvestModel
      ExportTypes: Map<int, ExportTypeIds>
      Types: Map<int, TypeFacts>
      NotFollowed: Map<int, string>
      /// Type id -> the F# type name this run declares for it - exports named first, then
      /// synthesized names for reachable anonymous shapes (hash-consing by id, §4.4). What
      /// lets a reference come out as `FsNamed` rather than an expansion.
      DeclNames: Map<int, string>
      /// Type id -> the source order its declaration sorts under: the export's own order, or
      /// for a synthesized declaration the order of the export that first reached it.
      DeclOrders: Map<int, DeclOrder option>
      /// `Exports` members accumulated by the class/function/value passes, keyed by harvest
      /// position so `order-declarations` can assemble them in source order.
      ExportMembers: (int * FsExportMember) list
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
