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

    /// The compiler-lib names Fable.Core already binds, and the F# spelling of each.
    ///
    /// O7 left the compiler-lib group widening to `obj` "until the shipped compiler-lib
    /// package exists". For the ECMAScript half of `lib.d.ts` it already does, and every
    /// generated file opens it: `Fable.Core.JS` is that package. So `Promise<Response>` is
    /// `JS.Promise<obj>` rather than a bare `obj`, which is one honest loss (the DOM name
    /// inside) instead of two.
    ///
    /// Each entry is the F# name and the arity that name takes. The arity is here rather than
    /// inferred because it is the whole safety argument: TypeScript's own lib moves - it made
    /// `Uint8Array` generic in a buffer parameter that Fable's abbreviation does not have -
    /// and a mapping that guessed would emit code that does not compile. Arities that agree
    /// map exactly; a lib type carrying *more* arguments than Fable's binding maps with the
    /// extras dropped and a finding; one carrying fewer is not this type at all and widens.
    ///
    /// The DOM half (`HTMLElement`, `EventTarget`, `Blob`, ...) is not here: it is four hundred
    /// names, so it is generated rather than transcribed, and it lives in `BrowserBindings`
    /// below. What stays hand-written here is what has a hand-judged loss note attached.
    module LibBindings =
        /// Name, F# arity, and the loss to record - `None` when the mapping gives up nothing.
        let private table =
            [ "Promise", ("JS.Promise", 1, None)
              // A thenable is not a promise: TypeScript's `PromiseLike` is the structural
              // supertype, and reading one as `JS.Promise` claims methods it may not have.
              "PromiseLike", ("JS.Promise", 1, Some "PromiseLike reads as JS.Promise; a bare thenable is not one")
              "Map", ("JS.Map", 2, None)
              "ReadonlyMap", ("JS.Map", 2, Some "ReadonlyMap reads as JS.Map; the readonly restriction is not carried")
              "WeakMap", ("JS.WeakMap", 2, None)
              "Set", ("JS.Set", 1, None)
              "ReadonlySet", ("JS.Set", 1, Some "ReadonlySet reads as JS.Set; the readonly restriction is not carried")
              "WeakSet", ("JS.WeakSet", 1, None)
              "Date", ("JS.Date", 0, None)
              "Function", ("JS.Function", 0, None)
              "Object", ("JS.Object", 0, None)
              "Math", ("JS.Math", 0, None)
              "JSON", ("JS.JSON", 0, None)
              "Console", ("JS.Console", 0, None)
              "PropertyDescriptor", ("JS.PropertyDescriptor", 0, None)
              "ArrayBuffer", ("JS.ArrayBuffer", 0, None)
              "ArrayBufferView", ("JS.ArrayBufferView", 0, None)
              "DataView", ("JS.DataView", 0, None)
              "Int8Array", ("JS.Int8Array", 0, None)
              "Uint8Array", ("JS.Uint8Array", 0, None)
              "Uint8ClampedArray", ("JS.Uint8ClampedArray", 0, None)
              "Int16Array", ("JS.Int16Array", 0, None)
              "Uint16Array", ("JS.Uint16Array", 0, None)
              "Int32Array", ("JS.Int32Array", 0, None)
              "Uint32Array", ("JS.Uint32Array", 0, None)
              "Float32Array", ("JS.Float32Array", 0, None)
              "Float64Array", ("JS.Float64Array", 0, None)
              "BigInt64Array", ("JS.BigInt64Array", 0, None)
              "AsyncIterable", ("JS.AsyncIterable", 1, None)
              "AsyncIterator", ("JS.AsyncIterator", 1, None)
              "AsyncGenerator", ("JS.AsyncGenerator", 1, None)
              "IteratorResult", ("JS.IteratorResult", 1, None) ]
            |> Map.ofList

        /// The binding for a lib name, if Fable.Core has one: its F# name, its arity, and the
        /// loss to record. `seq`-shaped names (`Iterable`, `Iterator`) are absent on purpose -
        /// Fable.Core binds only the async ones, and pretending `seq<'T>` interoperates with a
        /// JS iterable is exactly the kind of claim this table exists not to make.
        let tryFind (name: string) = Map.tryFind name table

    /// The compiler-lib names the `Fable.Browser.*` family binds - the DOM half of the same
    /// disposition `LibBindings` covers for the ECMAScript half.
    ///
    /// The table itself is generated (`BrowserBindingTable`, from
    /// `tools/browser-gen/generate.fsx`), because it is four hundred names read off two
    /// authorities that both move: what the referenced `Browser.*` assemblies export, and what
    /// the pinned compiler's `lib.*.d.ts` files declare. Only their intersection is here, so an
    /// entry is by construction a real lib name with a real binding. The *rule* below is
    /// hand-written and tested, next to the one it mirrors.
    ///
    /// Arity is a lookup key rather than a property of the name, which is where this rule and
    /// `LibBindings` differ: `CustomEvent` is bound both bare and generic, so a reference
    /// carrying one type argument should reach the generic binding and a bare one the other.
    /// Failing an exact match, the widest binding the reference has arguments for wins and the
    /// extras are dropped with a finding - `LibBindings`' rule, generalised. A reference with
    /// fewer arguments than the narrowest binding is some other type wearing a familiar name,
    /// and widens.
    ///
    /// Unlike `LibBindings` no entry carries a loss note: a `Browser.Types` name is the same
    /// type under a different spelling, so the mapping is Exact. Whether Fable's binding has
    /// caught up with the member the caller wants is that package's business, exactly as it is
    /// for `JS.Promise`.
    module BrowserBindings =
        /// Every arity a lib name is bound at, widest first, so the first entry that fits is
        /// the widest that fits.
        let private byName =
            BrowserBindingTable.entries
            |> List.groupBy (fun (name, _, _) -> name)
            |> List.map (fun (name, bound) ->
                name,
                bound
                |> List.map (fun (_, arity, fsharpName) -> arity, fsharpName)
                |> List.sortByDescending fst)
            |> Map.ofList

        /// The binding for a lib name at a reference carrying `argumentCount` type arguments:
        /// its F# name and the arity that name takes.
        let tryFind (name: string) (argumentCount: int) : (string * int) option =
            byName
            |> Map.tryFind name
            |> Option.bind (List.tryFind (fun (arity, _) -> arity <= argumentCount))
            |> Option.map (fun (arity, fsharpName) -> fsharpName, arity)

    /// The JavaScript key a member symbol stands for. The checker escapes a name that begins
    /// with two underscores by prepending a third, so that a real `__html` cannot collide with
    /// the internal names it invents (`__type`, `__call`); undoing that is what turns the
    /// symbol back into the key the object actually carries. Apply it only *after* testing for
    /// an internal name - the escaping is the one thing that tells the two apart.
    let memberName (name: string) =
        if name.StartsWith "___" then name.Substring 1 else name

    /// Whether a name can head an F# declaration. Backticks rescue keywords and spaces, but not
    /// every symbol name is spellable even so: an ambient module declaration's symbol *is* its
    /// quoted specifier (`"cloudflare:email"`), and `` ``"cloudflare:email"`` `` is FS0883, not a
    /// type name. The rule is deliberately conservative - letters, digits and underscore, not
    /// starting with a digit - because the failure it prevents is a whole file that will not
    /// compile, and the cost of a false negative is one finding.
    let isWritableTypeName (name: string) =
        not (System.String.IsNullOrEmpty name)
        && (System.Char.IsLetter name[0] || name[0] = '_')
        && name |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')

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

/// Where a harvested name came from, which is what decides how a *value* binds in JavaScript.
/// Types are unaffected: an interface is the same F# declaration either way.
type ExportOrigin =
    /// A member of the entry file's module symbol - bound with `[<Import(name, package)>]`.
    | FromModule
    /// An ambient declaration in global scope (`declare class Response`). A global type library
    /// has no module to import from; the name is already on `globalThis`, so values bind with
    /// `[<Global>]` instead.
    | FromGlobal

/// One export of the entry module, aliases already followed to their origin so re-exports
/// appear once under the name they are exported as. A global type library has no module to
/// export from, and its ambient declarations arrive here too - see `ExportOrigin`.
type HarvestedExport =
    { /// The name the entry module exports it under - `"default"` for a default export, and
      /// the declared name for an ambient global, which is exported under nothing.
      ExportName: string
      /// The origin symbol (`getAliasedSymbol` applied until stable).
      Symbol: SymbolResponse
      /// `getDocumentationComment`, already rendered to plain text by the wire.
      Docs: string
      Tags: JSDocTagInfo list
      Origin: ExportOrigin
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

/// One index signature (`[key: string]: V`) as the resolve tier records it. These are
/// invisible to property enumeration - `getPropertiesOfType` returns nothing for a type whose
/// only content is an index signature - so a type can carry these and no members at all, and
/// the shape tier has to consult both before deciding a type has no shape worth declaring.
type ResolvedIndex =
    { KeyTypeId: int
      ValueTypeId: int
      IsReadonly: bool }

type ResolvedSignature =
    { Parameters: ResolvedMember list
      /// The signature's last parameter is a rest parameter (`...args`).
      HasRest: bool
      /// The signature's own type parameters (§4.9). A generic *function* carries them here
      /// rather than on its type, which is where a callback alias's `T` lives.
      TypeParameters: int list
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
      /// Index signatures (§4.10). Kept apart from `Members` because they are not properties:
      /// they have no name, and a type may carry one with no members at all.
      IndexInfos: ResolvedIndex list
      CallSignatures: ResolvedSignature list
      ConstructSignatures: ResolvedSignature list
      /// `extends` bases of an interface or class instance type, by id.
      BaseTypes: int list
      /// Type arguments of a generic reference, resolved for *every* group - an external
      /// `Array<T>` carries entry-package types that must still be reached (O7 note).
      TypeArguments: int list
      /// A tuple's per-element flags, in element order, copied off its *target* - the wire
      /// carries them there, not on the reference. The target itself is deliberately left out
      /// of the table: deriving it drags all of `Array.prototype` in again for every distinct
      /// tuple shape, and nothing but these flags is wanted from it.
      TupleElements: ElementFlags list
      /// The arguments the type's *alias* was written with, by id (§4.9). On the declaration
      /// form of a generic alias these are its own parameters - `type Mapper<T> = (t: T) => T`
      /// leaves the function type itself parameterless, so this is the only place `T` appears.
      AliasTypeArguments: int list
      /// The constituents of an intersection, in the checker's order. Separate from
      /// `UnionMembers` because the two mean opposite things and the passes that read one
      /// must never see the other.
      IntersectionMembers: int list
      /// A type parameter's `extends` bound, by id (§4.9). Only type parameters carry one.
      Constraint: int option
      /// A type parameter's default type argument, by id (§4.9).
      Default: int option
      UnionMembers: int list }

module TypeFacts =
    /// Facts before derivation: the response alone.
    let shallow (response: TypeResponse) =
        { Response = response
          Origin = Unclassified
          SymbolName = None
          Members = []
          IndexInfos = []
          CallSignatures = []
          ConstructSignatures = []
          BaseTypes = []
          TypeArguments = []
          TupleElements = []
          AliasTypeArguments = []
          IntersectionMembers = []
          Constraint = None
          Default = None
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
      /// Ids deliberately not resolved, with the reason - the depth cutoff, or a response the
      /// compiler could not encode - so a reader of the table can tell "not followed" from
      /// "missing".
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
    /// A fixed-length tuple (D7, §4.12): Fable compiles an F# tuple to a JS array, so the
    /// mapping is exact. Optional tail elements arrive already `option`-wrapped, because the
    /// checker hands them over as `T | undefined`.
    | FsTuple of FsTypeRef list
    /// A heterogeneous union as Fable's erased `U2`-`U4` (D4, §4.5(4)). The threshold is four;
    /// wider unions widen to `obj`. Arms are distinct - a union whose arms collapse to one F#
    /// type is that type instead.
    | FsErasedUnion of FsTypeRef list
    /// A callback as a delegate (D5): parameter types and return. Renders as
    /// `System.Action`/`System.Func` so the arity is guaranteed at the Fable boundary.
    | FsDelegate of FsTypeRef list * FsTypeRef
    /// A type variable in scope - a type parameter of the declaration being shaped (§4.9).
    /// Carries the name TypeScript spelled, without the leading tick the renderer adds.
    | FsTypeVar of string
    /// A generic declaration applied to arguments: `Box<string>` (§4.9). The checker
    /// substitutes members eagerly, so this is written only when the instantiation's target is
    /// itself a declaration this run generates; otherwise the expansion stands on its own.
    | FsApp of string * FsTypeRef list
    /// A primitive carrying a unit of measure: the F# rendering of a TypeScript branding
    /// intersection (§4.6, D11). `string & { __brand: "UserId" }` is a value that is a string
    /// at runtime and refuses to substitute for another string at compile time, which is what
    /// a measure is. The measure name is a declaration this run emits.
    | FsBranded of primitive: FsTypeRef * measure: string
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

/// A type parameter (§4.9), bound by a declaration or by a generic signature of its own. The
/// constraint is carried only when F# can express it - a subtype constraint against another
/// generated interface. TypeScript bounds that have no F# form (`extends string`, `extends
/// keyof T`) are dropped with a finding rather than approximated, because a wrong constraint
/// rejects correct code.
type FsTypeParam =
    { Name: string
      Constraint: FsTypeRef option }

type FsParam =
    { Name: string
      Optional: bool
      /// A rest parameter: rendered `[<ParamArray>]` on static emissions and abstract members
      /// alike, so Fable spreads the array at the call.
      Rest: bool
      Type: FsTypeRef }

type FsMethodMember =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      /// The method's *own* parameters, where it is generic independently of its declaration:
      /// `read<K extends keyof T>(key: K)` binds `K` here and reads `T` from the interface.
      TypeParameters: FsTypeParam list
      Parameters: FsParam list
      Return: FsTypeRef }

/// A TypeScript index signature rendered as F#: an `Item` member under `[<EmitIndexer>]`, so
/// `bag["key"]` is what reaches JavaScript rather than a `.Item(...)` call (§4.10). A readonly
/// signature drops the setter.
type FsIndexerMember =
    { Key: FsTypeRef
      Value: FsTypeRef
      ReadOnly: bool }

/// An interface member. Overloads are consecutive `FsMethod` entries sharing a name -
/// overloaded abstract members are legal F#.
type FsMember =
    | FsProperty of FsPropertyMember
    | FsMethod of FsMethodMember
    | FsIndexer of FsIndexerMember

/// How a value export is bound to its JavaScript module.
type ImportBinding =
    | ImportDefault
    | ImportNamed of string
    /// An ambient global (`declare function fetch`): there is no module to import from, so the
    /// name is taken off `globalThis` with `[<Global>]`.
    | GlobalName of string

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
      /// A top-level generic function binds its parameters on the member: `Exports` itself is
      /// not generic, so `get<T>(source: T)` has nowhere else to put `T`.
      TypeParameters: FsTypeParam list
      Binding: ImportBinding
      Body: FsExportBody }

type FsInterfaceDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      TypeParameters: FsTypeParam list
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

/// One field of a tagged-union case. The name is the JS property key verbatim: Fable emits the
/// field under its F# name, and backtick escaping is transparent there (`` ``type`` `` reaches
/// JS as `type`), so no separate compiled name is needed.
type FsTaggedField = { Name: string; Type: FsTypeRef }

/// One case of a `[<TypeScriptTaggedUnion>]` DU: the case name, the tag literal when it does
/// not spell the case name, and the arm's own properties as case fields.
///
/// The fields are the arm's properties *other than* the discriminant - Fable writes the tag
/// itself from the case's compiled name. Verified against Fable 5.13: `Circle(radius = 2.0)`
/// emits `{ kind: "circle", radius: 2 }`, and a `None` in an optional field omits the key
/// rather than writing `undefined`, which is exactly TypeScript's optional-property semantics.
/// Carrying the arm type as a single payload field instead does *not* work - Fable wraps it as
/// `{ kind: "circle", Item: x }`, an object no TypeScript signature would accept.
type FsTaggedCase =
    { Name: string
      CompiledName: string option
      Fields: FsTaggedField list }

/// A discriminated union the checker proved is tagged (D4, §4.5(2)): every member is an object
/// type carrying the same property, and that property's type is a distinct string literal in
/// each. Fable erases the DU to a plain object literal, so this is Exact *and* pattern-matchable
/// - by far the best consumer experience, which is why §4.5 says to detect it aggressively.
type FsTaggedUnionDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      /// The discriminant property's name, as TypeScript spells it.
      Tag: string
      Cases: FsTaggedCase list }

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
      /// The alias's own type parameters, in declaration order (§4.9). A generic alias binds
      /// them on its left side exactly as TypeScript does: `type Callback<'T> = Func<'T, obj>`.
      TypeParameters: FsTypeParam list
      Target: FsTypeRef }

/// A declaration TypeScript *computes* and F# cannot reproduce: a mapped type, a conditional or
/// a template literal at an operand the checker could not resolve (§4.10, §4.11). There is no
/// structure to emit - the structure is a function of an argument not yet supplied - so the
/// declaration is erased and keeps only its name and arity, which is enough for uses of it to
/// stay distinct from each other and from `obj`. Its single case is private, so the only way in
/// or out is a cast, which is exactly the guarantee the generator can honestly make.
type FsPhantomDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      TypeParameters: FsTypeParam list
      /// What the value is at runtime once erased: `string` for a template literal or an
      /// intrinsic string mapping, `obj` for everything else.
      Carrier: FsTypeRef }

/// A unit of measure standing for a branding intersection (§4.6, D11). It has no body: a
/// measure is a name and nothing else, and the brand it marks is written at the *uses*, as
/// `string<UserId>`, rather than as an abbreviation - the name can only be spent once, and a
/// measure is what spends it.
type FsMeasureDecl =
    { Name: string
      Docs: string
      Tags: JSDocTagInfo list
      Order: DeclOrder option
      /// The primitive the brand is over, kept for the manifest and the doc comment: a
      /// measure itself says nothing about what it annotates.
      Primitive: FsTypeRef }

type FsDecl =
    | FsInterface of FsInterfaceDecl
    | FsStringEnum of FsStringEnumDecl
    | FsPhantom of FsPhantomDecl
    | FsMeasure of FsMeasureDecl
    | FsTaggedUnion of FsTaggedUnionDecl
    | FsEnum of FsEnumDecl
    | FsAbbrev of FsAbbrevDecl
    /// The one `Exports` type gathering the module's value exports.
    | FsExports of FsExportMember list

/// How a `K extends keyof T` variable is written in F# (§4.10, the open keyof regime).
/// TypeScript's key variable has no F# counterpart of its own: a bare `'K` would be an
/// unconstrained variable saying nothing about T's keys, and every use of it - including the
/// `T[K]` it selects - would have to widen to obj. The support package carries the idiom
/// instead, so `'K` is not bound at all; its uses are written as one of these.
type KeyBinding =
    /// `keyof<'T>`: the key is only ever a key, so nothing needs the type it selects.
    | KeyOf of operand: string
    /// `typekeyof<'T,'R>` at the key's uses and `'R` at `T[K]`: the signature reads the value
    /// the key selects, so `'K` is replaced by the result variable that names it.
    | TypedKeyOf of operand: string * result: string

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
      /// Type id -> the type-parameter ids a declaration reads without binding, in first-use
      /// order (§4.9). An anonymous object type hoisted out of a generic scope - the `props`
      /// of `each<T, U>(props: { items: T[]; render: (item: T) => U })` - binds nothing of
      /// its own, so it is declared over these and every reference applies them back.
      DeclParams: Map<int, int list>
      /// `Exports` members accumulated by the class/function/value passes, keyed by harvest
      /// position so `order-declarations` can assemble them in source order.
      ExportMembers: (int * FsExportMember) list
      /// Type-parameter id -> the name it is in scope under, for the declaration currently
      /// being shaped. Scope lives on the model rather than in `typeRef`'s arguments because
      /// it is a property of *where* the reference is written, not of the reference: a pass
      /// binds it once around a declaration and every nested `typeRef` inherits it.
      TypeVars: Map<int, string>
      /// Type-parameter id -> the support-package idiom its uses are written as, for the
      /// signature currently being shaped (§4.10). Scoped like `TypeVars`, and for the same
      /// reason: `K extends keyof T` binds nothing outside the signature that declared it.
      KeyVars: Map<int, KeyBinding>
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

// ---------------------------------------------------------------------------------------------
// Placement: reading a symbol's declaration handle for where it came from. Used by two tiers -
// harvest, to pick the entry package's ambient globals out of a whole global scope, and resolve,
// to disposition a group (O7) - so it lives below both rather than inside either.
// ---------------------------------------------------------------------------------------------

module Grouping =

    /// Parses the ordering key out of a symbol's first declaration handle. A handle is
    /// `index.kind.path` where only the path may contain further dots.
    let declOrder (declarations: string[] voption) : DeclOrder option =
        match declarations with
        | ValueSome handles when handles.Length > 0 ->
            match handles[0].Split([| '.' |], 3) with
            | [| index; _kind; path |] ->
                match System.Int32.TryParse index with
                | true, index -> Some { File = path; NodeIndex = index }
                | _ -> None
            | _ -> None
        | _ -> None

    /// Classifies a symbol's origin group (O7) from its first declaration's file path: under the
    /// package directory is the entry package; the compiler's default libs are the compiler-lib
    /// group; under a `node_modules` entry is that dependency; anything else - including
    /// anonymous shapes with no declaration - is unclassified, which dispositions as the entry
    /// group.
    ///
    /// The default libs are recognised three ways because the compiler serves them three ways:
    /// from the platform package (`node_modules/@typescript/typescript-<rid>/lib/lib.*.d.ts` -
    /// what the live wire reports), from `typescript/lib`, or as `bundled:` pseudo-paths for the
    /// embedded copies. A non-entry `lib.*.d.ts` anywhere else still classifies as compiler lib
    /// rather than unclassified: unclassified means Ship, and full derivation of a mistaken
    /// standard-lib file is the expensive failure, while a mis-grouped oddball is a visible
    /// finding.
    let classify (packageDir: string) (symbol: SymbolResponse voption) : PackageId =
        match symbol |> ValueOption.bind (fun s -> declOrder s.Declarations |> ValueOption.ofOption) with
        // `typeof globalThis` (type-fest's `GlobalThis`) is the checker's own symbol for the
        // global scope: it declares nothing anywhere, so by path it would be unclassified and
        // shipped - as one interface carrying every global there is, a third of the file
        // for a type whose members nobody would call through it. The scope is the compiler's,
        // so it groups with the compiler lib and widens with a name, identity only.
        | ValueNone when symbol |> ValueOption.exists (fun s -> s.Name = "globalThis") -> CompilerLib
        | ValueNone -> Unclassified
        | ValueSome order ->
            let path = order.File.Replace('\\', '/')
            let root = packageDir.Replace('\\', '/').TrimEnd '/' + "/"
            let file = path.Substring(path.LastIndexOf '/' + 1)
            let isLibFile = file.StartsWith "lib." && file.EndsWith ".d.ts"

            if path.StartsWith(root, System.StringComparison.OrdinalIgnoreCase) then
                EntryPackage
            else
                match path.LastIndexOf "/node_modules/" with
                | -1 -> if isLibFile then CompilerLib else Unclassified
                | at ->
                    match path.Substring(at + "/node_modules/".Length).Split '/' with
                    | parts when parts.Length > 0 && (parts[0] = "typescript" || parts[0] = "@typescript") ->
                        CompilerLib
                    | _ when isLibFile -> CompilerLib
                    | parts when parts.Length > 1 && parts[0].StartsWith "@" -> Dependency $"{parts[0]}/{parts[1]}"
                    | parts when parts.Length > 0 -> Dependency parts[0]
                    | _ -> Unclassified
