namespace Xantham.Generator

open System
open System.ComponentModel.DataAnnotations
open System.IO
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open System.ComponentModel
open Measure


/// The package boundary a symbol or type originates from, classified from its declaration's
/// file path (decision O7). Resolution depth and reference rendering are decided per group.
type PackageId =
    /// The package being generated.
    | EntryPackage
    /// The compiler's own `lib.*.d.ts` (bundled with the `typescript` npm package).
    | CompilerLib
    /// A dependency, by npm name (`@scope/name` kept whole).
    | Dependency of string<npmDependency>
    /// No declaration path to classify by - anonymous and synthetic shapes. Treated as part
    /// of the entry package, which is what they are in practice.
    | Unclassified
    static member inline StringDependency (dep: string) = dep * uom<npmDependency> |> Dependency

/// The F# destination of one mapped name (O7's `map`).
[<Description("The destination of each redirected name, keyed by the TypeScript name the group declares. A name outside the table widens.")>]
type MappedName =
    {
        /// The name written at a reference position, qualified as the destination package
        /// spells it: `Node.Buffer.Buffer`, `System.Text.RegularExpressions.Regex`.
        [<Required>]
        [<Description("The F# name a reference renders as, qualified as the destination package spells it.")>]
        FSharpName: string
        /// The number of type arguments the destination takes. A reference applying any other
        /// number widens with a finding (`TR053`).
        [<Description("The number of type arguments the destination takes. A reference applying any other number widens with finding TR053.")>]
        [<Range(0, Int32.MaxValue)>]
        Arity: int
    }

/// What the generator does with one group's types (O7). `Inline` is decided but not yet
/// built; it arrives with demand-driven resolution.
type GroupDisposition =
    /// Resolve fully and emit the group's declarations. Always the entry package's mode.
    | Ship
    /// Resolve identity only; references render as the group's templated module name, on the
    /// contract that a `ship` run of that group (ours or anyone's) produces those names.
    | Reference
    /// Resolve identity only; a name the table carries renders as its destination - a binding
    /// somebody already wrote by hand - and every other name of the group widens.
    | Map of names: Map<string, MappedName>
    /// Resolve identity only; references widen to `obj` with a finding. The default for
    /// non-entry groups until the shipped compiler-lib package exists.
    | Widen

/// The optional spelling controls for the combined compiler-library binding. Names remain
/// optional here so the JSONC reader can distinguish an omitted field from an override; the
/// layout below owns the effective values consumers use.
type CompilerLibConfig =
    {
        [<Description("The root F# module for the generated compiler-library binding. It may be dotted.")>]
        ModuleName: string option
        [<Description("The single F# identifier used for the ECMAScript child module.")>]
        EsModuleName: string option
        [<Description("The single F# identifier used for the DOM child module.")>]
        DomModuleName: string option
        [<Description("Open the ECMAScript child module from the compiler-library root. Defaults to false.")>]
        AutoOpenEs: bool
        [<Description("Open the DOM child module from the compiler-library root. Defaults to false.")>]
        AutoOpenDom: bool
    }

    static member Default =
        {
            ModuleName = None
            EsModuleName = None
            DomModuleName = None
            AutoOpenEs = false
            AutoOpenDom = false
        }

/// The one identifier validator shared by names that become F# declaration or module segments.
module Identifier =
    let private shaped =
        System.Text.RegularExpressions.Regex @"^[A-Za-z_][A-Za-z0-9_']*$"

    let isPlain (name: string) = shaped.IsMatch name

/// Effective module layout for the compiler's standard-library binding.
type CompilerLibLayout =
    {
        RootModule: string
        EsModule: string
        DomModule: string
        AutoOpenEs: bool
        AutoOpenDom: bool
        EsQualifiedModule: string
        DomQualifiedModule: string
    }

module CompilerLibLayout =
    [<Literal>]
    let private defaultRootModule = "TypeScript.Lib"

    [<Literal>]
    let private defaultEsModule = "Es"

    [<Literal>]
    let private defaultDomModule = "Dom"

    let private invalid field expectation =
        failwith $"xantham.json: compilerLib.{field} {expectation}"

    let private validateRoot (name: string) =
        if String.IsNullOrWhiteSpace name then
            invalid "module" "must be a nonempty dotted F# module name"

        name.Split('.')
        |> Array.iter (fun segment ->
            if not (Identifier.isPlain segment) then
                invalid "module" "must be a nonempty dotted F# module name")

    let private validateChild field (name: string) =
        if String.IsNullOrWhiteSpace name || not (Identifier.isPlain name) then
            invalid field "must be one nonempty F# identifier"

    /// Applies defaults and verifies the names can become the compiler-library's nested modules.
    let create (config: CompilerLibConfig) =
        let root = config.ModuleName |> Option.defaultValue defaultRootModule
        let es = config.EsModuleName |> Option.defaultValue defaultEsModule
        let dom = config.DomModuleName |> Option.defaultValue defaultDomModule

        validateRoot root
        validateChild "esModule" es
        validateChild "domModule" dom

        if es = dom then
            invalid "esModule" "must differ from compilerLib.domModule"

        {
            RootModule = root
            EsModule = es
            DomModule = dom
            AutoOpenEs = config.AutoOpenEs
            AutoOpenDom = config.AutoOpenDom
            EsQualifiedModule = $"{root}.{es}"
            DomQualifiedModule = $"{root}.{dom}"
        }

/// Per-package generator configuration, read from `xantham.json` next to the package manifest
/// when present (decision O4 in `docs/plans/generator-architecture.md`).
type GeneratorConfig =
    {
        /// Overrides the F# module name otherwise derived from the package name.
        [<Description(" \
        The F# module the binding is written into. Defaults to the package name under the O7 naming contract, where \
        `@scope/pkg-name` becomes `Scope.PkgName`.")>]
        ModuleName: string option
        /// The F# namespace a package family is written under. The entry package takes the
        /// namespace itself, and each group named under `groups` takes `<Namespace>.<Leaf>`:
        /// `FSharp.CloudEdge` names `@cloudedge/agents` as `FSharp.CloudEdge.Agents`. A
        /// dependency the configuration leaves unnamed keeps its derived module.
        ///
        /// Both sides of a `reference` configure the same namespace. The referencing run
        /// templates a name the referenced run has to produce, and `GE004` records each group
        /// named this way.
        [<Description("
        The F# namespace a package family is written under. The entry package takes it, and each group \
        named under `groups` takes `<namespace>.<Leaf>`, so `@cloudedge/agents` under `FSharp.CloudEdge` reads \
        `FSharp.CloudEdge.Agents`. Both sides of a reference configure the same namespace.")>]
        Namespace: string option
        /// Disposition per group, keyed as `xantham.json` spells them: npm name for a
        /// dependency, `typescript/lib` for the compiler lib.
        [<Description("What the generator does with each package boundary its declarations reach (decision O7), keyed \
        by npm name, with the compiler's own library as `typescript/lib`. An unlisted group widens.")>]
        Groups: Map<string<npmDependency>, GroupDisposition>
        /// The compiler's `lib` option, as `tsconfig.json` spells it (`["esnext"]`). `None` is the
        /// compiler's default, which includes the DOM. A global type library that redeclares DOM
        /// names (`@cloudflare/workers-types`) must set this to what its README prescribes: with
        /// the DOM loaded, every such name merges with the lib's declaration, is grouped as the
        /// compiler lib by its first declaration, and is not the package's to harvest.
        [<Description("The compiler's `lib` option, as `tsconfig.json` spells it. Omitted, the \
        compiler's default applies, which includes the DOM. A global type library that \
        redeclares DOM names sets this to what its README prescribes.")>]
        Lib: string list option
        /// Explicit ambient type packages, using the compiler's `types` option. `None` keeps
        /// automatic discovery; `Some []` disables it. Required packages must be installed.
        [<Description("The compiler's `types` option: installed ambient type packages required by this input. \
        Omitted, automatic discovery applies; an empty array disables automatic inclusion. \
        Missing named packages are reported before generation.")>]
        Types: string list option
        /// Emits declarations.json with the identity and final F# name of reusable declarations.
        [<Description("Emit declarations.json with stable TypeScript declaration identities and final F# names. Defaults to false.")>]
        DeclarationCatalog: bool
        /// Producer catalogs, absolute or relative to the input package directory.
        [<Description("Producer declarations.json files, absolute or relative to the input package directory. \
        Matching types reuse their producer's F# identity; incompatible catalogs fail generation.")>]
        DeclarationReferences: string list
        /// The TypeScript input file, relative to the package directory. `None` selects the
        /// manifest's root declaration entry. Set `RuntimePackage` separately for a public subpath.
        [<Description("The TypeScript input file, relative to the package directory passed to \
        generate, even when the configuration lives elsewhere. Must \
        name an existing .ts, .tsx, .mts or .cts file (including declarations) \
        within that directory. Omitted, selects types, typings, \
        a root-export types string, then index.d.ts. An exports map without a root requires an explicit entry. \
        Set runtime separately for a public JavaScript subpath; each invocation generates from one entry.")>]
        Entry: string option
        /// Overrides the npm package the generated `[<Import(…)>]` attributes name. `None`
        /// derives it from the package name (`GeneratorConfig.runtimePackage`), which is right
        /// for every package that ships its own JavaScript and for the DefinitelyTyped naming
        /// convention. It is the escape hatch for the packages the convention cannot describe:
        /// a `@types/*` package whose runtime is named something else entirely, and a
        /// types-only package published outside DefinitelyTyped.
        [<Description("The public JavaScript module or subpath used by generated `[<Import(…)>]` attributes. \
        Must be a nonempty string when provided. Independent of the entry declaration path. \
        Defaults to the package name with DefinitelyTyped's `@types/` convention undone, so \
        `@types/three` imports from `three`.")>]
        RuntimePackage: string option
        /// Resolves `NoInfer<T>` (§4.11's carve-out) to `T` at the mapping site, dropping the
        /// name. `false` emits `NoInfer<T>`, which resolves through the support package's own
        /// abbreviation and keeps the generated file showing what TypeScript declared.
        [<Description("Resolves TypeScript's `NoInfer<T>` (§4.11) to `T` at the mapping site, dropping the name. \
        Defaults to false, which emits `NoInfer<T>` and reaches the support package's own abbreviation.")>]
        ResolveNoInfer: bool
        /// Names and opening policy for the compiler's combined library binding.
        CompilerLib: CompilerLibConfig
    }

    static member Default =
        {
            ModuleName = None
            Namespace = None
            Groups = Map.empty
            Lib = None
            Types = None
            DeclarationCatalog = false
            DeclarationReferences = []
            Entry = None
            RuntimePackage = None
            ResolveNoInfer = false
            CompilerLib = CompilerLibConfig.Default
        }

module GeneratorConfig =
    let private jsonOptions =
        JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

    /// One entry of a mapped group's table: `"Buffer": "Node.Buffer.Buffer"` for a
    /// destination that takes no type arguments, `"Readable": { "name": "Node.Stream.Readable",
    /// "arity": 1 }` for one that does.
    let private parseMappedName (key: string) (name: string) (value: JsonElement) =
        match value.ValueKind with
        | JsonValueKind.String ->
            {
                FSharpName = value.GetString()
                Arity = 0
            }
        | JsonValueKind.Object ->
            let fsharpName =
                match value.TryGetProperty "name" with
                | true, v when v.ValueKind = JsonValueKind.String -> v.GetString()
                | _ -> failwith $"xantham.json: group {key} maps {name} to an object without a name"

            let arity =
                match value.TryGetProperty "arity" with
                | true, v when v.ValueKind = JsonValueKind.Number -> v.GetInt32()
                | true, _ -> failwith $"xantham.json: group {key} maps {name} at a non-numeric arity"
                | _ -> 0

            {
                FSharpName = fsharpName
                Arity = arity
            }
        | _ -> failwith $"xantham.json: group {key} maps {name} to neither a name nor a name-and-arity object"

    /// One group's disposition. A string is the disposition itself; an object carries the
    /// destination table a mapped group needs, keyed by TypeScript name.
    let private parseGroup (key: string) (value: JsonElement) =
        match value.ValueKind with
        | JsonValueKind.String ->
            match value.GetString() with
            | "ship" -> Ship
            | "reference" -> Reference
            | "widen" -> Widen
            | "map" -> failwith $"xantham.json: group {key} is mapped, so it needs a table: {{ \"map\": {{ ... }} }}"
            | other ->
                failwith $"xantham.json: group {key} has unknown disposition '{other}' (ship|reference|widen|map)"
        | JsonValueKind.Object ->
            match value.TryGetProperty "map" with
            | true, table when table.ValueKind = JsonValueKind.Object ->
                table.EnumerateObject()
                |> Seq.map (fun p -> p.Name, parseMappedName key p.Name p.Value)
                |> Map.ofSeq
                |> GroupDisposition.Map
            | _ -> failwith $"xantham.json: group {key} is an object, so its one key is \"map\""
        | _ -> failwith $"xantham.json: group {key} is neither a disposition nor a mapped group"

    /// Loads a configuration file, tolerating comments and trailing commas (the file is authored
    /// by hand). A missing file is the default configuration, not an error.
    let loadFile (path: string) : GeneratorConfig =
        if not (File.Exists path) then
            GeneratorConfig.Default
        else
            use doc = JsonDocument.Parse(File.ReadAllText path, jsonOptions)

            let field name =
                match doc.RootElement.TryGetProperty(name: string) with
                | true, v when v.ValueKind = JsonValueKind.String -> Some(v.GetString())
                | _ -> None

            let boolField name defaultValue =
                match doc.RootElement.TryGetProperty(name: string) with
                | true, v when v.ValueKind = JsonValueKind.True -> true
                | true, v when v.ValueKind = JsonValueKind.False -> false
                | true, _ -> failwith $"xantham.json: {name} must be a boolean"
                | _ -> defaultValue

            let entry =
                match doc.RootElement.TryGetProperty "entry" with
                | true, value when value.ValueKind = JsonValueKind.String -> Some(value.GetString())
                | true, _ -> failwith "xantham.json: entry must be a string"
                | _ -> None

            let runtime =
                match doc.RootElement.TryGetProperty "runtime" with
                | true, value when value.ValueKind = JsonValueKind.String ->
                    let name = value.GetString()

                    if String.IsNullOrWhiteSpace name then
                        failwith "xantham.json: runtime must be a nonempty string"

                    Some name
                | true, _ -> failwith "xantham.json: runtime must be a string"
                | _ -> None

            let groups =
                match doc.RootElement.TryGetProperty "groups" with
                | true, v when v.ValueKind = JsonValueKind.Object ->
                    v.EnumerateObject()
                    |> Seq.map (fun p -> (p.Name * uom<_>), parseGroup p.Name p.Value)
                    |> Map.ofSeq
                | _ -> Map.empty

            let lib =
                match doc.RootElement.TryGetProperty "lib" with
                | true, v when v.ValueKind = JsonValueKind.Array ->
                    v.EnumerateArray()
                    |> Seq.map (fun e ->
                        if e.ValueKind = JsonValueKind.String then
                            e.GetString()
                        else
                            failwith "xantham.json: lib must be an array of strings")
                    |> Seq.toList
                    |> Some
                | true, _ -> failwith "xantham.json: lib must be an array of strings"
                | _ -> None

            let types =
                match doc.RootElement.TryGetProperty "types" with
                | true, value when value.ValueKind = JsonValueKind.Array ->
                    value.EnumerateArray()
                    |> Seq.map (fun item ->
                        if item.ValueKind <> JsonValueKind.String then
                            failwith "xantham.json: types must be an array of nonempty strings"

                        let name = item.GetString()

                        if String.IsNullOrWhiteSpace name then
                            failwith "xantham.json: types must be an array of nonempty strings"

                        name)
                    |> Seq.toList
                    |> Some
                | true, _ -> failwith "xantham.json: types must be an array of nonempty strings"
                | _ -> None

            let declarationReferences =
                match doc.RootElement.TryGetProperty "declarationReferences" with
                | true, value when value.ValueKind = JsonValueKind.Array ->
                    value.EnumerateArray()
                    |> Seq.map (fun item ->
                        if
                            item.ValueKind <> JsonValueKind.String
                            || String.IsNullOrWhiteSpace(item.GetString())
                        then
                            failwith "xantham.json: declarationReferences must be an array of nonempty paths"

                        item.GetString())
                    |> Seq.toList
                | true, _ -> failwith "xantham.json: declarationReferences must be an array of nonempty paths"
                | _ -> []

            let compilerLib =
                match doc.RootElement.TryGetProperty "compilerLib" with
                | false, _ -> CompilerLibConfig.Default
                | true, value when value.ValueKind = JsonValueKind.Object ->
                    let name (field: string) =
                        match value.TryGetProperty field with
                        | false, _ -> None
                        | true, name when name.ValueKind = JsonValueKind.String -> Some(name.GetString())
                        | true, _ -> failwith $"xantham.json: compilerLib.{field} must be a string"

                    let flag (field: string) =
                        match value.TryGetProperty field with
                        | false, _ -> false
                        | true, flag when flag.ValueKind = JsonValueKind.True -> true
                        | true, flag when flag.ValueKind = JsonValueKind.False -> false
                        | true, _ -> failwith $"xantham.json: compilerLib.{field} must be a boolean"

                    {
                        ModuleName = name "module"
                        EsModuleName = name "esModule"
                        DomModuleName = name "domModule"
                        AutoOpenEs = flag "autoOpenEs"
                        AutoOpenDom = flag "autoOpenDom"
                    }
                    |> fun config ->
                        CompilerLibLayout.create config |> ignore
                        config
                | true, _ -> failwith "xantham.json: compilerLib must be an object"

            {
                ModuleName = field "module"
                Namespace = field "namespace"
                Groups = groups
                Lib = lib
                Types = types
                DeclarationCatalog = boolField "declarationCatalog" false
                DeclarationReferences = declarationReferences
                Entry = entry
                RuntimePackage = runtime
                ResolveNoInfer = boolField "resolveNoInfer" GeneratorConfig.Default.ResolveNoInfer
                CompilerLib = compilerLib
            }

    /// Loads `<packageDir>/xantham.json`.
    let load (packageDir: string) : GeneratorConfig =
        loadFile (Path.Combine(packageDir, "xantham.json"))

    /// The npm package a binding's imports name at runtime, derived from the package the
    /// declarations were generated from.
    ///
    /// A DefinitelyTyped package ships no JavaScript, so an import that names it resolves to
    /// nothing at all: `@types/three` carries the types and `three` carries the code. The
    /// derivation is DefinitelyTyped's own naming convention, which is the only thing on disk
    /// that says what the runtime is called - a `@types/*` manifest has no field for it:
    ///
    /// - `@types/three` -> `three`;
    /// - `@types/babel__core` -> `@babel/core`, because DT mangles a scope into the name by
    ///   folding `@scope/name` to `scope__name` (it publishes one flat `@types` scope, so it
    ///   has nowhere else to put the scope);
    /// - anything not under `@types/` is its own runtime, unchanged.
    ///
    /// The mangling is only ambiguous against an unscoped package whose own name contains
    /// `__`, which DefinitelyTyped's convention cannot express either; `runtime` in
    /// `xantham.json` overrides the whole derivation for that and for every other package the
    /// convention does not describe.
    let derivedRuntimePackage (packageName: string<npmDependency>) =
        let packageName = packageName / uom<npmDependency>
        let prefix = "@types/"

        let specifier =
            if not (packageName.StartsWith(prefix, StringComparison.Ordinal)) then
                packageName
            else
                let mangled = packageName.Substring prefix.Length

                match mangled.IndexOf("__", StringComparison.Ordinal) with
                | -1 -> if mangled = "" then packageName else mangled
                | at -> $"@{mangled.Substring(0, at)}/{mangled.Substring(at + 2)}"

        specifier * uom<importSpecifier>

    /// The configured runtime package, or the derived one. What every `[<Import(…)>]` the run
    /// renders names.
    let runtimePackage (config: GeneratorConfig) (packageName: string<npmDependency>) =
        config.RuntimePackage |> Option.map (fun value -> value * uom<importSpecifier>) |> Option.defaultValue (derivedRuntimePackage packageName)

    /// The key a group is addressed by under `xantham.json`'s `groups`; `None` for the groups
    /// that are not configurable (the entry package always ships).
    let groupKey =
        function
        | EntryPackage
        | Unclassified -> None
        | CompilerLib -> Some ("typescript/lib" * uom<npmDependency>)
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
        string (Char.ToUpperInvariant part[0]) + part.Substring 1

    let private segments (text: string) =
        text.Split([| '-'; '_'; '.' |], StringSplitOptions.RemoveEmptyEntries)

    /// One path segment of a package name, PascalCased: `workers-types` -> `WorkersTypes`.
    let pascalSegment (text: string) =
        segments text |> Array.map capitalize |> String.concat ""

    /// True for the plain identifier shape, which is the form a generated name opens a module
    /// under. A JavaScript key outside it - `beta channel`, `@cf/meta` - names a type only.
    let nestable (name: string) = Identifier.isPlain name

    /// `name` in the shape an F# declaration admits: characters outside the plain identifier
    /// separate segments, and every segment after the first is capitalised, so
    /// `Registry@cf/meta` reads `RegistryCfMeta`. Backticks carry a keyword or a space into a
    /// declaration name; `` type ``Registry@cf/meta`` `` is FS0883. An identifier-shaped name
    /// is returned unchanged.
    let identifierName (name: string) =
        if nestable name then
            name
        else
            let separated =
                name
                |> Seq.map (fun c -> if Char.IsLetterOrDigit c then c else '-')
                |> String.Concat

            match pascalSegment separated with
            | "" -> "Item"
            | text when Char.IsLetter text[0] -> text
            | text -> "N" + text

    /// A package's module name: `@scope/pkg-name` -> `Scope.PkgName`.
    ///
    /// The name is taken from the runtime package, so a DefinitelyTyped package is named for the
    /// library it describes: `@types/three` -> `Three`, `@types/babel__core` -> `Babel.Core`. An
    /// F# consumer opens the binding for the library, and `@types/` is a TypeScript-side
    /// convention for attaching declarations to a package that shipped without them - it
    /// distinguishes nothing on this side, where the declarations and the library are one
    /// binding. Deriving it here rather than at the call site keeps a declaration and every
    /// cross-package reference to it on the same name.
    let packageModule (packageName: string<npmDependency>) =
        (GeneratorConfig.derivedRuntimePackage packageName / uom<importSpecifier>).TrimStart('@').Split('/')
        |> Array.map pascalSegment
        |> String.concat "."

    /// The compiler-lib group's namespace. Its two modules are written into one file under it.
    [<Literal>]
    let CompilerLibModule = "TypeScript.Lib"

    /// The compiler-lib group's ECMAScript module.
    [<Literal>]
    let CompilerLibEsModule = "TypeScript.Lib.Es"

    /// The compiler-lib group's DOM module: the browser, worker and script-host libs.
    [<Literal>]
    let CompilerLibDomModule = "TypeScript.Lib.Dom"

    /// The compiler-lib module a family (`Grouping.libFamily`) is written into.
    let compilerLibFamilyModule (family: string) =
        if family = "Dom" then
            CompilerLibDomModule
        else
            CompilerLibEsModule

    /// A package's module under a namespace: `FSharp.CloudEdge` over `@cloudedge/agents` is
    /// `FSharp.CloudEdge.Agents`.
    let private underNamespace (ns: string) (packageName: string<npmDependency>) =
        let derived = GeneratorConfig.derivedRuntimePackage packageName / uom<importSpecifier>
        $"{ns}.{pascalSegment (derived.Split('/') |> Array.last)}"

    /// A dependency's module under the entry package's configured namespace, or its derived
    /// module where the configuration leaves the dependency unnamed.
    ///
    /// The namespace reaches the groups `groups` names, which is the family the entry package
    /// declares itself part of. Selecting them by configuration rather than by npm scope lets a
    /// family span scopes, and lets an unscoped one exist at all.
    let namespacedModule (config: GeneratorConfig) (packageName: string<npmDependency>) =
        match config.Namespace with
        | Some ns when Map.containsKey packageName config.Groups -> underNamespace ns packageName
        | _ -> packageModule packageName

    /// The module a group's declarations live in (or are templated to live in).
    ///
    /// A package generated as the entry takes the same name under a namespace that a run
    /// referencing it templates, so one `namespace` in each member of a family is the whole
    /// configuration. The family's root sets `module` to take the namespace bare.
    let groupModule (config: GeneratorConfig) (entryPackageName: string<npmDependency>) =
        function
        | EntryPackage
        | Unclassified ->
            match config.ModuleName, config.Namespace with
            | Some name, _ -> name
            | None, Some ns -> underNamespace ns entryPackageName
            | None, None -> packageModule entryPackageName
        | CompilerLib -> CompilerLibModule
        | Dependency name -> namespacedModule config name

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
    /// The names `Xantham.Fable.Core` shadows into `Fable.Core.JS`. A generated module reaches
    /// them unqualified through the `open Fable.Core.JS` it carries. Where a package declares
    /// one of these names itself, the package's declaration takes the unqualified spelling and
    /// the support package's is reached as `JS.<name>`, repaired in `repair-arity`.
    module SupportBindings =
        let private names =
            set
                [
                    "Record"
                    "ReadonlyRecord"
                    "PropertyRecord"
                    "NoInfer"
                    "keyof"
                    "typekeyof"
                ]

        let shadows (name: string) = Set.contains name names

        /// The spelling that reaches the support package's declaration past a package's own.
        let qualify (name: string) = $"JS.{name}"

    /// ECMAScript bindings whose arity or loss requires an explicit mapping.
    module LibBindings =
        /// Name, F# arity, and the loss to record - `None` when the mapping gives up nothing.
        let private table =
            [
                "Promise", ("JS.Promise", 1, None)
                // A thenable is not a promise: TypeScript's `PromiseLike` is the structural
                // supertype, and reading one as `JS.Promise` claims methods it may not have.
                "PromiseLike", ("JS.Promise", 1, Some "PromiseLike reads as JS.Promise; a bare thenable is not one")
                "Map", ("JS.Map", 2, None)
                "ReadonlyMap",
                ("JS.Map", 2, Some "ReadonlyMap reads as JS.Map; the readonly restriction is not carried")
                "WeakMap", ("JS.WeakMap", 2, None)
                "Set", ("JS.Set", 1, None)
                "ReadonlySet",
                ("JS.Set", 1, Some "ReadonlySet reads as JS.Set; the readonly restriction is not carried")
                "WeakSet", ("JS.WeakSet", 1, None)
                // Fable compiles a JavaScript `Error` to `System.Exception`, which is what makes
                // a class over this base raisable and catchable by type. The instance surface is
                // where the two disagree: `name`, `stack` and `cause` are the JavaScript object's.
                "Error",
                ("exn", 0, Some "Error reads as exn; the JavaScript name, stack and cause properties are not on it")
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
                "IteratorResult", ("JS.IteratorResult", 1, None)
                // `Fable.Core` has no name for the combined shape; `JS.AsyncIterable` carries
                // only `Symbol.asyncIterator`, not the `next`/`return`/`throw` methods an
                // iterator adds.
                "AsyncIterableIterator",
                ("JS.AsyncIterable",
                 1,
                 Some "AsyncIterableIterator reads as JS.AsyncIterable; its next/return/throw methods are not on it")
            ]
            |> Map.ofList

        /// The binding for a lib name, if Fable.Core has one: its F# name, its arity, and the
        /// loss to record. `seq`-shaped names (`Iterable`, `Iterator`) are absent on purpose -
        /// Fable.Core binds only the async ones, and pretending `seq<'T>` interoperates with a
        /// JS iterable is exactly the kind of claim this table exists not to make.
        let tryFind (name: string) = Map.tryFind name table

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
        not (String.IsNullOrEmpty name)
        && (Char.IsLetter name[0] || name[0] = '_')
        && name |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_')

    /// The DU case name for a string-literal union member: PascalCased over separator
    /// segments (`"utf-8"` -> `Utf8`), prefixed when the result cannot start an F# case.
    /// Pinned like the module scheme - StringEnum case names are part of a binding's surface.
    let enumCaseOfString (text: string) =
        let cleaned =
            text
            |> Seq.map (fun c -> if Char.IsLetterOrDigit c then c else '-')
            |> String.Concat

        match pascalSegment cleaned with
        | "" -> "Empty"
        | name when Char.IsLetter name[0] -> name
        | name -> "N" + name

    /// The DU case name for a numeric-literal union member (D12): `1` -> `N1`,
    /// `1.5` -> `N1_5`, `-1` -> `NMinus1`.
    let enumCaseOfNumber (value: float) =
        let text = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        "N" + text.Replace("-", "Minus").Replace(".", "_")

    /// The import selector a default export binds under - the JavaScript key, not an F# name.
    /// Shared because a class's statics compose it into a dotted selector of their own
    /// (`default.MAX`, which Fable reads as "the default import, then `.MAX`").
    let defaultImportKey = "default"

    /// The name a default export falls back to when its symbol is itself named `default`:
    /// the package name's last segment, camelCased (`ansi-regex` -> `ansiRegex`).
    let defaultExport (packageName: string<npmDependency>) =
        let last = (packageName / uom<npmDependency>).TrimStart('@').Split('/') |> Array.last

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
    {
        /// The bound snapshot and project over the batching mailbox. Pure passes never touch it,
        /// which is what lets their tests fabricate a `Context` without a live compiler.
        Session: Session<TscMailbox>
        Config: GeneratorConfig
        /// Absolute path of the package being generated from.
        PackageDir: string<dirPath>
        /// The `name` field of the package manifest, or the directory name without one.
        PackageName: string<npmDependency>
        /// Absolute path of the declaration entry point the program was created over.
        EntryFile: string<declFile>
    }

/// What a pass produced: the advanced model, or the model plus the findings that say where the
/// pass fell short of Exact.
type PassOutcome<'Model> =
    | Advanced of 'Model
    | Degraded of 'Model * Finding list

/// A nano-pass: one conceptual transformation over its tier's model, in the pipeline's uniform
/// async shape whether it talks to the compiler or not.
type Pass<'Model> =
    {
        Name: string
        Run: Context -> 'Model -> Async<PassOutcome<'Model>>
    }

module Pass =
    /// Lifts a pure rewrite into the pipeline's uniform async shape.
    let pure' name (f: Context -> 'M -> 'M) : Pass<'M> =
        {
            Name = name
            Run = fun ctx m -> async { return Advanced(f ctx m) }
        }

// ---------------------------------------------------------------------------------------------
// Tier 1 - Harvest: what the author exported. Wire-driven inventory, no mapping decisions.
// ---------------------------------------------------------------------------------------------

/// A deterministic source-order key parsed from a declaration node handle (`index.kind.path`).
/// The handle is otherwise opaque; only the file path and node index are read, and only for
/// ordering output the way the author ordered source.
type DeclOrder = { File: string<declFile>; NodeIndex: int<nodeId> }

/// Where a harvested name came from, which is what decides how a *value* binds in JavaScript.
/// Types are unaffected: an interface is the same F# declaration either way.
type ExportOrigin =
    /// A member of the entry file's module symbol - bound with `[<Import(name, package)>]`.
    | FromModule
    /// An ambient declaration in global scope (`declare class Response`). A global type library
    /// has no module to import from; the name is already on `globalThis`, so values bind with
    /// `[<Global>]` instead.
    | FromGlobal
    /// An export of an ambient module declaration (`declare module "cloudflare:workers"`),
    /// carrying the specifier that declaration quotes. Values bind with
    /// `[<Import(name, specifier)>]`.
    | FromAmbientModule of specifier: string<importSpecifier>
    static member inline StringFromAmbientModule specifier = (specifier : string) * uom<importSpecifier> |> FromAmbientModule

/// The public JavaScript surface which owns an exported value occurrence.
type ExportOwner =
    | EntryModule
    | AmbientModule of specifier: string<importSpecifier>
    | GlobalScope

/// One export of the entry module, aliases already followed to their origin so re-exports
/// appear once under the name they are exported as. A global type library has no module to
/// export from, and its ambient declarations arrive here too - see `ExportOrigin`.
type HarvestedExport =
    {
        /// The name the entry module exports it under - `"default"` for a default export, and
        /// the declared name for an ambient global, which is exported under nothing.
        ExportName: string
        /// The origin symbol (`getAliasedSymbol` applied until stable).
        Symbol: SymbolResponse
        /// The entry exposes this symbol as a value, through no type-only import/export edge.
        /// The origin symbol's value flags alone do not establish a runtime export.
        HasValueExport: bool
        /// `getDocumentationComment`, already rendered to plain text by the wire.
        Docs: string
        Tags: JSDocTagInfo list
        Origin: ExportOrigin
        Order: DeclOrder option
    }

type HarvestModel =
    {
        Exports: HarvestedExport list
        /// The namespaces the entry package declares, by symbol id, under names an F# module
        /// can be spelled with. A declaration written inside one nests under it where a second
        /// declaration claims the same name.
        Namespaces: Map<int<symbolId>, string<symbolName>>
        /// Count of the entry package's own declared names that a `lib.*.d.ts` declaration of
        /// the same name precedes: `harvest-globals` groups such a name as the compiler lib
        /// (`Grouping.classify`) and it does not reach `Exports` - unless the compiler-lib
        /// group's own disposition ships it, in which case those names are harvested rather
        /// than shadowed and this count excludes them. Zero for a package that harvests through
        /// `harvest-exports` instead - the count is meaningful only for a global type library,
        /// which is the shape `harvest-globals` runs against.
        ShadowedByLib: int
    }

    static member Empty: HarvestModel =
        {
            Exports = []
            Namespaces = Map.empty
            ShadowedByLib = 0
        }

// ---------------------------------------------------------------------------------------------
// Tier 2 - Resolve: what the checker says everything is. A type table keyed by TypeResponse.Id.
// ---------------------------------------------------------------------------------------------

/// A property or parameter, resolved: the symbol plus the derived facts every shape pass
/// would otherwise re-ask the wire for.
type ResolvedMember =
    {
        Symbol: SymbolResponse
        Docs: string
        Tags: JSDocTagInfo list
        Optional: bool
        ReadOnly: bool
        TypeId: int<typeId>
    }

/// One index signature (`[key: string]: V`) as the resolve tier records it. These are
/// invisible to property enumeration - `getPropertiesOfType` returns nothing for a type whose
/// only content is an index signature - so a type can carry these and no members at all, and
/// the shape tier has to consult both before deciding a type has no shape worth declaring.
type ResolvedIndex =
    {
        KeyTypeId: int<typeId>
        ValueTypeId: int<typeId>
        IsReadonly: bool
    }

type ResolvedSignature =
    {
        Parameters: ResolvedMember list
        /// The signature's last parameter is a rest parameter (`...args`).
        HasRest: bool
        /// The signature's own type parameters (§4.9). A generic *function* carries them here
        /// rather than on its type, which is where a callback alias's `T` lives.
        TypeParameters: int<typeId> list
        /// A construct signature of an `abstract class`, which TypeScript refuses `new` on. It
        /// marks the class as one written to be derived from (§4.4).
        IsAbstract: bool
        ReturnTypeId: int<typeId>
    }

/// A conditional type's mapping facts (§4.11).
type ConditionalFacts =
    {
        /// The alias the conditional is written under.
        Name: string option
        /// The branch the mapping takes, named for the manifest and carried by id. Absent where
        /// both branches are reachable and inhabited.
        Branch: (string * int<typeId>) option
    }

/// A `TypeResponse` plus the derived facts of the kinds the skeleton resolves: object members,
/// call signatures, union membership. Everything else stays on the raw response.
type TypeFacts =
    {
        Response: TypeResponse
        /// The group the type's own symbol is declared in (O7). Meaningful for object types;
        /// primitives and unions stay `Unclassified`, which dispositions as the entry group.
        Origin: PackageId
        /// Name of the type's own symbol where it has one - what a `reference` emission
        /// templates with, and what a widening finding names.
        SymbolName: string<symbolName> option
        /// File of the type's own symbol's first declaration, as the wire reports it.
        DeclFile: string<declFile> option
        /// Complete declaration handles of the actual type symbol, retained for catalog identity.
        Declarations: string<declHandle> list
        /// Alias arguments retain concrete substitutions for declaration catalog specialization keys.
        DeclarationArguments: TypeResponse list
        /// Declaration handles of the alias applied at this type occurrence.
        AliasDeclarations: string<declHandle> list
        /// Symbol id of the declaration the type's own symbol is written inside - a namespace,
        /// where `HarvestModel.Namespaces` has a name for it.
        SymbolParent: int<symbolId> option
        Members: ResolvedMember list
        /// Index signatures (§4.10). Kept apart from `Members` because they are not properties:
        /// they have no name, and a type may carry one with no members at all.
        IndexInfos: ResolvedIndex list
        CallSignatures: ResolvedSignature list
        ConstructSignatures: ResolvedSignature list
        /// `extends` bases of an interface or class instance type, by id.
        BaseTypes: int<typeId> list
        /// Type arguments of a generic reference, resolved for *every* group - an external
        /// `Array<T>` carries entry-package types that must still be reached (O7 note).
        TypeArguments: int<typeId> list
        /// A tuple's per-element flags, in element order, copied off its *target* - the wire
        /// carries them there, not on the reference. The target itself is deliberately left out
        /// of the table: deriving it drags all of `Array.prototype` in again for every distinct
        /// tuple shape, and nothing but these flags is wanted from it.
        TupleElements: ElementFlags list
        /// The arguments the type's *alias* was written with, by id (§4.9). On the declaration
        /// form of a generic alias these are its own parameters - `type Mapper<T> = (t: T) => T`
        /// leaves the function type itself parameterless, so this is the only place `T` appears.
        AliasTypeArguments: int<typeId> list
        /// The constituents of an intersection, in the checker's order. Separate from
        /// `UnionMembers` because the two mean opposite things and the passes that read one
        /// must never see the other.
        IntersectionMembers: int<typeId> list
        /// A type parameter's `extends` bound, by id (§4.9). Only type parameters carry one.
        Constraint: int<typeId> option
        /// A type parameter's default type argument, by id (§4.9).
        Default: int<typeId> option
        /// `T extends U ? X : Y` (§4.11), where the type is one.
        Conditional: ConditionalFacts option
        UnionMembers: int<typeId> list
        /// Compiler-returned literal-union alias after removing nullish members; populated only for catalog generation or reuse.
        NonNullableAlias: int<typeId> option
        /// The alias name and single argument an indexed-access reference was written through,
        /// where the checker has already expanded past it before the flags reach the shaper
        /// (§4.11's `NoInfer`). Populated only at an indexed-access reference site, never on a
        /// declaration - unrelated to `AliasTypeArguments`, which serves the declaration form.
        AliasIdentity: (string<symbolName> * int<typeId>) option
    }

module TypeFacts =
    /// Facts before derivation: the response alone.
    let shallow (response: TypeResponse) =
        {
            Response = response
            Origin = Unclassified
            SymbolName = None
            DeclFile = None
            Declarations = []
            DeclarationArguments = []
            AliasDeclarations = []
            SymbolParent = None
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
            Conditional = None
            UnionMembers = []
            NonNullableAlias = None
            AliasIdentity = None
        }

/// The type ids an export resolves to. A symbol can be both a type and a value (a class), so
/// the two are separate fields rather than one.
type ExportTypeIds =
    {
        Declared: int<typeId> option
        Value: int<typeId> option
    }

type ResolveModel =
    {
        Harvest: HarvestModel
        /// Export symbol id -> the type ids the checker gave for it.
        ExportTypes: Map<int<symbolId>, ExportTypeIds>
        /// The type table. Closed: every id referenced by a `TypeFacts` is a key here or in
        /// `NotFollowed` - that closure is the tier's invariant.
        Types: Map<int<typeId>, TypeFacts>
        /// Ids deliberately not resolved, with the reason - the depth cutoff, or a response the
        /// compiler could not encode - so a reader of the table can tell "not followed" from
        /// "missing".
        NotFollowed: Map<int<typeId>, string>
    }

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
    /// TypeScript's `bigint`. Exact: Fable 5 compiles F# `bigint` to the native JavaScript
    /// `BigInt` - read off the emitted `BigInt.js` (`typeof x === "bigint"`, `fromInt32 n` is
    /// `BigInt(n)`) and run against a JS object handing one over, not recalled.
    | FsBigInt
    | FsUnit
    | FsObj
    | FsOption of FsTypeRef
    | FsArray of FsTypeRef
    /// A fixed-length tuple (D7, §4.12): Fable compiles an F# tuple to a JS array, so the
    /// mapping is exact. Optional tail elements arrive already `option`-wrapped, because the
    /// checker hands them over as `T | undefined`.
    | FsTuple of FsTypeRef list
    /// A heterogeneous union as Fable's erased `U2`-`U9` (D4, §4.5(4)); arities above nine
    /// render against a `U<n>` declared in the emitting file's own footer. Arms are distinct -
    /// a union whose arms collapse to one F# type is that type instead.
    | FsErasedUnion of FsTypeRef list
    /// A callback as a delegate (D5): parameter types and return. Renders as
    /// `System.Action`/`System.Func` so the arity is guaranteed at the Fable boundary. Emitted
    /// where `FsFunc` would lose that arity: at two or more parameters, and wherever the return
    /// is itself a callback (D5a).
    | FsDelegate of FsTypeRef list * FsTypeRef
    /// A callback as an F# function type: one argument and a return, where `unit` is the argument
    /// of a nullary callback (D5a). Fable hands a JavaScript function of the declared arity across
    /// the boundary in both directions for this shape, and the F# consumer composes it with `>>`
    /// and partial application.
    | FsFunc of FsTypeRef * FsTypeRef
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
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        ReadOnly: bool
        Type: FsTypeRef
    }

/// A type parameter (§4.9), bound by a declaration or by a generic signature of its own. The
/// constraint is carried only when F# can express it - a subtype constraint against another
/// generated interface. TypeScript bounds that have no F# form (`extends string`, `extends
/// keyof T`) are dropped with a finding rather than approximated, because a wrong constraint
/// rejects correct code.
type FsTypeParam =
    {
        Name: string
        Constraint: FsTypeRef option
    }

type FsParam =
    {
        Name: string
        Optional: bool
        /// A rest parameter: rendered `[<ParamArray>]` on static emissions and abstract members
        /// alike, so Fable spreads the array at the call.
        Rest: bool
        Type: FsTypeRef
    }

type FsMethodMember =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        /// The method's *own* parameters, where it is generic independently of its declaration:
        /// `read<K extends keyof T>(key: K)` binds `K` here and reads `T` from the interface.
        TypeParameters: FsTypeParam list
        Parameters: FsParam list
        Return: FsTypeRef
    }

/// A TypeScript index signature rendered as F#: an `Item` member under `[<EmitIndexer>]`, so
/// `bag["key"]` is what reaches JavaScript rather than a `.Item(...)` call (§4.10). A readonly
/// signature drops the setter.
type FsIndexerMember =
    {
        Key: FsTypeRef
        Value: FsTypeRef
        ReadOnly: bool
    }

/// An interface member. Overloads are consecutive `FsMethod` entries sharing a name -
/// overloaded abstract members are legal F#.
/// A construct signature of a *constructor object* - the thing `typeof Request` names, and the
/// type of a `declare var Request: { new (...): Request }` (§4.4). F# has no first-class
/// constructor-object type, so the object is declared as an interface of its own and each of
/// its construct signatures becomes an `[<EmitConstructor>]` `Create` member: `$0` is the
/// object the member is read off, so `scope.Request.Create(url)` reaches JavaScript as
/// `new scope.Request(url)` rather than as a call.
///
/// The name is always `Create` - overloads are consecutive entries, exactly as methods are.
type FsConstructorMember =
    {
        Docs: string
        Tags: JSDocTagInfo list
        /// The signature's own parameters: `new <T>(value: T): Box<T>` binds `T` per call, and
        /// the constructor object itself is not generic, so this is where they go.
        TypeParameters: FsTypeParam list
        Parameters: FsParam list
        Return: FsTypeRef
    }

type FsMember =
    | FsProperty of FsPropertyMember
    | FsMethod of FsMethodMember
    | FsIndexer of FsIndexerMember
    | FsConstructor of FsConstructorMember
    /// A hybrid's call signature, reached through `[<Emit("$0($1...)")>]`: `x.Invoke(a)` applies
    /// the receiver to the arguments, so it reaches JavaScript as `x(a)` (§4.4's counterpart for
    /// the call side). Shares `FsConstructorMember`'s shape - the name is fixed the same way.
    | FsInvoke of FsConstructorMember

/// How a value export is bound to its JavaScript module.
type ImportBinding =
    | ImportDefault
    | ImportNamed of string
    /// An ambient global (`declare function fetch`): there is no module to import from, so the
    /// name is taken off `globalThis` with `[<Global>]`.
    | GlobalName of string
    /// An export of an ambient module, under the specifier that module declares. The specifier
    /// is the declaration's own rather than the run's runtime package.
    | ImportFrom of name: string * specifier: string<importSpecifier>

/// What one *bound* member is - a member whose body is not F# but a reference into JavaScript,
/// carried by an `ImportBinding`. Two kinds of declaration hold these: the `Exports` type, whose
/// members are the module's value exports, and a class, whose statics live on the constructor
/// object rather than on an instance.
type FsExportBody =
    /// A top-level exported function; overloads are consecutive members sharing a name.
    | ExportFunction of FsParam list * FsTypeRef
    /// An exported value (`const`/`let`, or a namespace object): a get-only property.
    | ExportValue of FsTypeRef
    /// A class constructor: `[<EmitConstructor>]`, so `Exports.Name(...)` is `new Name(...)`.
    | ExportConstructor of FsParam list * FsTypeRef

/// One bound member: an `Exports` member, or a class static.
type FsExportMember =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        /// A top-level generic function binds its parameters on the member: `Exports` itself is
        /// not generic, so `get<T>(source: T)` has nowhere else to put `T`.
        TypeParameters: FsTypeParam list
        /// The selector the member is read under. A settable member is bound through the object
        /// it hangs off (`Counter`, `globalThis`) and carries the attribute on its declaring
        /// type; every other member is bound through its own dotted selector (`Counter.MAX`) and
        /// carries the attribute itself.
        Binding: ImportBinding
        Body: FsExportBody
        /// A settable static or mutable global, rendered `with get, set`. Assignment to it
        /// compiles to a JavaScript property write.
        Settable: bool
    }

type OwnedExportMember =
    {
        Owner: ExportOwner
        HarvestIndex: int
        ExportName: string
        SourceSymbolId: int<symbolId>
        SignatureOrdinal: int option
        Member: FsExportMember
    }

type FsExportContainer =
    {
        Name: string
        Owner: ExportOwner
        Members: OwnedExportMember list
    }

/// What makes a declaration an F# *class* rather than an interface (§4.4): the import that binds
/// the JavaScript constructor and the parameters a derived class passes to it. An F# interface
/// admits no `inherit`, so a TypeScript class written to be derived from - an entrypoint an
/// ambient module exports - reaches F# only in this form.
type FsEntrypoint =
    {
        /// The specifier and name the constructor is imported under, carried at the type level so
        /// that Fable's `super(...)` reaches it.
        Binding: ImportBinding
        /// The primary constructor's parameters, from the class's first construct signature.
        Parameters: FsParam list
        /// The F# base the class derives, rendered as the `inherit` line of the class form.
        /// `Some exn` where the TypeScript base is `Error`: a consumer raises the declaration and
        /// catches it by type. `None` where the class has no base F# reaches.
        Inherits: FsTypeRef option
    }

type FsInterfaceDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        TypeParameters: FsTypeParam list
        /// Base interfaces (`extends`, or a class base) - rendered as `inherit` lines.
        Inherits: FsTypeRef list
        Members: FsMember list
        /// Present where the declaration renders as an `[<AbstractClass>]` a consumer inherits;
        /// `None` where it renders as an `[<Interface>]`.
        Entrypoint: FsEntrypoint option
        /// `[<ParamObject; Emit("$0")>]` Create overloads for plain-data interfaces (D3) -
        /// parameter lists mirroring the members, so consumers never hand-build objects.
        CreateOverloads: FsParam list list
        /// A class's static members (§4.4): the properties of the constructor object. A get-only
        /// static is bound through a dotted selector of its own (`[<Import("Counter.MAX",
        /// "pkg")>]`); a settable one is bound through the type-level attribute this list puts on
        /// the declaration. Either way a consumer spells `Counter.MAX` as TypeScript does. Empty
        /// for everything that is not an exported class.
        Statics: FsExportMember list
    }

/// One case of a `[<StringEnum>]` DU. `CompiledName` carries the literal when it differs from
/// the case name; `CompiledValue` carries a non-string literal (D12).
type FsUnionCase =
    {
        Name: string
        CompiledName: string option
        CompiledValue: FsLiteral option
    }

type FsStringEnumDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        Cases: FsUnionCase list
    }

/// One parameter of a named delegate declaration: the name TypeScript spelled, and the F# type
/// written at that position.
type FsDelegateParam = { Name: string; Type: FsTypeRef }

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
    {
        Name: string
        CompiledName: string option
        Fields: FsTaggedField list
    }

/// A discriminated union the checker proved is tagged (D4, §4.5(2)): every member is an object
/// type carrying the same property, and that property's type is a distinct string literal in
/// each. Fable erases the DU to a plain object literal, so this is Exact *and* pattern-matchable
/// - by far the best consumer experience, which is why §4.5 says to detect it aggressively.
type FsTaggedUnionDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        /// The discriminant property's name, as TypeScript spells it.
        Tag: string
        Cases: FsTaggedCase list
    }

/// A numeric TS enum as an F# enum - `type E = A = 1` (§4.7).
type FsEnumDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        Cases: (string * int) list
    }

/// A type abbreviation: an exported alias whose right side is a reference, not a shape of its
/// own (callback aliases to delegates, alias-of-alias, primitive aliases).
type FsAbbrevDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        /// The alias's own type parameters, in declaration order (§4.9). A generic alias binds
        /// them on its left side exactly as TypeScript does: `type Callback<'T> = Func<'T, obj>`.
        TypeParameters: FsTypeParam list
        Target: FsTypeRef
        /// A re-exported class can share an instance alias while binding its constructor value here.
        Value: (ImportBinding * FsTypeRef) option
    }

/// A callback declared as a named F# delegate: `type TickHandler = delegate of x: float * y:
/// float -> string` (D5). It guarantees arity at the Fable boundary exactly as `System.Func` and
/// `System.Action` do, and carries the parameter names TypeScript spelled on top of that, so
/// `handler.Invoke(x = 1.0, y = 2.0)` and every tooltip reads them.
type FsDelegateDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        /// The delegate's own type parameters, in declaration order (§4.9), written on its left
        /// side: `type Reader<'T> = delegate of source: 'T -> string`.
        TypeParameters: FsTypeParam list
        /// Empty for a nullary callback, which renders `delegate of unit -> ...`.
        Parameters: FsDelegateParam list
        Return: FsTypeRef
    }

/// A declaration TypeScript *computes* and F# cannot reproduce: a mapped type, a conditional or
/// a template literal at an operand the checker could not resolve (§4.10, §4.11). There is no
/// structure to emit - the structure is a function of an argument not yet supplied - so the
/// declaration is erased and keeps only its name and arity, which is enough for uses of it to
/// stay distinct from each other and from `obj`. Its single case is private, so the only way in
/// or out is a cast, which is exactly the guarantee the generator can honestly make.
type FsPhantomDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        TypeParameters: FsTypeParam list
        /// What the value is at runtime once erased: `string` for a template literal or an
        /// intrinsic string mapping, `obj` for everything else.
        Carrier: FsTypeRef
    }

/// A unit of measure standing for a branding intersection (§4.6, D11). It has no body: a
/// measure is a name and nothing else, and the brand it marks is written at the *uses*, as
/// `string<UserId>`, rather than as an abbreviation - the name can only be spent once, and a
/// measure is what spends it.
type FsMeasureDecl =
    {
        Name: string
        Docs: string
        Tags: JSDocTagInfo list
        Order: DeclOrder option
        /// The primitive the brand is over, kept for the manifest and the doc comment: a
        /// measure itself says nothing about what it annotates.
        Primitive: FsTypeRef
    }

type FsDecl =
    | FsInterface of FsInterfaceDecl
    | FsStringEnum of FsStringEnumDecl
    | FsPhantom of FsPhantomDecl
    | FsMeasure of FsMeasureDecl
    | FsTaggedUnion of FsTaggedUnionDecl
    | FsEnum of FsEnumDecl
    | FsAbbrev of FsAbbrevDecl
    | FsDelegateType of FsDelegateDecl
    /// The one `Exports` type gathering the module's value exports.
    | FsExports of FsExportContainer

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
    {
        Harvest: HarvestModel
        ExportTypes: Map<int<symbolId>, ExportTypeIds>
        Types: Map<int<typeId>, TypeFacts>
        NotFollowed: Map<int<typeId>, string>
        /// Type id -> the F# type name this run declares for it - exports named first, then
        /// synthesized names for reachable anonymous shapes (hash-consing by id, §4.4). What
        /// lets a reference come out as `FsNamed` rather than an expansion.
        DeclNames: Map<int<typeId>, string>
        /// Type id -> the source order its declaration sorts under: the export's own order, or
        /// for a synthesized declaration the order of the export that first reached it.
        DeclOrders: Map<int<typeId>, DeclOrder option>
        /// Type id -> the type-parameter ids a declaration reads without binding, in first-use
        /// order (§4.9). An anonymous object type hoisted out of a generic scope - the `props`
        /// of `each<T, U>(props: { items: T[]; render: (item: T) => U })` - binds nothing of
        /// its own, so it is declared over these and every reference applies them back.
        DeclParams: Map<int<typeId>, int<typeId> list>
        /// Recognized generic application id -> the declaration id whose name it references.
        AliasApplications: Map<int<typeId>, int<typeId>>
        /// `Exports` members accumulated by the class/function/value passes, keyed by harvest
        /// position so `order-declarations` can assemble them in source order.
        ExportMembers: OwnedExportMember list
        /// Type-parameter id -> the name it is in scope under, for the declaration currently
        /// being shaped. Scope lives on the model rather than in `typeRef`'s arguments because
        /// it is a property of *where* the reference is written, not of the reference: a pass
        /// binds it once around a declaration and every nested `typeRef` inherits it.
        TypeVars: Map<int<typeId>, string>
        /// Type-parameter id -> the support-package idiom its uses are written as, for the
        /// signature currently being shaped (§4.10). Scoped like `TypeVars`, and for the same
        /// reason: `K extends keyof T` binds nothing outside the signature that declared it.
        KeyVars: Map<int<typeId>, KeyBinding>
        Decls: FsDecl list
    }

// ---------------------------------------------------------------------------------------------
// Tier 4 - Render: source text plus the fidelity manifest.
// ---------------------------------------------------------------------------------------------

type RenderModel =
    {
        ModuleName: string
        PackageName: string<npmDependency>
        /// The npm package every `[<Import(…)>]` names - the configured `runtime`, or the one
        /// derived from `PackageName`. Distinct from `PackageName`, which stays the package the
        /// *declarations* came from: a `@types/*` package is the provenance of the binding and
        /// never the specifier a consumer's bundler resolves.
        RuntimePackage: string<importSpecifier>
        /// Absolute path of the package generated from; the manifest writes declaration files
        /// relative to it.
        PackageDir: string<dirPath>
        Decls: FsDecl list
        /// Every finding of every earlier tier, stamped with its pass.
        Findings: Finding list
        /// Rendered output: file name -> content. Written to disk by `Pipeline.run`, not here,
        /// so rendering stays pure.
        Files: (string * string) list
        /// `HarvestModel.ShadowedByLib`, carried through for the CLI's own diagnostics - no
        /// tier past harvest reads or changes it.
        ShadowedByLib: int
    }

type TierCounts =
    {
        Exact: int
        Ergonomic: int
        Widened: int
        Escape: int
    }

/// What a run reports back: where the fidelity manifest's numbers come from.
type RunReport =
    {
        ModuleName: string
        OutputFiles: string list
        Findings: Finding list
        Counts: TierCounts
        /// Count of the entry package's own declared names a `lib.*.d.ts` declaration of the
        /// same name shadowed (`HarvestModel.ShadowedByLib`).
        ShadowedByLib: int
    }

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
                match Int32.TryParse index with
                | true, index -> Some { File = (path * uom<declFile>); NodeIndex = (index * uom<nodeId>) }
                | _ -> None
            | _ -> None
        | _ -> None

    /// The file of a symbol's first declaration.
    let declFile (symbol: SymbolResponse voption) : string option =
        symbol
        |> ValueOption.bind (fun s -> declOrder s.Declarations |> ValueOption.ofOption)
        |> ValueOption.map (fun order -> (order.File / uom<declFile>).Replace('\\', '/'))
        |> ValueOption.toOption

    /// The compiler-lib family a declaration file belongs to: `Dom` for the browser and worker
    /// libs, `Es` for the ECMAScript libs.
    let libFamily (file: string) : string =
        let name = file.Substring(file.LastIndexOf '/' + 1)

        if
            name.StartsWith "lib.dom"
            || name.StartsWith "lib.webworker"
            || name.StartsWith "lib.scripthost"
        then
            "Dom"
        else
            "Es"

    /// Classifies a declaration's origin group (O7) from its file path: under the
    /// package directory and outside any `node_modules` below it is the entry package; the
    /// compiler's default libs are the compiler-lib group; under a `node_modules` entry is that
    /// dependency, at whatever depth npm installed it; anything else - including anonymous
    /// shapes with no declaration - is unclassified, which dispositions as the entry group.
    ///
    /// The default libs are recognised three ways because the compiler serves them three ways:
    /// from the platform package (`node_modules/@typescript/typescript-<rid>/lib/lib.*.d.ts` -
    /// what the live wire reports), from `typescript/lib`, or as `bundled:` pseudo-paths for the
    /// embedded copies. A non-entry `lib.*.d.ts` anywhere else still classifies as compiler lib
    /// rather than unclassified: unclassified means Ship, and full derivation of a mistaken
    /// standard-lib file is the expensive failure, while a mis-grouped oddball is a visible
    /// finding.
    let classifyFile (packageDir: string<dirPath>) (filePath: string<filePath>) : PackageId =
        let path = (filePath / uom<filePath>).Replace('\\', '/')
        let root = (packageDir / uom<dirPath>).Replace('\\', '/').TrimEnd '/' + "/"
        let file = path.Substring(path.LastIndexOf '/' + 1)
        let isLibFile = file.StartsWith "lib." && file.EndsWith ".d.ts"
        let installedAt = path.LastIndexOf "/node_modules/"

        // npm installs a package's dependencies under the package's own `node_modules`, so
        // a dependency's path carries the entry package's directory as a prefix. The
        // deepest `node_modules` boundary decides the group: one below the entry directory
        // separates a dependency from its host, one at or above it is the entry package's
        // own installation.
        if
            path.StartsWith(root, StringComparison.OrdinalIgnoreCase)
            && installedAt < root.Length - 1
        then
            EntryPackage
        else
            match installedAt with
            | -1 -> if isLibFile then CompilerLib else Unclassified
            | at ->
                match path.Substring(at + "/node_modules/".Length).Split '/' with
                | parts when parts.Length > 0 && (parts[0] = "typescript" || parts[0] = "@typescript") -> CompilerLib
                | _ when isLibFile -> CompilerLib
                | parts when parts.Length > 1 && parts[0].StartsWith "@" -> Dependency ($"{parts[0]}/{parts[1]}" * uom<npmDependency>)
                | parts when parts.Length > 0 -> Dependency (parts[0] * uom<npmDependency>)
                | _ -> Unclassified

    /// Entry sources first, then package-relative dependency and compiler sources.
    /// Compiler installation directories must not decide declaration or export order.
    let sourceOrderKey (packageDir: string<dirPath>) (filePath: string<filePath>) =
        let path = (filePath / uom<filePath>).Replace('\\', '/')

        let relative () =
            Path.GetRelativePath(packageDir / uom<dirPath>, path).Replace('\\', '/')

        match classifyFile packageDir filePath with
        | EntryPackage -> 0, "", relative ()
        | CompilerLib -> 1, "typescript/lib", path.Substring(path.LastIndexOf '/' + 1)
        | Dependency package ->
            let package = package / uom<npmDependency>
            let at = path.LastIndexOf("/node_modules/", StringComparison.Ordinal)
            1, package, path.Substring(at + "/node_modules/".Length + package.Length + 1)
        | Unclassified -> 1, "", relative ()

    /// A symbol's origin, using its first declaration. The synthetic global environment has
    /// no declaration; compiler-lib disposition controls whether resolve follows its members.
    let classify (packageDir: string<dirPath>) (symbol: SymbolResponse voption) : PackageId =
        match
            symbol
            |> ValueOption.bind (fun s -> declOrder s.Declarations |> ValueOption.ofOption)
        with
        // `typeof globalThis` (type-fest's `GlobalThis`) is the checker's own symbol for the
        // global scope: it declares nothing anywhere, so by path it would be unclassified and
        // shipped - as one interface carrying every global there is, a third of the file
        // for a type whose members nobody would call through it. The scope is the compiler's,
        // so it groups with the compiler lib and widens with a name, identity only.
        | ValueNone when symbol |> ValueOption.exists (fun s -> s.Name = "globalThis") -> CompilerLib
        | ValueNone -> Unclassified
        | ValueSome order -> classifyFile packageDir (order.File / uom<node>)

    /// Whether any of `symbol`'s declarations sits under `packageDir`, by the same root test
    /// `classify` applies to only the first. Declaration merging can carry a symbol's list past
    /// its own file: a `lib.*.d.ts` declaration first in the list sends `classify` to
    /// `CompilerLib`, and this is what still finds the entry package's own declaration among
    /// the rest.
    let declaresUnderPackage (packageDir: string<dirPath>) (symbol: SymbolResponse) : bool =
        let root = (packageDir / uom<dirPath>).Replace('\\', '/').TrimEnd '/' + "/"

        let underRoot (path: string) =
            let path = path.Replace('\\', '/')
            let installedAt = path.LastIndexOf "/node_modules/"

            path.StartsWith(root, StringComparison.OrdinalIgnoreCase)
            && installedAt < root.Length - 1

        symbol.Declarations
        |> ValueOption.map (fun handles ->
            handles
            |> Array.exists (fun handle ->
                match handle.Split([| '.' |], 3) with
                | [| _; _; path |] -> underRoot path
                | _ -> false))
        |> ValueOption.defaultValue false
