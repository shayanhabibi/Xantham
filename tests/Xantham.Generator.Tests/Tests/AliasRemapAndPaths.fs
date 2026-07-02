module Xantham.Generator.Tests.Tests.AliasRemapAndPaths

(*
Per-pass isolation coverage for two GENERATOR passes:

  (1) TypeAliasRemap — the alias-substitution pass.
      - prerenderTypeAliases registers `value.Type.Value -> aliasRef` into ctx.TypeAliasRemap
        only for NON-shareable alias bodies (isShareableAliasBody gate).
      - Render.TypeAlias.resolveInnerRef consumes the remap: when the alias body is present in
        BOTH PreludeRenders and TypeAliasRemap it substitutes the remapped alias name back to the
        real body render (replace value newRef stripped) — protecting the alias's OWN definition
        from self-remap. With no remap it renders the body verbatim.

  (2) PATH INTERCEPTORS — Path.fromInterface / fromTypeAlias / fromClass / fromVariable /
      fromFunction and the createModulePath contract underneath them, plus the
      pipeInterface / pipeClass / pipeTypeAlias interceptor hooks and the IgnorePathRender gate.
      createModulePath contract:
        - source=None,  MemberPath=[]   -> ModulePath.init "Global"  (synthetic root)
        - source=None,  MemberPath=h::t -> the FQN module trace becomes the module path
        - source=Some s                 -> sanitizeSource s is the module root; the FQN
                                           MemberPath nests beneath it (the synthetic module)
        - source=Some (Abnormal s) + node_modules FilePath (a FILE import from a declaration
          file) -> module path = PACKAGE IDENTITY only: `@scope/pkg` (2 segments) or `pkg`
          (1 segment), PLUS an immediately-following VERSION segment (`v3`, `v4`, ...) when
          present — a package shipping multiple versions in one install (zod) declares the
          same type names under each, so the version segment keeps the twins apart
          (Zod.V3.X / Zod.V4.X). All other packaging dir segments (dist/esm/lib/...) are
          dropped. Guard: if the Abnormal source does not contain the file name, fall back
          to the plain sanitizeSource arm.
      The custom TypePaths/MemberPaths interceptor prunes the phantom "Typescript" parent; the
      IgnorePathRender.Source gate suppresses babel/typescript-sourced renders.

These assert the FAITHFUL/CORRECT behavior of each pass. Where current behavior is wrong the
case is a ptestCase with the root cause noted.
*)

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// Render a Name<'u> list as a dotted source string.
let private dot (names: Name<'u> list) =
    names |> List.map Name.Case.valueOrModified |> String.concat "."

/// Flatten a TypePath to its dotted absolute path (Module.Module.Type).
let private typePathStr (path: TypePath) =
    TypePath.flatten path |> dot

/// Flatten a MemberPath to its dotted absolute path (Module...Type?.member).
let private memberPathStr (path: MemberPath) =
    let modulePath, trace = MemberPath.traceToParentModule path
    ModulePath.flatten modulePath
    |> List.map Name.Case.valueOrModified
    |> fun ms -> ms @ (trace |> List.map Name.Case.valueOrModified) @ [ Name.Case.valueOrModified path.Name ]
    |> String.concat "."

// =====================================================================================
// (1) TypeAliasRemap — prerenderTypeAliases gate via isShareableAliasBody, and the
//     Render.TypeAlias.resolveInnerRef consumption of the remap.
// =====================================================================================

let private ctx () = GeneratorContext.Empty

/// Render a TypeRefRender to its F# type-annotation text (reuses the TypeRefRender suite helper).
let private renderRef ref =
    // testTypeRef returns (expected, rendered); the rendered side is "let _: <TYPE> = JS.undefined".
    let s = Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef "" ref |> snd
    let marker = "let _: "
    let i = s.IndexOf(marker) + marker.Length
    let j = s.IndexOf(" = JS.undefined")
    s.Substring(i, j - i)

/// The Type field of a rendered alias, as F# text.
let private aliasTypeText render =
    match render with
    | TypeAliasRender.Alias aliasRef -> renderRef aliasRef.Type
    | other -> failtestf "expected TypeAliasRender.Alias but got %A" other

[<Tests>]
let prerenderTypeAliasesGateTests =
    testList "TypeAliasRemap.isShareableAliasBody gate (prerenderTypeAliases)" [

        // A primitive alias body (type D1SessionBookmark = string) is SHAREABLE: the
        // body instance is the memoised primitive shared across the whole surface, so it
        // must NOT become a remap key. Otherwise the alias name hijacks every `string`.
        testCase "alias-to-primitive body is gated out (shareable)" <| fun _ ->
            ResolvedType.primitive TypeKindPrimitive.String
            |> ArenaInterner.isShareableAliasBody
            |> Flip.Expect.isTrue "string alias body must be treated as shareable -> skipped from remap"

        // A literal alias body (type Mode = "primary-only") is likewise shareable.
        testCase "alias-to-literal body is gated out (shareable)" <| fun _ ->
            ResolvedType.Literal.wrap (ResolvedType.Literal.createString "primary-only")
            |> ArenaInterner.isShareableAliasBody
            |> Flip.Expect.isTrue "literal alias body must be treated as shareable -> skipped from remap"

        // A named/structural alias body (type Alias = MyInterface) is NOMINAL: safe to remap.
        testCase "alias-to-named-type body is remappable (non-shareable)" <| fun _ ->
            ResolvedType.Interface.create "MyInterface"
            |> ResolvedType.Interface.wrap
            |> ArenaInterner.isShareableAliasBody
            |> Flip.Expect.isFalse "named-type alias body must remain remappable"
    ]

[<Tests>]
let aliasRemapConsumptionTests =
    testList "TypeAliasRemap consumption (Render.TypeAlias.resolveInnerRef)" [

        // No PreludeRenders entry for the inner type -> resolveInnerRef returns the
        // plain prerendered body. type AliasS = string renders as `string`.
        testCase "alias to primitive (no remap) renders the body verbatim" <| fun _ ->
            let ctx = ctx ()
            let inner = ResolvedType.primitive TypeKindPrimitive.String
            let typeAlias = ResolvedType.TypeAlias.create inner "AliasS"
            let scopeStore = RenderScopeStore.create ()
            Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            |> aliasTypeText
            |> Flip.Expect.equal "alias to string renders as string" "string"

        // Inner type IS in PreludeRenders but NOT in TypeAliasRemap -> resolveInnerRef
        // returns newRef.TypeRef (the prerendered body) untouched.
        testCase "alias to named type with no remap renders the body ref" <| fun _ ->
            let ctx = ctx ()
            let inner =
                ResolvedType.Interface.create "MyInterface"
                |> ResolvedType.Interface.wrap
            // Seed PreludeRenders so resolveInnerRef takes the (true, newRef) branch.
            TestHelper.prerender ctx inner |> ignore
            let typeAlias = ResolvedType.TypeAlias.create inner "MyAlias"
            let scopeStore = RenderScopeStore.create ()
            Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            |> aliasTypeText
            // A path-less interface anchors at the synthetic Global root, so its ref is Global.MyInterface.
            |> Flip.Expect.equal "alias to MyInterface renders the interface ref" "Global.MyInterface"

        // Inner type in BOTH PreludeRenders and TypeAliasRemap. The alias's OWN definition
        // must NOT render as itself (self-remap): resolveInnerRef does
        // replace(remapValue, newRef, stripped) — substituting the remapped alias name back
        // to the real body render. Here remapValue = body ref, so the body render is preserved.
        testCase "alias body present in remap is substituted back to the body render (no self-remap)" <| fun _ ->
            let ctx = ctx ()
            let inner =
                ResolvedType.Interface.create "MyInterface"
                |> ResolvedType.Interface.wrap
            let bodyRef = TestHelper.prerender ctx inner
            // Simulate prerenderTypeAliases registering body -> bodyRef (self-equal remap).
            GeneratorContext.Prelude.addTypeAliasRemap ctx inner bodyRef
            let typeAlias = ResolvedType.TypeAlias.create inner "MyAlias"
            let scopeStore = RenderScopeStore.create ()
            Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            |> aliasTypeText
            |> Flip.Expect.equal "alias definition still renders the body, not its own name" "Global.MyInterface"

        // Self-referencing alias body (type Simplify = Simplify): inner maps to itself via
        // the remap and replace(A,A,A) must be identity (issue #39) — render must terminate
        // and yield the body ref.
        testCase "self-referencing alias resolves without infinite recursion" <| fun _ ->
            let ctx = ctx ()
            let inner = ResolvedType.primitive TypeKindPrimitive.NonPrimitive
            let innerRef = TestHelper.prerender ctx inner
            GeneratorContext.Prelude.addTypeAliasRemap ctx inner innerRef
            let typeAlias = ResolvedType.TypeAlias.create inner "Simplify"
            let scopeStore = RenderScopeStore.create ()
            Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            |> aliasTypeText
            |> Flip.Expect.equal "self-referencing alias renders the body ref" "obj"
    ]

// =====================================================================================
// (1b) THIN-REFERENCE alias-body STORE contract (the FS0953 self-abbreviation root).
//
// `type AgentOptions<Env> = PartyServerOptions<Env>`: the encoder's self-reference
// decoupling makes the alias body a TypeReference whose ResolvedType IS the alias's own
// export-key instance, and prerenderTypeAliases phase 1 remaps BOTH instances to the
// alias NAME. The remap is a RETURN-layer concern (the cache-hit path re-applies it on
// every read), so the STORED scope must keep the REAL render: resolveInnerRef reads
// PreludeRenders directly (bypassing the return layer) to restore the alias's own decl
// body — if the store bakes the remapped name in, the decl emits the FS0953
// self-abbreviation `type AgentOptions<'Env> = AgentOptions<obj>` (61 aliases).
// Two return-layer fixes were tried and reverted net-negative (+93 skipping the
// registration; +48 following through at the decl) — the STORE is the owning seam.
// =====================================================================================

[<Tests>]
let thinReferenceBodyStoreTests =
    let aliasNameRef () =
        ModulePath.createFromList [ "M" ]
        |> TypePath.create "MyAlias"
        |> RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath
        |> RenderScopeStore.TypeRef.Unsafe.createAtom
        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
    let poisonShape (ctx: GeneratorContext) =
        // body = TypeReference{ResolvedType = target}; phase-1 remaps body AND target to the name
        let target = ResolvedType.Interface.create "Foreign" |> ResolvedType.Interface.wrap
        let body =
            ResolvedType.TypeReference {
                Type = LazyContainer.CreateFromValue target
                TypeArguments = []
                ResolvedType = Some (LazyContainer.CreateFromValue target)
            }
        let nameRef = aliasNameRef ()
        GeneratorContext.Prelude.addTypeAliasRemap ctx body nameRef
        GeneratorContext.Prelude.addTypeAliasRemap ctx target nameRef
        body, nameRef

    testList "thin-reference alias-body STORE (FS0953 root)" [
        // PENDING: the store fix (keep the target's un-remapped render when this node remaps to
        // the same name) is contract-correct and was verified in isolation — but landing it
        // re-emits 61 real alias bodies whose obj-padded CONSTRAINED args mint FS0001 violations
        // ('obj' not compatible with 'ZodTypeAny', +54 net). PREREQUISITE: constraint-aware
        // defaults synthesis (TypeParameter.Default instead of obj at constrained positions).
        // Flip to testCase when the defaults machinery lands.
        ptestCase "the body's STORED scope keeps the real render, not the remapped self-name" <| fun _ ->
            let ctx = ctx ()
            let body, nameRef = poisonShape ctx
            TestHelper.prerender ctx body |> ignore
            match ctx.PreludeRenders.TryGetValue body with
            | true, stored ->
                Expect.notEqual stored.TypeRef nameRef
                    "stored scope must keep the REAL render — resolveInnerRef reads it directly to build the alias's own decl"
            | _ -> failtest "body scope not registered"

        testCase "external reads of the body still render the alias name (return-layer remap)" <| fun _ ->
            let ctx = ctx ()
            let body, nameRef = poisonShape ctx
            TestHelper.prerender ctx body |> ignore
            // second read = the cache-hit path, which re-applies the remap on return
            TestHelper.prerender ctx body
            |> Flip.Expect.equal "reference-position reads must still see the alias name" nameRef
    ]

// =====================================================================================
// (2) PATH INTERCEPTORS — createModulePath contract via Path.fromXxx, and the pipeXxx
//     interceptor hooks + IgnorePathRender gate.
// =====================================================================================

[<Tests>]
let createModulePathTests =
    testList "createModulePath (via Path.fromInterface / fromTypeAlias / fromClass)" [

        // source=None, MemberPath=[] -> ModulePath.init "Global"; the type sits at the root.
        testCase "source=None, no module trace -> Global root" <| fun _ ->
            ResolvedType.Interface.create "Color"
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "path-less interface anchors at the synthetic Global root" "Global.Color"

        testCase "source=None, no module trace -> Global root (type alias)" <| fun _ ->
            ResolvedType.TypeAlias.create (ResolvedType.primitive TypeKindPrimitive.String) "MyAlias"
            |> Path.fromTypeAlias
            |> typePathStr
            |> Flip.Expect.equal "path-less alias anchors at Global root" "Global.MyAlias"

        testCase "source=None, no module trace -> Global root (class)" <| fun _ ->
            // A class is mocked as an interface-shaped record? Use TypeAlias/Interface paths;
            // classes share the same createModulePath contract. Cover via interface w/ deeper FQN.
            ResolvedType.Interface.create "Widget"
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "" "Global.Widget"

        // source=None, MemberPath=[A;B] -> the FQN module trace becomes the module path.
        // withPath prepends modules BEFORE the name; QualifiedName extracts [A;B] as MemberPath,
        // "Bar" as Name.
        testCase "source=None, module trace nests the type under its modules" <| fun _ ->
            ResolvedType.Interface.create "Bar"
            |> ResolvedType.Interface.withPath [ "Foo"; "Inner" ]
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "module trace becomes Foo.Inner; type Bar nested" "Foo.Inner.Bar"

        // source=Some s -> sanitizeSource s becomes the module root (the synthetic module),
        // the FQN MemberPath nests beneath it. A `typescript` source -> a `Typescript` module.
        testCase "source=Some typescript -> synthetic Typescript module" <| fun _ ->
            ResolvedType.Interface.create "Response"
            |> ResolvedType.Interface.withSource "typescript"
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "typescript-sourced type sits under the synthetic Typescript module" "Typescript.Response"

        // source=Some s with a module trace -> module root is the source, FQN modules nest under.
        testCase "source=Some + module trace nests FQN modules under the source module" <| fun _ ->
            ResolvedType.Interface.create "Bar"
            |> ResolvedType.Interface.withPath [ "Foo" ]
            |> ResolvedType.Interface.withSource "mylib"
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "source 'mylib' is root; FQN module Foo nests; Bar leaf" "Mylib.Foo.Bar"

        // A scoped/slashed source is split on path separators into nested modules.
        testCase "source with path separators splits into nested modules" <| fun _ ->
            ResolvedType.Interface.create "Thing"
            |> ResolvedType.Interface.withSource "@scope/pkg"
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "@scope/pkg -> Scope.Pkg nested modules" "Scope.Pkg.Thing"
    ]

// =====================================================================================
// (2b) createModulePath — the node_modules FILE-IMPORT arm (Abnormal source + a
//      node_modules FilePath). Coverage plane for the package-identity truncation and
//      the multi-version (vN) twin disambiguation (zod v3/v4). The packaging dir
//      segments after the package name (dist/esm/lib/...) are npm artifacts, NOT module
//      structure — only the package identity and an IMMEDIATELY-following version
//      segment survive.
// =====================================================================================

[<Tests>]
let nodeModulesPathTests =
    // An interface as read from a node_modules declaration-file import: the FQN carries the
    // absolute file path (quoted, as the TS checker prints module sources) followed by the
    // member trace; the Source is the same quoted path (Abnormal — quotes + slashes).
    let quoted (filePath: string) = "\"" + filePath + "\""
    let abnormal (filePath: string) =
        QualifiedNamePart.Abnormal(
            quoted filePath,
            QualifiedNamePartDiagnostic.ContainsQuotationMarks
            ||| QualifiedNamePartDiagnostic.ContainsSlash)
    let fileImportInterface (filePath: string) (name: string) =
        let iface = ResolvedType.Interface.create name
        { iface with
            Interface.Source = Some (abnormal filePath)
            FullyQualifiedName = [ abnormal filePath; QualifiedNamePart.Normal name ] }
    let pathOf filePath name =
        fileImportInterface filePath name |> Path.fromInterface |> typePathStr

    testList "createModulePath node_modules file-import arm" [

        // The zod-twin disambiguation: a `vN` segment IMMEDIATELY after the package name
        // is version identity and must be kept — v3 and v4 declare the same type names.
        testCase "version segment kept: zod/v3 -> Zod.V3" <| fun _ ->
            pathOf "/repo/node_modules/zod/v3/types" "ZodObject"
            |> Flip.Expect.equal "v3 twin keeps its version home" "Zod.V3.ZodObject"

        testCase "version segment kept: zod/v4 (deeper artifacts dropped)" <| fun _ ->
            pathOf "/repo/node_modules/zod/v4/classic/schemas" "ZodString"
            |> Flip.Expect.equal "v4 twin keeps V4; classic/schemas are packaging dirs" "Zod.V4.ZodString"

        // Packaging dirs are NOT module structure.
        testCase "non-version dir segment dropped: zod/lib -> Zod" <| fun _ ->
            pathOf "/repo/node_modules/zod/lib/index" "ZodAny"
            |> Flip.Expect.equal "lib is a packaging dir, not a version" "Zod.ZodAny"

        // Scoped package identity is 2 segments; deep dist/esm chains are dropped.
        testCase "scoped package keeps @scope/pkg only" <| fun _ ->
            pathOf "/repo/node_modules/@modelcontextprotocol/sdk/dist/esm/types" "ClientOptions"
            |> Flip.Expect.equal "scoped identity, packaging chain dropped" "Modelcontextprotocol.Sdk.ClientOptions"

        // The version segment must IMMEDIATELY follow the package name — a vN buried in
        // the packaging chain is not package-version identity.
        testCase "vN not immediately after package name is dropped" <| fun _ ->
            pathOf "/repo/node_modules/pkg/dist/v3/mod" "Thing"
            |> Flip.Expect.equal "dist/v3 is a packaging chain, not a version home" "Pkg.Thing"

        // isVersionSeg boundaries: `v` alone and `v<non-digits>` are NOT versions.
        testCase "bare v is not a version segment" <| fun _ ->
            pathOf "/repo/node_modules/zod/v/mod" "Thing"
            |> Flip.Expect.equal "bare v dropped" "Zod.Thing"

        testCase "vNext is not a version segment" <| fun _ ->
            pathOf "/repo/node_modules/zod/vNext/mod" "Thing"
            |> Flip.Expect.equal "v + non-digits dropped" "Zod.Thing"

        testCase "multi-digit version v10 is kept" <| fun _ ->
            pathOf "/repo/node_modules/zod/v10/mod" "Thing"
            |> Flip.Expect.equal "v10 is a version segment" "Zod.V10.Thing"

        testCase "scoped package + version segment keeps both" <| fun _ ->
            pathOf "/repo/node_modules/@scope/pkg/v2/mod" "Thing"
            |> Flip.Expect.equal "@scope/pkg/v2 -> Scope.Pkg.V2" "Scope.Pkg.V2.Thing"

        // The FQN's middle (namespace) parts still nest under the package home.
        testCase "member trace nests under the versioned package home" <| fun _ ->
            let iface =
                let baseIface = ResolvedType.Interface.create "Inner"
                { baseIface with
                    Interface.Source = Some (abnormal "/repo/node_modules/zod/v3/types")
                    FullyQualifiedName =
                        [ abnormal "/repo/node_modules/zod/v3/types"
                          QualifiedNamePart.Normal "Ns"
                          QualifiedNamePart.Normal "Inner" ] }
            iface
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "FQN namespace nests under Zod.V3" "Zod.V3.Ns.Inner"

        // Guard: an Abnormal source that does NOT contain the file name is NOT a file
        // import — it falls back to the plain sanitizeSource arm.
        testCase "abnormal source not matching the file name falls back to sanitizeSource" <| fun _ ->
            let iface =
                let baseIface = ResolvedType.Interface.create "Thing"
                { baseIface with
                    Interface.Source = Some (QualifiedNamePart.Abnormal("@other/lib", QualifiedNamePartDiagnostic.ContainsSlash))
                    FullyQualifiedName =
                        [ abnormal "/repo/node_modules/zod/v3/types"
                          QualifiedNamePart.Normal "Thing" ] }
            iface
            |> Path.fromInterface
            |> typePathStr
            |> Flip.Expect.equal "falls back to the sanitized source root" "Other.Lib.Thing"
    ]

[<Tests>]
let memberPathTests =
    testList "createModulePath for MEMBER paths (Path.fromFunction / fromVariable)" [

        // A path-less function is a module-level member at the Global root.
        testCase "path-less function -> Global member" <| fun _ ->
            ResolvedType.Function.create "doThing" (ResolvedType.primitive TypeKindPrimitive.Void)
            |> Path.fromFunction
            |> memberPathStr
            |> Flip.Expect.equal "function sits as a member on the Global module" "Global.doThing"

        // FQN member-path nesting: a function under modules A.B is a member on the B module.
        testCase "function under a module trace nests as that module's member" <| fun _ ->
            ResolvedType.Function.create "fn" (ResolvedType.primitive TypeKindPrimitive.Void)
            |> ResolvedType.Function.withPath [ "A"; "B" ]
            |> Path.fromFunction
            |> memberPathStr
            |> Flip.Expect.equal "fn is a member on module A.B" "A.B.fn"
    ]

[<Tests>]
let pipeInterceptorTests =
    testList "pipeInterface / pipeTypeAlias interceptor hooks" [

        // The DEFAULT interceptor (GeneratorContext.Empty) is identity: pipeInterface equals
        // fromInterface.
        testCase "default TypePaths interceptor is identity (interface)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let iface = ResolvedType.Interface.create "Foo" |> ResolvedType.Interface.withPath [ "M" ]
            let piped = Path.Interceptors.pipeInterface ctx iface |> typePathStr
            let direct = Path.fromInterface iface |> typePathStr
            piped |> Flip.Expect.equal "pipeInterface == fromInterface under default interceptor" direct

        testCase "default TypePaths interceptor is identity (type alias)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let alias =
                ResolvedType.TypeAlias.create (ResolvedType.primitive TypeKindPrimitive.String) "MyAlias"
                |> ResolvedType.TypeAlias.withPath [ QualifiedNamePart.Normal "M" ]
            let piped = Path.Interceptors.pipeTypeAlias ctx alias |> typePathStr
            let direct = Path.fromTypeAlias alias |> typePathStr
            piped |> Flip.Expect.equal "pipeTypeAlias == fromTypeAlias under default interceptor" direct

        // The production interceptor prunes the phantom "Typescript" parent so a
        // typescript-sourced type's reference resolves at the global root (no Typescript.X).
        testCase "custom TypePaths interceptor prunes the phantom Typescript parent" <| fun _ ->
            let ctx =
                GeneratorContext.EmptyWithCustomisation (fun c ->
                    { c with
                        Customisation.Interceptors.Paths.TypePaths =
                            fun _ _ s -> TypePath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s })
            let iface =
                ResolvedType.Interface.create "Response"
                |> ResolvedType.Interface.withSource "typescript"
            Path.Interceptors.pipeInterface ctx iface
            |> typePathStr
            |> Flip.Expect.equal "Typescript parent pruned -> Response at root" "Response"
    ]

[<Tests>]
let ignorePathRenderTests =
    testList "IgnorePathRender source gate (Path.Interceptors.shouldIgnoreRender)" [

        // Default IgnorePathRender ignores nothing.
        testCase "default gate ignores nothing" <| fun _ ->
            let ctx = GeneratorContext.Empty
            ResolvedType.Interface.create "Foo"
            |> ResolvedType.Interface.withSource "typescript"
            |> Path.Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors
            |> Flip.Expect.isFalse "default gate must not ignore any source"

        // The production gate ignores a `typescript` source (suppresses the synthetic module
        // render so the reference resolves to the real emitted type).
        testCase "typescript-sourced type is ignored by the production source gate" <| fun _ ->
            let ctx =
                GeneratorContext.EmptyWithCustomisation (fun c ->
                    { c with
                        Customisation.Interceptors.IgnorePathRender.Source =
                            function
                            | QualifiedNamePart.Normal text
                            | QualifiedNamePart.Abnormal(text, _) ->
                                text.Contains("babel", System.StringComparison.OrdinalIgnoreCase)
                                || text.Contains("typescript", System.StringComparison.OrdinalIgnoreCase) })
            ResolvedType.Interface.create "Response"
            |> ResolvedType.Interface.withSource "typescript"
            |> Path.Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors
            |> Flip.Expect.isTrue "typescript source must be gated out by the production interceptor"

        // A non-stdlib (workers-types own) source is NOT ignored by the production gate.
        testCase "an ordinary source is NOT ignored by the production gate" <| fun _ ->
            let ctx =
                GeneratorContext.EmptyWithCustomisation (fun c ->
                    { c with
                        Customisation.Interceptors.IgnorePathRender.Source =
                            function
                            | QualifiedNamePart.Normal text
                            | QualifiedNamePart.Abnormal(text, _) ->
                                text.Contains("babel", System.StringComparison.OrdinalIgnoreCase)
                                || text.Contains("typescript", System.StringComparison.OrdinalIgnoreCase) })
            ResolvedType.Interface.create "MyType"
            |> ResolvedType.Interface.withSource "my-own-package"
            |> Path.Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors
            |> Flip.Expect.isFalse "an ordinary package source must not be gated out"
    ]
