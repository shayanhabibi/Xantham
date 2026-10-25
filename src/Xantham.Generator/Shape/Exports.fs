module Xantham.Generator.Shape.Exports

open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec
open Xantham.Generator.Shape.ExportLayout

/// The object every ambient global is a property of, and the selector a settable global binds
/// through.
let private globalObject = "globalThis"


/// Source-ordered calls with their original expanded ordinals. Identical nongeneric inputs
/// select the first declaration's return in TypeScript.
let private sourceCallSignatures (model: ShapeModel) symbolFlags (facts: TypeFacts) =
    let mutable seen = Set.empty

    facts.CallSignatures
    |> List.collect (fun signature ->
        let eligible =
            hasAny SymbolFlags.Function symbolFlags
            && signature.Source.IsSome
            && signature.TypeParameters.IsEmpty
            && not signature.HasRest
            && not signature.HasThisParameter

        let key =
            signature.Parameters
            |> List.map (fun parameter -> parameter.TypeId, parameter.Optional)

        let retained = not eligible || not (Set.contains key seen)

        if eligible then
            seen <- Set.add key seen

        restOverloads model signature |> List.map (fun expanded -> retained, expanded))
    |> List.indexed
    |> List.choose (fun (ordinal, (retained, signature)) -> if retained then Some(ordinal, signature) else None)

/// `Exports` members from the value exports that are not classes: functions (every reachable overload
/// emitted), and values - `const`/`let`/`var` and namespace objects - as properties, settable
/// where the value is a mutable global.
let shapeExports: Pass<ShapeModel> =
    {
        Name = "shape-exports"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let emit finding = findings <- findings @ [ finding ]

                    // Once per run, not once per import: `@types/three` renders 737 of them and
                    // they all name the same specifier, so a per-import finding would say one
                    // thing 737 times. `<module>` is the run-level symbol `harvest-globals`
                    // already uses. Raised only where the runtime package was *derived* - a
                    // configured `runtime` is a decision someone made and recorded, not a
                    // convention this run had to guess from a name.
                    let runtimePackage = GeneratorConfig.runtimePackage ctx.Config ctx.PackageName

                    if
                        ctx.Config.RuntimePackage.IsNone
                        && runtimePackage / uom<importSpecifier> <> ctx.PackageName / uom<npmDependency>
                    then
                        emit (
                            Finding.make
                                "<module>"
                                (ShapeExports.RuntimeSpecifierDerived(runtimePackage / uom<importSpecifier>))
                        )

                    let fallback = defaultExportName ctx

                    // A member's findings carry the name its manifest row will: the container
                    // that binds it, then its F# name.
                    let containers = ExportLayout.containers model

                    let members =
                        model.Harvest.Exports
                        |> List.indexed
                        |> List.collect (fun (index, export) ->
                            if not export.HasValueExport || hasAny SymbolFlags.Class export.Symbol.Flags then
                                []
                            else
                                let name = fsName fallback export
                                let owner = ownerOf runtimePackage export.Origin
                                let findingName = $"{containers[owner]}.{name}"

                                let binding = bindingOf export

                                let valueFacts =
                                    Map.tryFind export.Symbol.SymbolId model.ExportTypes
                                    |> Option.bind _.Value
                                    |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                                match valueFacts with
                                | None ->
                                    emit (Finding.make findingName ShapeExports.NoValueType)
                                    []
                                // A function's value is a method only where its call signatures
                                // are all it carries; one merged with a namespace holding values
                                // is a hybrid object, bound whole and called through `Invoke`.
                                // A polymorphic callable value also stays whole: turning it
                                // into a method prevents passing its original interface value.
                                | Some facts when
                                    not facts.CallSignatures.IsEmpty
                                    && facts.Members.IsEmpty
                                    && facts.IndexInfos.IsEmpty
                                    && facts.ConstructSignatures.IsEmpty
                                    && not (isPolymorphicCallableValue model export.Symbol.Flags facts)
                                    ->
                                    sourceCallSignatures model export.Symbol.Flags facts
                                    |> List.map (fun (ordinal, signature) ->
                                        let typeParameters, parameters, returns, signatureFindings =
                                            shapeSignature ctx model None findingName signature

                                        findings <- findings @ signatureFindings

                                        {
                                            OwnedExportMember.Owner = owner
                                            HarvestIndex = index
                                            ExportName = export.ExportName
                                            SourceSymbolId = export.Symbol.SymbolId
                                            SignatureOrdinal = Some ordinal
                                            Member =
                                                {
                                                    Name = name
                                                    Docs = export.Docs
                                                    Tags = signatureTags export.Tags signature
                                                    TypeParameters = typeParameters
                                                    Binding = binding
                                                    Body = ExportFunction(parameters, returns)
                                                    Settable = false
                                                }
                                        })
                                | Some facts ->
                                    let reference, refFindings =
                                        typeRef
                                            ctx
                                            (selectAnyReturn ctx model export.ExportName findingName)
                                            None
                                            findingName
                                            facts.Response.TypeId

                                    findings <- findings @ refFindings

                                    // A `var` on the global object is the one binding an
                                    // assignment reaches: it is a writable property of
                                    // `globalThis`, where a module's exports stay immutable to
                                    // an importer whatever the exporter declared them.
                                    let mutableValue = hasAny SymbolFlags.FunctionScopedVariable export.Symbol.Flags

                                    let settable =
                                        match binding with
                                        | GlobalName _ -> mutableValue
                                        | ImportDefault
                                        | ImportFrom _
                                        | ImportKeyed _
                                        | ImportNamed _ -> false

                                    if mutableValue && not settable then
                                        emit (Finding.make findingName ShapeExports.MutableValueReadOnly)

                                    [
                                        {
                                            OwnedExportMember.Owner = owner
                                            HarvestIndex = index
                                            ExportName = export.ExportName
                                            SourceSymbolId = export.Symbol.SymbolId
                                            SignatureOrdinal = None
                                            Member =
                                                {
                                                    Name = name
                                                    Docs = export.Docs
                                                    Tags = export.Tags
                                                    TypeParameters = []
                                                    Binding = (if settable then GlobalName globalObject else binding)
                                                    Body = ExportValue reference
                                                    Settable = settable
                                                }
                                        }
                                    ])

                    let model =
                        { model with
                            ExportMembers = model.ExportMembers @ members
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
