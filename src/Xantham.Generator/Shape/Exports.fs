module Xantham.Generator.Shape.Exports

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The object every ambient global is a property of, and the selector a settable global binds
/// through.
let private globalObject = "globalThis"

/// `Exports` members from the value exports that are not classes: functions (every overload
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

                    if ctx.Config.RuntimePackage.IsNone && runtimePackage <> ctx.PackageName then
                        emit (Finding.make "<module>" (ShapeExports.RuntimeSpecifierDerived runtimePackage))

                    let fallback = defaultExportName ctx

                    let members =
                        model.Harvest.Exports
                        |> List.indexed
                        |> List.collect (fun (index, export) ->
                            if
                                not (hasAny SymbolFlags.Value export.Symbol.Flags)
                                || hasAny SymbolFlags.Class export.Symbol.Flags
                            then
                                []
                            else
                                let name = fsName fallback export

                                let binding = bindingOf export

                                let valueFacts =
                                    Map.tryFind export.Symbol.Id model.ExportTypes
                                    |> Option.bind _.Value
                                    |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                                match valueFacts with
                                | None ->
                                    emit (Finding.make name ShapeExports.NoValueType)
                                    []
                                | Some facts when not facts.CallSignatures.IsEmpty ->
                                    facts.CallSignatures
                                    |> List.map (fun signature ->
                                        let typeParameters, parameters, returns, signatureFindings =
                                            shapeSignature ctx model None name signature

                                        findings <- findings @ signatureFindings

                                        index,
                                        {
                                            Name = name
                                            Docs = export.Docs
                                            Tags = export.Tags
                                            TypeParameters = typeParameters
                                            Binding = binding
                                            Body = ExportFunction(parameters, returns)
                                            Settable = false
                                        })
                                | Some facts ->
                                    let reference, refFindings = typeRef ctx model None name facts.Response.Id
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
                                        | ImportNamed _ -> false

                                    if mutableValue && not settable then
                                        emit (Finding.make name ShapeExports.MutableValueReadOnly)

                                    [
                                        index,
                                        {
                                            Name = name
                                            Docs = export.Docs
                                            Tags = export.Tags
                                            TypeParameters = []
                                            Binding = (if settable then GlobalName globalObject else binding)
                                            Body = ExportValue reference
                                            Settable = settable
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
