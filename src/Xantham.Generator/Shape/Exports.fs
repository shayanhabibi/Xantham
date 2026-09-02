module Xantham.Generator.Shape.Exports

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// `Exports` members from the value exports that are not classes: functions (every overload
/// emitted), and values - `const`/`let` and namespace objects - as get-only properties.
let shapeExports: Pass<ShapeModel> =
    {
        Name = "shape-exports"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let emit finding = findings <- findings @ [ finding ]

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
                                        })
                                | Some facts ->
                                    let reference, refFindings = typeRef ctx model None name facts.Response.Id
                                    findings <- findings @ refFindings

                                    [
                                        index,
                                        {
                                            Name = name
                                            Docs = export.Docs
                                            Tags = export.Tags
                                            TypeParameters = []
                                            Binding = binding
                                            Body = ExportValue reference
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
