module Xantham.Generator.Shape.Callbacks

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The delegate shape of a named callback, without the self-name lookup that would just return
/// the abbreviation being defined.
let private delegateRefFor
    (ctx: Context)
    (model: ShapeModel)
    (name: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    match facts.CallSignatures with
    | [] -> FsObj, [ Finding.make name TypeReference.CallableWithoutSignatures ]
    | signature :: rest ->
        let overloadFindings =
            if rest.IsEmpty then
                []
            else
                [
                    Finding.make name (TypeReference.CallbackOverloadsFromFirst(rest.Length + 1))
                ]

        // The signature's own parameters are discarded here rather than written: a delegate
        // type has nowhere to put them. `aliasTypeParams` has already hoisted them onto the
        // alias around this callback, with the rank-2 finding that records the cost.
        let _, parameters, returns, signatureFindings =
            shapeSignature ctx model None name signature

        let parameterTypes = parameters |> List.map _.Type
        FsDelegate(parameterTypes, returns), overloadFindings @ signatureFindings

/// Abbreviations for named pure-callback types: `type TimerCallback = Action<Timer>` (D5).
let shapeCallbacks: Pass<ShapeModel> =
    {
        Name = "shape-callbacks"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let decls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when flag TypeFlags.Object facts && isPureCallback facts ->
                                let typeParameters, scope, parameterFindings = aliasTypeParams ctx model name facts

                                // The signature is read under the alias's own parameters, so
                                // `Callback<T> = (self: T) => void` writes `'T` rather than widening it.
                                let reference, refFindings =
                                    delegateRefFor ctx { model with TypeVars = scope } name facts

                                findings <- findings @ parameterFindings @ refFindings

                                Some(
                                    FsAbbrev
                                        {
                                            Name = name
                                            Docs = ""
                                            Tags = []
                                            Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                            TypeParameters = typeParameters
                                            Target = reference
                                        }
                                )
                            | _ -> None)

                    let model =
                        { model with
                            Decls = model.Decls @ decls
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
