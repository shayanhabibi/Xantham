module Xantham.Generator.Shape.Callbacks

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The shape of a named callback: the parameters it was declared with, the reference the arity
/// rule writes for it, and the findings both produce. The self-name lookup is skipped, because it
/// would return the declaration being written.
let private delegateShapeFor
    (ctx: Context)
    (model: ShapeModel)
    (name: string)
    (facts: TypeFacts)
    : FsParam list * FsTypeRef * Finding list =
    match facts.CallSignatures with
    | [] -> [], FsObj, [ Finding.make name TypeReference.CallableWithoutSignatures ]
    | signature :: rest ->
        let overloadFindings =
            if rest.IsEmpty then
                []
            elif callSignaturesSeparable model facts then
                [
                    Finding.make name (TypeReference.CallbackOverloadsFromFirst(rest.Length + 1))
                ]
            else
                [
                    Finding.make name (TypeReference.CallbackOverloadsNotSeparable(rest.Length + 1))
                ]

        // The signature's own parameters are discarded here rather than written: a delegate
        // type has nowhere to put them. `aliasTypeParams` has already hoisted them onto the
        // alias around this callback, with the rank-2 finding that records the cost.
        let _, parameters, returns, signatureFindings =
            shapeSignature ctx model None name signature

        let parameterTypes = parameters |> List.map _.Type
        let reference, callbackFindings = callbackRef name parameterTypes returns
        parameters, reference, overloadFindings @ signatureFindings @ callbackFindings

/// A declaration for every named pure-callback type. One the arity rule retains is a named
/// delegate carrying the parameter names TypeScript spelled - `type TickHandler = delegate of x:
/// float * y: float -> string` (D5); one that converts is an abbreviation of the F# function type
/// - `type Formatter = (float -> string)` (D5a).
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
                                let parameters, reference, refFindings =
                                    delegateShapeFor ctx { model with TypeVars = scope } name facts

                                findings <- findings @ parameterFindings @ refFindings

                                let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                                match reference with
                                | FsDelegate(arguments, returns) ->
                                    Some(
                                        FsDelegateType
                                            {
                                                Name = name
                                                Docs = ""
                                                Tags = []
                                                Order = order
                                                TypeParameters = typeParameters
                                                Parameters =
                                                    List.zip (parameters |> List.map _.Name) arguments
                                                    |> List.map (fun (parameterName, argument) ->
                                                        {
                                                            FsDelegateParam.Name = parameterName
                                                            Type = argument
                                                        })
                                                Return = returns
                                            }
                                    )
                                | _ ->
                                    Some(
                                        FsAbbrev
                                            {
                                                Name = name
                                                Docs = ""
                                                Tags = []
                                                Order = order
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
