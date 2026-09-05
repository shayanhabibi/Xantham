module Xantham.Generator.Shape.ParamObjects

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Parameters beyond this stop being construction ergonomics: a Create this wide is unusable
/// at a call site, and each one is quadratic work for the F# typechecker.
[<Literal>]
let private CreateParameterBudget = 24

/// The closed vocabulary `SP003` reports, so a corpus aggregates by reason.
module private Reason =
    [<Literal>]
    let NoMembers = "the declaration has no members of its own"

    [<Literal>]
    let IndexSignature = "an index signature has no name to bind a parameter to"

    [<Literal>]
    let OverloadedMethod = "an overloaded method would bind two parameters of one name"

    [<Literal>]
    let OverBudget = "more members than the Create parameter budget"

    [<Literal>]
    let EntrypointClass =
        "the declaration is a class a consumer inherits, where `new` is the construction"

let private isConstructor =
    function
    | FsConstructor _ -> true
    | _ -> false

let private isIndexer =
    function
    | FsIndexer _ -> true
    | _ -> false

/// Why the declaration carries no `Create`, or `None` where one is synthesized.
let private refusal (decl: FsInterfaceDecl) =
    let methodNames =
        decl.Members
        |> List.choose (function
            | FsMethod m -> Some m.Name
            | _ -> None)

    if decl.Entrypoint.IsSome then
        Some Reason.EntrypointClass
    elif List.isEmpty decl.Members then
        Some Reason.NoMembers
    elif decl.Members |> List.exists isIndexer then
        Some Reason.IndexSignature
    elif (List.distinct methodNames).Length <> methodNames.Length then
        Some Reason.OverloadedMethod
    elif decl.Members.Length > CreateParameterBudget then
        Some Reason.OverBudget
    else
        None

/// The `Create` parameter a member binds. A method binds the callback that a function-valued
/// property of the same signature carries (D5), and binds it as a required parameter.
let private parameterFor (owner: string) (m: FsMember) : FsParam * Finding list =
    match m with
    | FsProperty p ->
        {
            Name = p.Name
            Optional =
                match p.Type with
                | FsOption _ -> true
                | _ -> false
            Rest = false
            Type = p.Type
        },
        []
    | FsMethod method' ->
        let reference, findings =
            callbackRef $"{owner}.{method'.Name}" (method'.Parameters |> List.map _.Type) method'.Return

        {
            Name = method'.Name
            Optional = false
            Rest = false
            Type = reference
        },
        findings
    | FsIndexer _
    | FsConstructor _ -> failwith "unreachable: refused above"

/// Construction ergonomics (D3, §4.4): an interface gains a `[<ParamObject; Emit("$0")>]`
/// Create overload mirroring its members, required members first, so consumers never hand-build
/// objects. A method member reads as a delegate-typed parameter (§3 of the Fable 5 workarounds
/// document): the delegate receives no `this`.
let synthesizeParamObjects: Pass<ShapeModel> =
    {
        Name = "synthesize-paramobjects"
        Run =
            fun _ model ->
                async {
                    let mutable findings = []

                    let decls =
                        model.Decls
                        |> List.map (function
                            // The `Create` members of a constructor object come from its
                            // construct signatures (§4.4).
                            | FsInterface decl when not (decl.Members |> List.exists isConstructor) ->
                                match refusal decl with
                                | Some reason ->
                                    findings <-
                                        findings
                                        @ [
                                            Finding.make decl.Name (SynthesizeParamObjects.CreateNotSynthesized reason)
                                        ]

                                    FsInterface decl
                                | None ->
                                    let parameters, callbacks =
                                        decl.Members |> List.map (parameterFor decl.Name) |> List.unzip

                                    let required, optional = parameters |> List.partition (fun p -> not p.Optional)

                                    let carried =
                                        decl.Members
                                        |> List.choose (function
                                            | FsMethod method' ->
                                                Some(
                                                    Finding.make
                                                        $"{decl.Name}.{method'.Name}"
                                                        SynthesizeParamObjects.MethodMemberAsCreateParameter
                                                )
                                            | _ -> None)

                                    findings <-
                                        findings
                                        @ [ Finding.make decl.Name SynthesizeParamObjects.ParamObjectSynthesized ]
                                        @ carried
                                        @ List.concat callbacks

                                    FsInterface
                                        { decl with
                                            CreateOverloads = [ required @ optional ]
                                        }
                            | decl -> decl)

                    let model = { model with Decls = decls }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
