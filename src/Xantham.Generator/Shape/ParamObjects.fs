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

    [<Literal>]
    let SymbolKeyed =
        "a required well-known symbol member has no key a ParamObject literal can write"

let private isConstructor =
    function
    | FsConstructor _ -> true
    | _ -> false

let private isIndexer =
    function
    | FsIndexer _ -> true
    | _ -> false

let private isInvoke =
    function
    | FsInvoke _ -> true
    | _ -> false

let private isSymbolKeyed' =
    function
    | FsSymbolKeyed _ -> true
    | _ -> false

let private isLiteralKeyed' =
    function
    | FsLiteralKeyed _ -> true
    | _ -> false

/// The JavaScript key and value of a literal member.
let private literalMember =
    function
    | FsProperty p -> p.Literal |> Option.map (fun value -> p.Name, value)
    | _ -> None

let private isLiteralMember (m: FsMember) = (literalMember m).IsSome

/// Why the declaration carries no `Create`, or `None` where one is synthesized. `members` is the
/// property/method set the `Create` binds - a hybrid's `Invoke` reads its own call signatures
/// and carries no `Create` parameter of its own.
let private refusal (decl: FsInterfaceDecl) (members: FsMember list) =
    let methodNames =
        members
        |> List.choose (function
            | FsMethod m -> Some m.Name
            | _ -> None)

    if decl.Entrypoint.IsSome then
        Some Reason.EntrypointClass
    elif
        decl.Members |> List.exists isSymbolKeyed'
        || decl.Inherits |> List.exists (sequenceElement >> Option.isSome)
    then
        Some Reason.SymbolKeyed
    elif List.isEmpty members then
        Some Reason.NoMembers
    elif members |> List.exists isIndexer then
        Some Reason.IndexSignature
    // A literal-keyed overload is one more overload of its JavaScript key.
    elif
        (List.distinct methodNames).Length <> methodNames.Length
        || members |> List.exists isLiteralKeyed'
    then
        Some Reason.OverloadedMethod
    elif (members |> List.filter (isLiteralMember >> not)).Length > CreateParameterBudget then
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
    | FsConstructor _
    | FsInvoke _
    | FsSymbolKeyed _
    | FsLiteralKeyed _ -> failwith "unreachable: refused above, or excluded from the members Create binds"

/// Construction ergonomics (D3, §4.4): an interface gains a `[<ParamObject; Emit("$0")>]`
/// Create overload mirroring its members, required members first, so consumers never hand-build
/// objects. A method member reads as a delegate-typed parameter (§3 of the Fable 5 workarounds
/// document): the delegate receives no `this`. A literal member, inherited ones included, is
/// written by the Emit and binds no parameter.
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
                            // construct signatures (§4.4). An interface already carrying
                            // overloads of its own - the exclusive-arm fold's one `Create` per
                            // arm (wave fourteen item 4) - keeps them rather than being
                            // collapsed to the single overload this pass synthesizes.
                            | FsInterface decl when
                                not (decl.Members |> List.exists isConstructor)
                                && List.isEmpty decl.CreateOverloads
                                ->
                                // A hybrid's `Invoke` reads its own call signatures (§4.4); `Create`
                                // binds only the properties and methods beside it.
                                let members = decl.Members |> List.filter (isInvoke >> not)

                                match refusal decl members with
                                | Some reason ->
                                    findings <-
                                        findings
                                        @ [
                                            Finding.make decl.Name (SynthesizeParamObjects.CreateNotSynthesized reason)
                                        ]

                                    FsInterface decl
                                | None ->
                                    let literals = members |> List.choose literalMember
                                    let bound = members |> List.filter (isLiteralMember >> not)

                                    let parameters, callbacks = bound |> List.map (parameterFor decl.Name) |> List.unzip

                                    let required, optional = parameters |> List.partition (fun p -> not p.Optional)

                                    let carried =
                                        bound
                                        |> List.collect (function
                                            | FsMethod method' ->
                                                let symbol = $"{decl.Name}.{method'.Name}"

                                                let bound =
                                                    method'.TypeParameters
                                                    |> List.map (fun p -> $"'{p.Name}")
                                                    |> String.concat ", "

                                                [
                                                    Finding.make
                                                        symbol
                                                        SynthesizeParamObjects.MethodMemberAsCreateParameter
                                                    if not method'.TypeParameters.IsEmpty then
                                                        Finding.make
                                                            symbol
                                                            (SynthesizeParamObjects.MethodTypeParametersBoundAtCreate
                                                                bound)
                                                ]
                                            | _ -> [])

                                    let written =
                                        if literals.IsEmpty then
                                            []
                                        else
                                            [
                                                Finding.make
                                                    decl.Name
                                                    (SynthesizeParamObjects.LiteralMembersWrittenByCreate(
                                                        literals |> List.map fst |> String.concat ", "
                                                    ))
                                            ]

                                    findings <-
                                        findings
                                        @ [ Finding.make decl.Name SynthesizeParamObjects.ParamObjectSynthesized ]
                                        @ written
                                        @ carried
                                        @ List.concat callbacks

                                    FsInterface
                                        { decl with
                                            CreateOverloads =
                                                [
                                                    {
                                                        Parameters = required @ optional
                                                        Literals = literals
                                                    }
                                                ]
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
