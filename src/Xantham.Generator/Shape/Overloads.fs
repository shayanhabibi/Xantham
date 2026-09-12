module Xantham.Generator.Shape.Overloads

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The single-case `[<StringEnum>]` a retained literal is written as: one case, compiled to the
/// literal, so `Store.Text.Text` reaches JavaScript as `"text"`. Paired with a finding where the
/// case collides with a reserved F# name and keeps `RequireQualifiedAccess` (LU002).
let private literalDecl (name: string, text: string, order: DeclOrder option) =
    let case = Naming.enumCaseOfString text

    let decl =
        FsStringEnum
            {
                Name = name
                Docs = ""
                Tags = []
                Order = order
                Cases =
                    [
                        {
                            Name = case
                            CompiledName = (if text = case then None else Some text)
                            CompiledValue = None
                        }
                    ]
            }

    let finding =
        if Set.contains case reservedCaseNames then
            Some(Finding.make name (ClassifyLiteralUnions.QualifiedAccessKept case))
        else
            None

    decl, finding

/// Overloads that widened into the same F# signature are duplicates the compiler rejects -
/// .NET overload resolution sees through type abbreviations and ignores return types. The
/// first survives; the rest drop with a finding.
///
/// A literal-typed parameter keeps its literal as a type of its own where that is what separates
/// an overload set (`Spec.literalOverloads`), so those signatures arrive here distinct. The types
/// they read are declared beside them.
///
/// Retention reads the members of a declaration, so an exported function's overloads arrive
/// widened and its drops report `DO004` apart from `DO001`.
///
/// A set separated in TypeScript by a `keyof` bound alone reaches F# as one signature, since
/// .NET keeps constraints out of a method signature (`Spec.keyBoundedOverloads`). Those drops
/// report `DO005`.
let dedupeOverloads: Pass<ShapeModel> =
    {
        Name = "dedupe-overloads"
        Run =
            fun _ model ->
                async {
                    let separated = literalOverloads model

                    let literalDecls, literalFindings =
                        separated
                        |> List.collect _.Declared
                        |> List.distinctBy (fun (name, _, _) -> name)
                        |> List.map literalDecl
                        |> List.unzip

                    let mutable findings =
                        (separated
                         |> List.map (fun set ->
                             Finding.make set.Member (DedupeOverloads.OverloadsDistinguishedByLiteral set.Parameter)))
                        @ List.choose id literalFindings

                    let abbrevs =
                        model.Decls
                        |> List.choose (function
                            | FsAbbrev decl -> Some(decl.Name, decl.Target)
                            | _ -> None)
                        |> Map.ofList

                    /// The reference with abbreviations expanded, so `TargetsParam` and
                    /// `DOMTargetsParam` (both `obj`) compare equal the way the compiler sees them.
                    let rec normalize (visited: Set<string>) (reference: FsTypeRef) : FsTypeRef =
                        match reference with
                        | FsNamed name when Map.containsKey name abbrevs && not (Set.contains name visited) ->
                            normalize (Set.add name visited) abbrevs[name]
                        | FsOption inner -> FsOption(normalize visited inner)
                        | FsArray element -> FsArray(normalize visited element)
                        | FsDelegate(args, ret) ->
                            FsDelegate(args |> List.map (normalize visited), normalize visited ret)
                        | FsFunc(argument, ret) -> FsFunc(normalize visited argument, normalize visited ret)
                        | other -> other

                    /// A reference with its own signature's type variables renamed by declaration
                    /// order, so `<A extends T>(value: A): A` and `<B extends T>(value: B): B`
                    /// compare equal once their dropped constraints leave both as `'T0 -> 'T0` -
                    /// .NET overload resolution does not see a type parameter's name.
                    let rec renameTypeVars (rename: Map<string, string>) (reference: FsTypeRef) : FsTypeRef =
                        let recur = renameTypeVars rename

                        match reference with
                        | FsTypeVar name -> FsTypeVar(rename |> Map.tryFind name |> Option.defaultValue name)
                        | FsOption inner -> FsOption(recur inner)
                        | FsArray element -> FsArray(recur element)
                        | FsTuple components -> FsTuple(List.map recur components)
                        | FsErasedUnion arms -> FsErasedUnion(List.map recur arms)
                        | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
                        | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
                        | FsApp(name, args) -> FsApp(name, List.map recur args)
                        | FsBranded(primitive, measure) -> FsBranded(recur primitive, measure)
                        | other -> other

                    let signatureKey (typeParameters: FsTypeParam list) (parameters: FsParam list) =
                        let rename = typeParameters |> List.mapi (fun i p -> p.Name, $"T{i}") |> Map.ofList

                        parameters
                        |> List.map (fun p -> p.Optional, p.Rest, normalize Set.empty (renameTypeVars rename p.Type))

                    let keyBounded = keyBoundedOverloads model

                    /// The parameter a dropped overload was separated at in TypeScript alone:
                    /// one taking a type parameter its key-set bound was erased from.
                    let keyErasedParameter (owner: string) (m: FsMethodMember) =
                        if not (Set.contains $"{owner}.{m.Name}" keyBounded) then
                            None
                        else
                            let bare =
                                m.TypeParameters
                                |> List.filter (fun p -> p.Constraint.IsNone)
                                |> List.map (fun p -> FsTypeVar p.Name)
                                |> Set.ofList

                            m.Parameters
                            |> List.tryFind (fun p -> Set.contains p.Type bare)
                            |> Option.map _.Name

                    let dedupeMethods (owner: string) (members: FsMember list) =
                        let mutable seen = Set.empty

                        members
                        |> List.filter (function
                            | FsProperty _ -> true
                            // Two `Item` overloads differing only in key type are legal and
                            // wanted - a type may index by both string and number.
                            | FsIndexer _ -> true
                            | FsConstructor c ->
                                // `Create` overloads collide the same way methods do, and share
                                // their namespace: a static side with both `new (url: string)`
                                // and a `Create(url: string)` property would be one clash.
                                let key = ("Create", signatureKey c.TypeParameters c.Parameters).ToString()

                                if Set.contains key seen then
                                    findings <-
                                        findings @ [ Finding.make $"{owner}.Create" DedupeOverloads.OverloadDropped ]

                                    false
                                else
                                    seen <- Set.add key seen
                                    true
                            | FsMethod m ->
                                let key = (m.Name, signatureKey m.TypeParameters m.Parameters).ToString()

                                if Set.contains key seen then
                                    let dropped =
                                        match keyErasedParameter owner m with
                                        | Some parameter -> DedupeOverloads.KeyofConstrainedOverloadDropped parameter
                                        | None -> DedupeOverloads.OverloadDropped

                                    findings <- findings @ [ Finding.make $"{owner}.{m.Name}" dropped ]

                                    false
                                else
                                    seen <- Set.add key seen
                                    true
                            | FsInvoke c ->
                                // `Invoke` overloads collide the same way `Create` overloads do: two
                                // call signatures that widen to the same F# parameter types are one
                                // .NET member, not two.
                                let key = ("Invoke", signatureKey c.TypeParameters c.Parameters).ToString()

                                if Set.contains key seen then
                                    findings <-
                                        findings @ [ Finding.make $"{owner}.Invoke" DedupeOverloads.OverloadDropped ]

                                    false
                                else
                                    seen <- Set.add key seen
                                    true)

                    let decls =
                        model.Decls
                        |> List.map (function
                            | FsInterface decl ->
                                FsInterface
                                    { decl with
                                        Members = dedupeMethods decl.Name decl.Members
                                    }
                            | decl -> decl)

                    let model =
                        { model with
                            Decls = decls @ literalDecls
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
