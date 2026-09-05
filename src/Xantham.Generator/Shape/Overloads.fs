module Xantham.Generator.Shape.Overloads

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The single-case `[<StringEnum>]` a retained literal is written as: one case, compiled to the
/// literal, so `Store.Text.Text` reaches JavaScript as `"text"`.
let private literalDecl (name: string, text: string, order: DeclOrder option) =
    let case = Naming.enumCaseOfString text

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

/// Overloads that widened into the same F# signature are duplicates the compiler rejects -
/// .NET overload resolution sees through type abbreviations and ignores return types. The
/// first survives; the rest drop with a finding.
///
/// A literal-typed parameter keeps its literal as a type of its own where that is what separates
/// an overload set (`Spec.literalOverloads`), so those signatures arrive here distinct. The types
/// they read are declared beside them.
let dedupeOverloads: Pass<ShapeModel> =
    {
        Name = "dedupe-overloads"
        Run =
            fun _ model ->
                async {
                    let separated = literalOverloads model

                    let literalDecls =
                        separated
                        |> List.collect _.Declared
                        |> List.distinctBy (fun (name, _, _) -> name)
                        |> List.map literalDecl

                    let mutable findings =
                        separated
                        |> List.map (fun set ->
                            Finding.make set.Member (DedupeOverloads.OverloadsDistinguishedByLiteral set.Parameter))

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
                        | other -> other

                    let signatureKey (parameters: FsParam list) =
                        parameters |> List.map (fun p -> p.Optional, p.Rest, normalize Set.empty p.Type)

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
                                let key = ("Create", signatureKey c.Parameters).ToString()

                                if Set.contains key seen then
                                    findings <-
                                        findings @ [ Finding.make $"{owner}.Create" DedupeOverloads.OverloadDropped ]

                                    false
                                else
                                    seen <- Set.add key seen
                                    true
                            | FsMethod m ->
                                let key = (m.Name, signatureKey m.Parameters).ToString()

                                if Set.contains key seen then
                                    findings <-
                                        findings @ [ Finding.make $"{owner}.{m.Name}" DedupeOverloads.OverloadDropped ]

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

                    let mutable seenExports = Set.empty

                    let exportMembers =
                        model.ExportMembers
                        |> List.filter (fun (_, m) ->
                            let key =
                                match m.Body with
                                | ExportFunction(parameters, _) -> Some("fn", signatureKey parameters)
                                | ExportConstructor(parameters, _) -> Some("new", signatureKey parameters)
                                | ExportValue _ -> None

                            match key with
                            | None -> true
                            | Some key ->
                                let key = (m.Name, key).ToString()

                                if Set.contains key seenExports then
                                    findings <- findings @ [ Finding.make m.Name DedupeOverloads.OverloadDropped ]

                                    false
                                else
                                    seenExports <- Set.add key seenExports
                                    true)

                    let model =
                        { model with
                            Decls = decls @ literalDecls
                            ExportMembers = exportMembers
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
