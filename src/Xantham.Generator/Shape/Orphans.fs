module Xantham.Generator.Shape.Orphans

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.Generator.Shape.Spec

// ---------------------------------------------------------------------------------------------
// `synthesize-anonymous` claims a delegate name while the positions reading the callback are
// still open, and a position settled later can write the parameters out in full instead: a
// lifecycle hook rendered as a handler interface does. This pass runs over the finished set.
// ---------------------------------------------------------------------------------------------

/// The declared names a reference reads, including the operand of an application.
let rec private namesOf (reference: FsTypeRef) : string list =
    match reference with
    | FsNamed name -> [ name ]
    | FsApp(name, arguments) -> name :: List.collect namesOf arguments
    | FsOption inner
    | FsArray inner -> namesOf inner
    | FsTuple parts
    | FsErasedUnion parts -> List.collect namesOf parts
    | FsDelegate(arguments, returns) -> List.collect namesOf arguments @ namesOf returns
    | FsFunc(argument, returns) -> namesOf argument @ namesOf returns
    | FsBranded(primitive, measure) -> measure :: namesOf primitive
    | FsBool
    | FsString
    | FsFloat
    | FsBigInt
    | FsUnit
    | FsObj
    | FsTypeVar _ -> []

let private paramNames (parameters: FsParam list) =
    parameters |> List.collect (_.Type >> namesOf)

let private typeParamNames (parameters: FsTypeParam list) =
    parameters
    |> List.collect (_.Constraint >> Option.map namesOf >> Option.defaultValue [])

let rec private memberNames (m: FsMember) =
    match m with
    | FsProperty p -> namesOf p.Type
    | FsMethod m -> typeParamNames m.TypeParameters @ paramNames m.Parameters @ namesOf m.Return
    | FsIndexer i -> namesOf i.Key @ namesOf i.Value
    | FsConstructor c
    | FsInvoke c -> typeParamNames c.TypeParameters @ paramNames c.Parameters @ namesOf c.Return
    | FsSymbolKeyed(_, inner) -> memberNames inner
    | FsLiteralKeyed(_, _, inner) -> memberNames inner

let private exportMemberNames (m: FsExportMember) =
    typeParamNames m.TypeParameters
    @ (match m.Body with
       | ExportFunction(parameters, returns)
       | ExportConstructor(parameters, returns) -> paramNames parameters @ namesOf returns
       | ExportValue reference -> namesOf reference)

/// Every declared name the declaration reads, excluding a delegate's own name.
let internal readNames (decl: FsDecl) : string list =
    match decl with
    | FsInterface i ->
        typeParamNames i.TypeParameters
        @ List.collect namesOf i.Inherits
        @ List.collect memberNames i.Members
        @ (i.Entrypoint
           |> Option.map (fun e ->
               paramNames e.Parameters
               @ (e.Inherits |> Option.map namesOf |> Option.defaultValue []))
           |> Option.defaultValue [])
        @ List.collect (fun (overload: FsCreateOverload) -> paramNames overload.Parameters) i.CreateOverloads
        @ List.collect exportMemberNames i.Statics
    | FsAbbrev a ->
        typeParamNames a.TypeParameters
        @ namesOf a.Target
        @ (a.Value |> Option.map (snd >> namesOf) |> Option.defaultValue [])
    | FsDelegateType d ->
        typeParamNames d.TypeParameters
        @ (d.Parameters |> List.collect (_.Type >> namesOf))
        @ namesOf d.Return
        |> List.filter (fun name -> name <> d.Name)
    | FsPhantom p -> typeParamNames p.TypeParameters @ namesOf p.Carrier
    | FsMeasure m -> namesOf m.Primitive
    | FsTaggedUnion u ->
        typeParamNames u.TypeParameters
        @ (u.Cases |> List.collect (_.Fields >> List.collect (_.Type >> namesOf)))
    | FsExports container -> container.Members |> List.collect (_.Member >> exportMemberNames)
    | FsStringEnum _
    | FsEnum _ -> []

/// Every name a declaration is written under: the set `audit-coverage` looks an export up in.
let private declaredNames (decls: FsDecl list) : Set<string> =
    decls
    |> List.collect (function
        | FsInterface d -> [ d.Name ]
        | FsStringEnum d -> [ d.Name ]
        | FsTaggedUnion d -> [ d.Name ]
        | FsEnum d -> [ d.Name ]
        | FsAbbrev d -> [ d.Name ]
        | FsDelegateType d -> [ d.Name ]
        | FsPhantom d -> [ d.Name ]
        | FsMeasure d -> [ d.Name ]
        | FsExports container -> container.Members |> List.map (fun owned -> owned.Member.Name))
    |> Set.ofList

/// Whether a declaration carrying one of `names` represents the export: its own name, a name
/// nested under it, or one whose final segment is it. `audit-coverage` accepts the same three.
let private represented (names: Set<string>) (exported: string) =
    Set.contains exported names
    || names
       |> Set.exists (fun declared -> declared.StartsWith(exported + ".") || declared.EndsWith("." + exported))

/// The interface names of the anonymous arms a declared DU carries whole: every arm of an
/// unfolded case, and an arm of a folded case whose every field the case carries.
let private taggedArmNames (model: ShapeModel) : Set<string> =
    let unions =
        model.Decls
        |> List.choose (function
            | FsTaggedUnion u -> Some u.Name
            | _ -> None)
        |> Set.ofList

    model.DeclNames
    |> Map.toList
    |> List.filter (snd >> unions.Contains)
    |> List.choose (fun (typeId, _) -> Map.tryFind typeId model.Types)
    |> List.collect (fun facts ->
        let carriedWhole =
            match taggedUnionShape model facts with
            | Discriminated(tag, cases, folded) ->
                fun (arm: TypeFacts) ->
                    match taggedArmValue model tag arm with
                    | Some value when List.contains value folded ->
                        cases
                        |> List.exists (fun (case, caseValue) ->
                            caseValue = value && caseCarriesArm model tag case arm)
                    | Some _ -> true
                    | None -> false
            | TagCollides _
            | Untagged -> fun _ -> false

        facts.UnionMembers
        |> List.filter (fun armId ->
            Map.tryFind armId model.Types
            |> Option.exists (fun arm -> isAnonymousArm arm && carriedWhole arm)))
    |> List.choose (fun armId -> Map.tryFind armId model.DeclNames)
    |> Set.ofList

/// The name prefixes under which declarations exist for arms alone: each tagged union's `Cases`
/// module, each anonymous tagged arm's own path, and each union the exclusive-arm fold declares as
/// one interface.
let private armRoots (model: ShapeModel) (arms: Set<string>) : string list =
    let unionRoots =
        model.DeclNames
        |> Map.toList
        |> List.choose (fun (typeId, name) ->
            match Map.tryFind typeId model.Types with
            | Some facts when not (Map.isEmpty (taggedCaseNames model facts)) -> Some $"{name}.Cases."
            | Some facts ->
                match exclusiveArmShape model facts with
                | ExclusiveArmsFold _ -> Some $"{name}."
                | NotExclusiveArms
                | ExclusiveArmsDecline _ -> None
            | None -> None)

    unionRoots @ [ for arm in arms -> $"{arm}." ]

/// The declaration names the generator minted for an inline shape.
let private mintedNames (model: ShapeModel) : Set<string> =
    model.DeclNames
    |> Map.toList
    |> List.choose (fun (typeId, name) ->
        match Map.tryFind typeId model.Types with
        | Some facts when isAnonymousArm facts -> Some name
        | _ -> None)
    |> Set.ofList

/// The name a declaration is written under; `None` for the `Exports` container.
let private declName (decl: FsDecl) =
    match decl with
    | FsInterface d -> Some d.Name
    | FsStringEnum d -> Some d.Name
    | FsTaggedUnion d -> Some d.Name
    | FsEnum d -> Some d.Name
    | FsAbbrev d -> Some d.Name
    | FsDelegateType d -> Some d.Name
    | FsPhantom d -> Some d.Name
    | FsMeasure d -> Some d.Name
    | FsExports _ -> None

/// The declaration set with every unread delegate, unread tagged-union arm interface and unread
/// generator-minted declaration under an arm root removed, and the names removed. A declaration
/// whose removal would leave a harvested export unrepresented is retained. Dropping one
/// declaration can leave another unread, so the removal repeats until the set is stable.
let rec private prune
    (arms: Set<string>)
    (minted: Set<string>)
    (roots: string list)
    (exports: string list)
    (dropped: string list)
    (decls: FsDecl list)
    : FsDecl list * string list =
    let read = decls |> List.collect readNames |> Set.ofList

    let candidates =
        decls
        |> List.choose (fun decl ->
            match decl, declName decl with
            | FsDelegateType d, _ when not (Set.contains d.Name read) -> Some d.Name
            | FsInterface i, _ when Set.contains i.Name arms && not (Set.contains i.Name read) -> Some i.Name
            | _, Some name when
                not (Set.contains name read)
                && Set.contains name minted
                && roots |> List.exists (fun root -> name.StartsWith root)
                ->
                Some name
            | _ -> None)
        |> Set.ofList

    let names = declaredNames decls
    let surviving = Set.difference names candidates

    // The candidates that are an export's only representation, taken back out of the set.
    let retained =
        exports
        |> List.filter (fun exported -> represented names exported && not (represented surviving exported))
        |> List.fold
            (fun held exported ->
                candidates
                |> Set.filter (fun candidate -> represented (Set.singleton candidate) exported)
                |> Set.union held)
            Set.empty

    let unread = Set.difference candidates retained

    if Set.isEmpty unread then
        decls, dropped
    else
        decls
        |> List.filter (fun decl -> declName decl |> Option.forall (fun name -> not (Set.contains name unread)))
        |> prune arms minted roots exports (dropped @ List.ofSeq unread)

/// The name every harvested export is looked up under: the qualified name `name-exports`
/// assigned, falling back to the bare export name for a value export.
let private exportNames (ctx: Context) (model: ShapeModel) : string list =
    let bare = fsName (defaultExportName ctx)

    model.Harvest.Exports
    |> List.map (fun export ->
        model.ExportTypes
        |> Map.tryFind export.Symbol.SymbolId
        |> Option.bind _.Declared
        |> Option.bind (fun typeId -> Map.tryFind typeId model.DeclNames)
        |> Option.defaultValue (bare export))

//FOR-REVIEW The pass also drops arm interfaces and their nested declarations now; renaming it to
//FOR-REVIEW `drop-orphan-declarations` is deferred, since the name appears in every manifest.
let dropOrphanDelegates: Pass<ShapeModel> =
    {
        Name = "drop-orphan-delegates"
        Run =
            fun ctx model ->
                async {
                    let arms = taggedArmNames model

                    let delegates =
                        model.Decls
                        |> List.choose (function
                            | FsDelegateType d -> Some d.Name
                            | _ -> None)
                        |> Set.ofList

                    let decls, dropped =
                        prune arms (mintedNames model) (armRoots model arms) (exportNames ctx model) [] model.Decls

                    let model = { model with Decls = decls }

                    let findings =
                        dropped
                        |> List.sort
                        |> List.map (fun name ->
                            if Set.contains name delegates then
                                Finding.make name DropOrphanDelegates.DelegateReachedNoReference
                            elif Set.contains name arms then
                                Finding.make name DropOrphanDelegates.TaggedArmReachedNoReference
                            else
                                Finding.make name DropOrphanDelegates.ArmDeclarationReachedNoReference)

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
