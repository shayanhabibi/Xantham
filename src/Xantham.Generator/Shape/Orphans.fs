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

let private memberNames (m: FsMember) =
    match m with
    | FsProperty p -> namesOf p.Type
    | FsMethod m -> typeParamNames m.TypeParameters @ paramNames m.Parameters @ namesOf m.Return
    | FsIndexer i -> namesOf i.Key @ namesOf i.Value
    | FsConstructor c
    | FsInvoke c -> typeParamNames c.TypeParameters @ paramNames c.Parameters @ namesOf c.Return

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
        @ List.collect paramNames i.CreateOverloads
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
    | FsTaggedUnion u -> u.Cases |> List.collect (_.Fields >> List.collect (_.Type >> namesOf))
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

/// The declaration set with every unread delegate removed, and the names removed. A delegate
/// whose removal would leave a harvested export unrepresented is retained. Dropping one delegate
/// can leave another unread, so the removal repeats until the set is stable.
let rec private prune (exports: string list) (dropped: string list) (decls: FsDecl list) : FsDecl list * string list =
    let read = decls |> List.collect readNames |> Set.ofList

    let candidates =
        decls
        |> List.choose (function
            | FsDelegateType d when not (Set.contains d.Name read) -> Some d.Name
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
        |> List.filter (function
            | FsDelegateType d -> not (Set.contains d.Name unread)
            | _ -> true)
        |> prune exports (dropped @ List.ofSeq unread)

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

let dropOrphanDelegates: Pass<ShapeModel> =
    {
        Name = "drop-orphan-delegates"
        Run =
            fun ctx model ->
                async {
                    let decls, dropped = prune (exportNames ctx model) [] model.Decls

                    let model = { model with Decls = decls }

                    let findings =
                        dropped
                        |> List.sort
                        |> List.map (fun name -> Finding.make name DropOrphanDelegates.DelegateReachedNoReference)

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
