/// Tier 3 - Shape: the mapping document executed. Phase B of
/// `docs/plans/generator-architecture.md` covers interfaces with methods and overloads,
/// literal unions (D12), callbacks as delegates (D5), classes (instance interface plus
/// constructor members on `Exports`), ParamObject synthesis (D3), arrays, and value exports;
/// what remains richer than that widens to `obj` with a finding, so the fidelity manifest -
/// not silence - says what is not done yet. Every pass here is pure.
module Xantham.Generator.Shape

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The fallback name for a default export - `Naming.defaultExport` over the entry package.
let defaultExportName (ctx: Context) = Naming.defaultExport ctx.PackageName

/// The F# name a harvested export generates under: the exported name; a default export takes
/// its declaring symbol's name - except `export default function name` binds the symbol itself
/// as `default`, which falls back to the package-derived name.
let fsName (fallback: string) (export: HarvestedExport) =
    if export.ExportName <> "default" then export.ExportName
    elif export.Symbol.Name = "default" || export.Symbol.Name.StartsWith "__" then fallback
    else export.Symbol.Name

// ---------------------------------------------------------------------------------------------
// Facts the passes repeatedly ask of a type.
// ---------------------------------------------------------------------------------------------

let private flag (f: TypeFlags) (facts: TypeFacts) = facts.Response.Flags.HasFlag f

/// The literal payload of a literal type, when the response carries one.
let literalOf (facts: TypeFacts) : FsLiteral option =
    if flag TypeFlags.StringLiteral facts then
        match facts.Response.Value with
        | null -> None
        | value -> Some(LitString(value.GetValue<string>()))
    elif flag TypeFlags.NumberLiteral facts then
        match facts.Response.Value with
        | null -> None
        | value -> Some(LitNumber(value.GetValue<float>()))
    elif flag TypeFlags.BooleanLiteral facts then
        match facts.Response.Value with
        | null ->
            match facts.Response.IntrinsicName with
            | ValueSome "true" -> Some(LitBool true)
            | ValueSome "false" -> Some(LitBool false)
            | _ -> None
        | value -> Some(LitBool(value.GetValue<bool>()))
    else
        None

let private isNullish (facts: TypeFacts) =
    flag TypeFlags.Undefined facts || flag TypeFlags.Null facts || flag TypeFlags.Void facts

/// A union's members split into the hoisted nullish part (D1) and everything else.
let private splitNullish (model: ShapeModel) (facts: TypeFacts) =
    facts.UnionMembers
    |> List.partition (fun id ->
        match Map.tryFind id model.Types with
        | Some m -> isNullish m
        | None -> false)

/// `true | false` after nullish hoisting: TS re-expands `boolean` inside larger unions, and
/// the pair is just `bool` again.
let private isBooleanPair (model: ShapeModel) (memberIds: int list) =
    memberIds.Length = 2
    && memberIds
       |> List.forall (fun id ->
           match Map.tryFind id model.Types with
           | Some m -> flag TypeFlags.BooleanLiteral m
           | None -> false)

/// A union candidate's non-nullish member ids, sorted for member-set comparison.
let private nonNullishMemberSet (model: ShapeModel) (candidate: TypeFacts) =
    candidate.UnionMembers
    |> List.filter (fun id ->
        match Map.tryFind id model.Types with
        | Some m -> not (isNullish m)
        | None -> true)
    |> List.sort

/// The declared union whose non-nullish member set matches, if any: what lets an
/// `"ms" | "s" | undefined` member position resolve to the exported `TimeUnit` rather than a
/// synthesized twin (literal types are interned, so the ids match across positions).
let private namedUnionByMembers (model: ShapeModel) (memberIds: int list) : string option =
    let wanted = List.sort memberIds

    model.DeclNames
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.tryPick (fun (typeId, name) ->
        match Map.tryFind typeId model.Types with
        | Some candidate when flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate) ->
            if nonNullishMemberSet model candidate = wanted then Some name else None
        | _ -> None)

/// An object type that is only a callback: call signatures and nothing else worth keeping.
let private isPureCallback (facts: TypeFacts) =
    not facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty
    && facts.Members.IsEmpty

/// The compiler's array types, recognized by identity so the check holds for every group
/// disposition (the entry package cannot declare a global `Array`).
let private arrayElement (facts: TypeFacts) =
    match facts.SymbolName, facts.TypeArguments with
    | Some("Array" | "ReadonlyArray"), [ element ] -> Some element
    | _ -> None

/// A symbol name the checker made up for an anonymous shape rather than one the author wrote.
/// Module symbols are named by their quoted file path, which is no name either.
let private isSyntheticName (name: string) =
    name.StartsWith "__" || name.StartsWith "\""

/// A member keyed by a JS well-known symbol (`__@iterator@<id>`): unrepresentable in F#, and
/// the embedded checker id is session-specific - keeping one would also break determinism.
let private isSymbolKeyed (name: string) = name.StartsWith "__@"

/// A tuple type - shaped as an array until D7's dedicated pass (phase C).
let private isTuple (facts: TypeFacts) =
    facts.Response.IsTupleType = ValueSome true

// ---------------------------------------------------------------------------------------------
// Type references.
// ---------------------------------------------------------------------------------------------

/// The F# type written at a reference position, with the findings any widening produces.
/// `self` is the name of the declaration being shaped, so a polymorphic `this` return can
/// resolve to it. Flag-test order matters: `boolean` (a union wearing the Boolean flag) before
/// the union case, unions before the literal tests, literals before their base primitives.
let rec typeRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (typeId: int) : FsTypeRef * Finding list =
    match Map.tryFind typeId model.Types with
    | None ->
        match Map.tryFind typeId model.NotFollowed with
        | Some reason -> FsObj, [ Finding.make Widened owner $"type not resolved ({reason}); widened to obj" ]
        | None -> FsObj, [ Finding.make Escape owner $"type#{typeId} missing from the type table; widened to obj" ]
    | Some facts ->
        let has f = flag f facts

        if has TypeFlags.Boolean then
            FsBool, []
        elif has TypeFlags.Union then
            unionRef ctx model self owner facts
        elif has TypeFlags.BooleanLiteral then
            FsBool, []
        elif has TypeFlags.EnumLiteral then
            match literalOf facts with
            | Some(LitNumber _) -> FsFloat, [ Finding.make Widened owner "lone enum member widened to float" ]
            | _ -> FsString, [ Finding.make Widened owner "lone enum member widened to string" ]
        elif has TypeFlags.StringLiteral then
            FsString, [ Finding.make Widened owner "string literal type widened to string (doc-noted, §4.2)" ]
        elif has TypeFlags.NumberLiteral then
            FsFloat, [ Finding.make Widened owner "numeric literal type widened to float (doc-noted, §4.2)" ]
        elif has TypeFlags.String then
            FsString, []
        elif has TypeFlags.Number then
            FsFloat, []
        elif has TypeFlags.Void || has TypeFlags.Undefined || has TypeFlags.Never then
            FsUnit, []
        elif has TypeFlags.Any then
            FsObj, [ Finding.make Escape owner "any maps to obj" ]
        elif has TypeFlags.Unknown then
            FsObj, [ Finding.make Widened owner "unknown maps to obj (D8)" ]
        elif has TypeFlags.TypeParameter then
            if facts.Response.IsThisType = ValueSome true then
                match self with
                | Some name -> FsNamed name, [ Finding.make Ergonomic owner "polymorphic this reads as the declaring type" ]
                | None -> FsObj, [ Finding.make Widened owner "this type outside a declaration; widened to obj" ]
            else
                FsObj, [ Finding.make Widened owner "type parameter widened to obj (generics are phase C)" ]
        elif has TypeFlags.Object then
            objectRef ctx model self owner facts
        else
            FsObj, [ Finding.make Widened owner $"type flags {facts.Response.Flags} not mapped yet; widened to obj" ]

and private objectRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    match arrayElement facts with
    | Some element ->
        let inner, findings = typeRef ctx model self owner element
        FsArray inner, findings
    | None ->

    match Map.tryFind facts.Response.Id model.DeclNames with
    | Some name -> FsNamed name, []
    | None ->
        if isTuple facts then
            // Tuples read as arrays until D7's shape-tuples pass (phase C): the homogeneous
            // element type when there is one, `obj[]` otherwise.
            let elements =
                facts.TypeArguments
                |> List.map (fun element -> typeRef ctx model self owner element |> fst)
                |> List.distinct

            match elements with
            | [ element ] ->
                FsArray element, [ Finding.make Widened owner "tuple reads as an array (D7 tuples are phase C)" ]
            | _ ->
                FsArray FsObj,
                [ Finding.make Widened owner "heterogeneous tuple widened to obj[] (D7 tuples are phase C)" ]
        elif isPureCallback facts then
            delegateRef ctx model self owner facts
        else
            match GeneratorConfig.disposition ctx.Config facts.Origin, facts.SymbolName with
            | Reference, Some typeName ->
                // The O7 contract: a `ship` run of this group produces exactly this name.
                FsNamed $"{Naming.groupModule ctx.PackageName facts.Origin}.{typeName}", []
            | Reference, None ->
                FsObj,
                [ Finding.make Widened owner "anonymous type in a referenced group cannot be templated; widened to obj" ]
            | (Ship | Widen), _ ->
                let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
                FsObj, [ Finding.make Widened owner $"{shown} is not among the generated declarations; widened to obj" ]

/// A callback as a delegate (D5): guaranteed arity at the boundary. Only the first signature
/// shapes the delegate; further overloads on a callback are a finding.
and private delegateRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    match facts.CallSignatures with
    | [] -> FsObj, [ Finding.make Widened owner "callable type without signatures; widened to obj" ]
    | signature :: rest ->
        let mutable findings =
            if rest.IsEmpty then
                []
            else
                [ Finding.make Widened owner $"callback with {rest.Length + 1} overloads shaped from the first" ]

        let parameters =
            signature.Parameters
            |> List.map (fun p ->
                let reference, refFindings = typeRef ctx model self $"{owner}({p.Symbol.Name})" p.TypeId
                findings <- findings @ refFindings
                optionalRef (isOptionalParam p reference) reference)

        let returns, returnFindings = typeRef ctx model self $"{owner}()" signature.ReturnTypeId
        FsDelegate(parameters, returns), findings @ returnFindings

/// A union hoists its `null`/`undefined` members into `option` (D1). What remains resolves as
/// a single member, a named literal union (classified by `classify-literal-unions`), or widens
/// - position-aware union treatment (D4) is phase C.
and private unionRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let hoisted, remaining = splitNullish model facts

    let wrap reference findings =
        if List.isEmpty hoisted then
            reference, findings
        else
            // Never nest: an already-optional inner stays one level, per the D1 note on
            // Fable's erased option being unsound when nested.
            let wrapped =
                match reference with
                | FsOption _ -> reference
                | reference -> FsOption reference

            wrapped, Finding.make Ergonomic owner "null/undefined union members hoisted to option" :: findings

    match remaining with
    | [] -> FsUnit, [ Finding.make Widened owner "union of only null/undefined members maps to unit" ]
    | [ single ] ->
        let inner, findings = typeRef ctx model self owner single
        wrap inner findings
    | _ when isBooleanPair model remaining -> wrap FsBool []
    | _ ->
        match Map.tryFind facts.Response.Id model.DeclNames with
        | Some name -> wrap (FsNamed name) []
        | None ->
            match namedUnionByMembers model remaining with
            | Some name -> wrap (FsNamed name) []
            | None ->
                wrap FsObj [ Finding.make Widened owner "union with several non-null members widened to obj (D4 is phase C)" ]

/// An optional member or parameter reads as `option`, one level deep however the optionality
/// arrived (a `?` marker, an `undefined` union member, or both).
and optionalRef (optional: bool) (reference: FsTypeRef) =
    match optional, reference with
    | false, reference -> reference
    | true, FsOption _ -> reference
    | true, reference -> FsOption reference

/// The wire does not flag optional parameters on their symbols, so a parameter whose type
/// admits `undefined` (already hoisted to option by `typeRef`) is optional too - D1 collapses
/// the distinction anyway.
and private isOptionalParam (p: ResolvedMember) (reference: FsTypeRef) =
    p.Optional
    || (match reference with
        | FsOption _ -> true
        | _ -> false)

// ---------------------------------------------------------------------------------------------
// Shared shaping of members and signatures.
// ---------------------------------------------------------------------------------------------

/// A resolved signature as an F# parameter list and return reference. Rest parameters are
/// marked from the signature flag; their array types read as-is.
let private shapeSignature (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (signature: ResolvedSignature) : FsParam list * FsTypeRef * Finding list =
    let mutable findings = []
    let parameterCount = signature.Parameters.Length

    let parameters =
        signature.Parameters
        |> List.mapi (fun i p ->
            let paramOwner = $"{owner}({p.Symbol.Name})"
            let reference, refFindings = typeRef ctx model self paramOwner p.TypeId
            findings <- findings @ refFindings

            let rest = signature.HasRest && i = parameterCount - 1
            let optional = not rest && isOptionalParam p reference

            if p.Optional then
                findings <- findings @ [ Finding.make Ergonomic paramOwner "optional parameter reads as option" ]

            { Name = p.Symbol.Name
              Optional = optional
              Rest = rest
              Type = optionalRef optional reference })

    let returns, returnFindings = typeRef ctx model self $"{owner}()" signature.ReturnTypeId
    parameters, returns, findings @ returnFindings

/// The interface members of an object type: methods for method symbols (each call signature an
/// overload), properties otherwise, callbacks as delegate-typed properties (D5).
let private shapeMembers (ctx: Context) (model: ShapeModel) (self: string) (facts: TypeFacts) : FsMember list * Finding list =
    let mutable findings = []
    let emit finding = findings <- findings @ [ finding ]

    let members =
        facts.Members
        |> List.filter (fun m ->
            if isSymbolKeyed m.Symbol.Name then
                // The name is cut at the checker id (`__@iterator@1469` -> `__@iterator`):
                // the id is session-specific and would break run-to-run determinism.
                let stable = m.Symbol.Name.Substring(0, m.Symbol.Name.LastIndexOf '@')
                emit (Finding.make Widened $"{self}.{stable}" "symbol-keyed member dropped (unrepresentable in F#)")
                false
            else
                true)
        |> List.collect (fun m ->
            let owner = $"{self}.{m.Symbol.Name}"

            let asMethod =
                if not (hasAny SymbolFlags.Method m.Symbol.Flags) then
                    None
                else
                    match Map.tryFind m.TypeId model.Types with
                    | Some memberFacts when not memberFacts.CallSignatures.IsEmpty -> Some memberFacts
                    | _ -> None

            match asMethod with
            | Some memberFacts ->
                memberFacts.CallSignatures
                |> List.map (fun signature ->
                    let parameters, returns, signatureFindings =
                        shapeSignature ctx model (Some self) owner signature

                    findings <- findings @ signatureFindings

                    FsMethod
                        { Name = m.Symbol.Name
                          Docs = m.Docs
                          Tags = m.Tags
                          Parameters = parameters
                          Return = returns })
            | None ->
                let reference, refFindings = typeRef ctx model (Some self) owner m.TypeId
                findings <- findings @ refFindings

                if m.Optional then
                    emit (Finding.make Ergonomic owner "optional member reads as option")

                [ FsProperty
                      { Name = m.Symbol.Name
                        Docs = m.Docs
                        Tags = m.Tags
                        ReadOnly = m.ReadOnly
                        Type = optionalRef m.Optional reference } ])

    members, findings

// ---------------------------------------------------------------------------------------------
// Passes.
// ---------------------------------------------------------------------------------------------

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type the first in harvest order names it and `shape-aliases` abbreviates the rest.
let nameExports: Pass<ShapeModel> =
    Pass.pure' "name-exports" (fun ctx model ->
        let fallback = defaultExportName ctx

        let names, orders =
            model.Harvest.Exports
            |> List.fold
                (fun (names, orders) export ->
                    if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                        names, orders
                    else
                        match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                        | Some typeId when not (Map.containsKey typeId names) ->
                            Map.add typeId (fsName fallback export) names, Map.add typeId export.Order orders
                        | _ -> names, orders)
                (model.DeclNames, model.DeclOrders)

        { model with
            DeclNames = names
            DeclOrders = orders })

/// Walks the type graph reachable from the exports in deterministic order and names what needs
/// a declaration but has none: anonymous entry-group object types with members, and literal
/// unions (hash-consing by type id, §4.4). Named non-exported entry types keep their own name;
/// path-derived names cover the anonymous rest; collisions take a numeric suffix in visit
/// order.
let synthesizeAnonymous: Pass<ShapeModel> =
    Pass.pure' "synthesize-anonymous" (fun ctx model ->
        let mutable names = model.DeclNames
        let mutable orders = model.DeclOrders
        let mutable taken = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList
        let mutable visited = Set.empty

        let claim (preferred: string) typeId order =
            let unique =
                if not (Set.contains preferred taken) then
                    preferred
                else
                    Seq.initInfinite (fun i -> $"{preferred}{i + 2}")
                    |> Seq.find (fun candidate -> not (Set.contains candidate taken))

            names <- Map.add typeId unique names
            orders <- Map.add typeId order orders
            taken <- Set.add unique taken

        /// A literal union worth a declaration: at least two non-nullish members, all literal,
        /// not just `true | false`, and no already-named union with the same member set.
        let isLiteralUnion (facts: TypeFacts) =
            let _, remaining = splitNullish model facts

            remaining.Length > 1
            && remaining
               |> List.forall (fun id ->
                   match Map.tryFind id model.Types with
                   | Some m -> (literalOf m).IsSome
                   | None -> false)
            && not (isBooleanPair model remaining)
            && (namedUnionByMembers { model with DeclNames = names } remaining).IsNone

        let needsName (facts: TypeFacts) =
            if Map.containsKey facts.Response.Id names then
                false
            elif flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
                isLiteralUnion facts
            elif flag TypeFlags.Object facts then
                // Entry-group object shapes with members become interfaces; callbacks stay
                // inline as delegates, arrays as arrays, tuples as arrays (D7). Constructor
                // objects (a class's static side) get their constructors on `Exports`, not a
                // declaration.
                GeneratorConfig.disposition ctx.Config facts.Origin = Ship
                && not (isPureCallback facts)
                && (arrayElement facts).IsNone
                && not (isTuple facts)
                && facts.ConstructSignatures.IsEmpty
                && not facts.Members.IsEmpty
            else
                false

        let rec walk (path: string) (order: DeclOrder option) (typeId: int) =
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited

                match Map.tryFind typeId model.Types with
                | None -> ()
                | Some facts ->
                    if needsName facts then
                        let preferred =
                            match facts.SymbolName with
                            | Some name when not (isSyntheticName name) -> Naming.pascalSegment name
                            | _ -> path

                        claim preferred typeId order

                    // Recurse in the shape the declaration will read: members, signatures,
                    // union members, then structural identity (element, arguments, bases).
                    let named = Map.tryFind typeId names
                    let into segment = (named |> Option.defaultValue path) + segment

                    for m in facts.Members do
                        walk (into (Naming.pascalSegment m.Symbol.Name)) order m.TypeId

                    let signatures = facts.CallSignatures @ facts.ConstructSignatures

                    for signature in signatures do
                        for p in signature.Parameters do
                            walk (into (Naming.pascalSegment p.Symbol.Name)) order p.TypeId

                        walk (into "Result") order signature.ReturnTypeId

                    for memberId in facts.UnionMembers do
                        walk (named |> Option.defaultValue path) order memberId

                    for argument in facts.TypeArguments do
                        walk (into "Item") order argument

                    for baseId in facts.BaseTypes do
                        walk (into "Base") order baseId

        let fallback = defaultExportName ctx

        for export in model.Harvest.Exports do
            let root = Naming.pascalSegment (fsName fallback export)

            match Map.tryFind export.Symbol.Id model.ExportTypes with
            | Some ids ->
                for typeId in [ yield! Option.toList ids.Declared; yield! Option.toList ids.Value ] do
                    walk root export.Order typeId
            | None -> ()

        { model with
            DeclNames = names
            DeclOrders = orders })

/// Case names must be unique within one DU; a later duplicate takes a numeric suffix in member
/// order, deterministically.
let private dedupeUnionCases (cases: FsUnionCase list) =
    let mutable seen = Set.empty

    cases
    |> List.map (fun case ->
        let unique =
            if not (Set.contains case.Name seen) then
                case.Name
            else
                Seq.initInfinite (fun i -> $"{case.Name}{i + 2}")
                |> Seq.find (fun candidate -> not (Set.contains candidate seen))

        seen <- Set.add unique seen
        { case with Name = unique })

let private dedupeEnumCases (cases: (string * int) list) =
    let mutable seen = Set.empty

    cases
    |> List.map (fun (name, value) ->
        let unique =
            if not (Set.contains name seen) then
                name
            else
                Seq.initInfinite (fun i -> $"{name}{i + 2}")
                |> Seq.find (fun candidate -> not (Set.contains candidate seen))

        seen <- Set.add unique seen
        unique, value)

/// Declarations for named literal unions: StringEnum DUs with `CompiledName` per case, mixed
/// unions carrying `CompiledValue` cases (D12), all-integer unions as F# enums - including
/// reassembled TS enums, whose members name their cases (§4.7, §4.2).
let classifyLiteralUnions: Pass<ShapeModel> =
    { Name = "classify-literal-unions"
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
                        | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
                            let _, remaining = splitNullish model facts

                            let literals =
                                remaining
                                |> List.choose (fun id ->
                                    Map.tryFind id model.Types
                                    |> Option.bind (fun m -> literalOf m |> Option.map (fun l -> m, l)))

                            if
                                literals.Length < remaining.Length
                                || literals.Length < 2
                                || isBooleanPair model remaining
                            then
                                None
                            else

                            let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                            let intCase =
                                function
                                | LitNumber value when System.Double.IsInteger value && abs value < 2147483648.0 ->
                                    Some(int value)
                                | _ -> None

                            let allInts = literals |> List.forall (fun (_, literal) -> (intCase literal).IsSome)

                            if allInts then
                                // Numeric enum territory: member symbols (a real TS enum) name
                                // the cases; bare numeric literal unions derive them.
                                let cases =
                                    literals
                                    |> List.map (fun (m, literal) ->
                                        let caseName =
                                            match m.SymbolName with
                                            | Some symbolName when not (isSyntheticName symbolName) ->
                                                Naming.pascalSegment symbolName
                                            | _ ->
                                                match literal with
                                                | LitNumber value -> Naming.enumCaseOfNumber value
                                                | _ -> "Case"

                                        caseName, (intCase literal).Value)

                                Some(FsEnum { Name = name; Docs = ""; Tags = []; Order = order; Cases = dedupeEnumCases cases })
                            else
                                let cases =
                                    literals
                                    |> List.map (fun (m, literal) ->
                                        let caseName =
                                            match m.SymbolName with
                                            | Some symbolName when not (isSyntheticName symbolName) ->
                                                Naming.pascalSegment symbolName
                                            | _ ->
                                                match literal with
                                                | LitString text -> Naming.enumCaseOfString text
                                                | LitNumber value -> Naming.enumCaseOfNumber value
                                                | LitBool true -> "True"
                                                | LitBool false -> "False"

                                        match literal with
                                        | LitString text ->
                                            { Name = caseName
                                              CompiledName = (if text = caseName then None else Some text)
                                              CompiledValue = None }
                                        | literal ->
                                            findings <-
                                                findings
                                                @ [ Finding.make
                                                        Exact
                                                        name
                                                        "non-string literal case carries CompiledValue (D12)" ]

                                            { Name = caseName
                                              CompiledName = None
                                              CompiledValue = Some literal })

                                Some(FsStringEnum { Name = name; Docs = ""; Tags = []; Order = order; Cases = dedupeUnionCases cases })
                        | _ -> None)

                let model = { model with Decls = model.Decls @ decls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// The delegate shape of a named callback, without the self-name lookup that would just return
/// the abbreviation being defined.
let private delegateRefFor (ctx: Context) (model: ShapeModel) (name: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    match facts.CallSignatures with
    | [] -> FsObj, [ Finding.make Widened name "callable type without signatures; widened to obj" ]
    | signature :: rest ->
        let overloadFindings =
            if rest.IsEmpty then
                []
            else
                [ Finding.make Widened name $"callback with {rest.Length + 1} overloads shaped from the first" ]

        let parameters, returns, signatureFindings = shapeSignature ctx model None name signature

        let parameterTypes = parameters |> List.map _.Type
        FsDelegate(parameterTypes, returns), overloadFindings @ signatureFindings

/// Abbreviations for named pure-callback types: `type TimerCallback = Action<Timer>` (D5).
let shapeCallbacks: Pass<ShapeModel> =
    { Name = "shape-callbacks"
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
                            let reference, refFindings = delegateRefFor ctx model name facts
                            findings <- findings @ refFindings

                            Some(
                                FsAbbrev
                                    { Name = name
                                      Docs = ""
                                      Tags = []
                                      Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                      Target = reference }
                            )
                        | _ -> None)

                let model = { model with Decls = model.Decls @ decls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// F# interfaces for every named object type with members: exported interfaces and class
/// instance sides alike, plus the synthesized anonymous shapes. Heritage is flattened - the
/// checker's property list already includes inherited members, and F# rejects re-abstracted
/// inherited members - with a finding recording the lost is-a relation.
let shapeInterfaces: Pass<ShapeModel> =
    { Name = "shape-interfaces"
      Run =
        fun ctx model ->
            async {
                let mutable findings = []

                let fallbackDocs =
                    model.Harvest.Exports
                    |> List.choose (fun export ->
                        Map.tryFind export.Symbol.Id model.ExportTypes
                        |> Option.bind _.Declared
                        |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                    |> Map.ofList

                let decls =
                    model.DeclNames
                    |> Map.toList
                    |> List.sortBy fst
                    |> List.choose (fun (typeId, name) ->
                        match Map.tryFind typeId model.Types with
                        | Some facts when
                            flag TypeFlags.Object facts
                            && not facts.Members.IsEmpty
                            && (arrayElement facts).IsNone
                            && not (isTuple facts)
                            ->
                            let members, memberFindings = shapeMembers ctx model name facts
                            findings <- findings @ memberFindings

                            if not facts.CallSignatures.IsEmpty then
                                findings <-
                                    findings
                                    @ [ Finding.make
                                            Widened
                                            name
                                            "callable-and-properties hybrid loses its call signatures (Invoke emission is future work)" ]

                            if not facts.BaseTypes.IsEmpty then
                                findings <-
                                    findings
                                    @ [ Finding.make
                                            Ergonomic
                                            name
                                            "base members flattened into the interface (the is-a relation is not emitted)" ]

                            let docs, tags =
                                Map.tryFind typeId fallbackDocs |> Option.defaultValue ("", [])

                            Some(
                                FsInterface
                                    { Name = name
                                      Docs = docs
                                      Tags = tags
                                      Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                      Inherits = []
                                      Members = members
                                      CreateOverloads = [] }
                            )
                        | _ -> None)

                let model = { model with Decls = model.Decls @ decls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// `typeRef` with the type's own naming suppressed, for the right side of an abbreviation -
/// otherwise every abbreviation would just name itself. Declared unions with the same member
/// set may only be matched at a *smaller* type id (the canonical twin), so alias chains
/// strictly decrease and can never cycle - the smallest twin widens structurally instead.
let private typeRefIgnoringSelf (ctx: Context) (model: ShapeModel) (name: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let largerTwins =
        if flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
            let wanted = nonNullishMemberSet model facts

            model.DeclNames
            |> Map.toList
            |> List.choose (fun (typeId, _) ->
                if typeId <= facts.Response.Id then
                    None
                else
                    match Map.tryFind typeId model.Types with
                    | Some candidate when flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate) ->
                        if nonNullishMemberSet model candidate = wanted then Some typeId else None
                    | _ -> None)
        else
            []

    let unnamed =
        { model with
            DeclNames =
                largerTwins
                |> List.fold (fun names id -> Map.remove id names) (Map.remove facts.Response.Id model.DeclNames) }

    typeRef ctx unnamed None name facts.Response.Id

/// Abbreviations for the named types no earlier pass declared: aliases to primitives, arrays,
/// other named types, or whatever `typeRef` widens them to. Also covers a second export of an
/// already-named type.
let shapeAliases: Pass<ShapeModel> =
    { Name = "shape-aliases"
      Run =
        fun ctx model ->
            async {
                let mutable findings = []

                let declaredNames =
                    model.Decls
                    |> List.collect (function
                        | FsInterface decl -> [ decl.Name ]
                        | FsStringEnum decl -> [ decl.Name ]
                        | FsEnum decl -> [ decl.Name ]
                        | FsAbbrev decl -> [ decl.Name ]
                        | FsExports _ -> [])
                    |> Set.ofList

                let fallback = defaultExportName ctx

                // A second type-like export of an already-named type abbreviates to it.
                let aliasDecls =
                    model.Harvest.Exports
                    |> List.choose (fun export ->
                        if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                            None
                        else
                            let name = fsName fallback export

                            match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                            | Some typeId ->
                                match Map.tryFind typeId model.DeclNames with
                                | Some primary when primary <> name ->
                                    Some(
                                        FsAbbrev
                                            { Name = name
                                              Docs = export.Docs
                                              Tags = export.Tags
                                              Order = export.Order
                                              Target = FsNamed primary }
                                    )
                                | _ -> None
                            | None -> None)

                let remainingDecls =
                    model.DeclNames
                    |> Map.toList
                    |> List.sortBy fst
                    |> List.choose (fun (typeId, name) ->
                        if Set.contains name declaredNames then
                            None
                        else
                            match Map.tryFind typeId model.Types with
                            | Some facts ->
                                // The named cases earlier passes handle; what reaches here is
                                // referable without a declaration of its own.
                                let reference, refFindings =
                                    match arrayElement facts with
                                    | Some element ->
                                        let inner, innerFindings = typeRef ctx model None name element
                                        FsArray inner, innerFindings
                                    | None ->
                                        match Map.tryFind facts.Response.Id model.DeclNames with
                                        | Some primary when primary <> name -> FsNamed primary, []
                                        | _ -> typeRefIgnoringSelf ctx model name facts

                                findings <- findings @ refFindings

                                Some(
                                    FsAbbrev
                                        { Name = name
                                          Docs = ""
                                          Tags = []
                                          Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                          Target = reference }
                                )
                            | None -> None)

                let model =
                    { model with
                        Decls = model.Decls @ aliasDecls @ remainingDecls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// Constructor members on `Exports` for exported classes: `Exports.Name(...)` is
/// `new Name(...)` through `[<EmitConstructor>]` (§4.4). Static class members beyond the
/// constructor are findings until a fixture needs them.
let shapeClasses: Pass<ShapeModel> =
    { Name = "shape-classes"
      Run =
        fun ctx model ->
            async {
                let mutable findings = []
                let fallback = defaultExportName ctx

                let members =
                    model.Harvest.Exports
                    |> List.indexed
                    |> List.collect (fun (index, export) ->
                        if not (hasAny SymbolFlags.Class export.Symbol.Flags) then
                            []
                        else
                            let name = fsName fallback export

                            let valueFacts =
                                Map.tryFind export.Symbol.Id model.ExportTypes
                                |> Option.bind _.Value
                                |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                            match valueFacts with
                            | None ->
                                findings <-
                                    findings @ [ Finding.make Escape name "class export without a value type; constructor dropped" ]

                                []
                            | Some facts ->
                                let statics =
                                    facts.Members
                                    |> List.filter (fun m -> not (m.Symbol.Name = "prototype"))

                                for m in statics do
                                    findings <-
                                        findings
                                        @ [ Finding.make
                                                Widened
                                                $"{name}.{m.Symbol.Name}"
                                                "static class member dropped (statics emission awaits a fixture)" ]

                                facts.ConstructSignatures
                                |> List.map (fun signature ->
                                    let parameters, returns, signatureFindings =
                                        shapeSignature ctx model (Some name) name signature

                                    findings <- findings @ signatureFindings

                                    index,
                                    { Name = name
                                      Docs = export.Docs
                                      Tags = export.Tags
                                      Binding =
                                        if export.ExportName = "default" then
                                            ImportDefault
                                        else
                                            ImportNamed export.ExportName
                                      Body = ExportConstructor(parameters, returns) }))

                let model =
                    { model with
                        ExportMembers = model.ExportMembers @ members }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// `Exports` members from the value exports that are not classes: functions (every overload
/// emitted), and values - `const`/`let` and namespace objects - as get-only properties.
let shapeExports: Pass<ShapeModel> =
    { Name = "shape-exports"
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

                            let binding =
                                if export.ExportName = "default" then
                                    ImportDefault
                                else
                                    ImportNamed export.ExportName

                            let valueFacts =
                                Map.tryFind export.Symbol.Id model.ExportTypes
                                |> Option.bind _.Value
                                |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                            match valueFacts with
                            | None ->
                                emit (Finding.make Escape name "no value type in the table; export dropped")
                                []
                            | Some facts when not facts.CallSignatures.IsEmpty ->
                                facts.CallSignatures
                                |> List.map (fun signature ->
                                    let parameters, returns, signatureFindings =
                                        shapeSignature ctx model None name signature

                                    findings <- findings @ signatureFindings

                                    index,
                                    { Name = name
                                      Docs = export.Docs
                                      Tags = export.Tags
                                      Binding = binding
                                      Body = ExportFunction(parameters, returns) })
                            | Some facts ->
                                let reference, refFindings = typeRef ctx model None name facts.Response.Id
                                findings <- findings @ refFindings

                                [ index,
                                  { Name = name
                                    Docs = export.Docs
                                    Tags = export.Tags
                                    Binding = binding
                                    Body = ExportValue reference } ])

                let model =
                    { model with
                        ExportMembers = model.ExportMembers @ members }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// Parameters beyond this stop being construction ergonomics: a Create this wide is unusable
/// at a call site, and each one is quadratic work for the F# typechecker.
[<Literal>]
let private CreateParameterBudget = 24

/// Construction ergonomics (D3, §4.4): every plain-data interface - properties only - gains a
/// `[<ParamObject; Emit("$0")>]` Create overload mirroring its members, required members
/// first, so consumers never hand-build objects.
let synthesizeParamObjects: Pass<ShapeModel> =
    { Name = "synthesize-paramobjects"
      Run =
        fun _ model ->
            async {
                let mutable findings = []

                let decls =
                    model.Decls
                    |> List.map (function
                        | FsInterface decl when
                            not decl.Members.IsEmpty
                            && decl.Members.Length <= CreateParameterBudget
                            && decl.Members
                               |> List.forall (function
                                   | FsProperty _ -> true
                                   | FsMethod _ -> false)
                            ->
                            let parameters =
                                decl.Members
                                |> List.map (function
                                    | FsProperty p ->
                                        let optional =
                                            match p.Type with
                                            | FsOption _ -> true
                                            | _ -> false

                                        { Name = p.Name
                                          Optional = optional
                                          Rest = false
                                          Type = p.Type }
                                    | FsMethod _ -> failwith "unreachable: filtered to properties")

                            let required, optional = parameters |> List.partition (fun p -> not p.Optional)

                            findings <-
                                findings
                                @ [ Finding.make Ergonomic decl.Name "ParamObject Create synthesized (D3)" ]

                            FsInterface
                                { decl with
                                    CreateOverloads = [ required @ optional ] }
                        | decl -> decl)

                let model = { model with Decls = decls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// Overloads that widened into the same F# signature are duplicates the compiler rejects -
/// .NET overload resolution sees through type abbreviations and ignores return types. The
/// first survives; the rest drop with a finding.
let dedupeOverloads: Pass<ShapeModel> =
    { Name = "dedupe-overloads"
      Run =
        fun _ model ->
            async {
                let mutable findings = []

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
                    | FsDelegate(args, ret) -> FsDelegate(args |> List.map (normalize visited), normalize visited ret)
                    | other -> other

                let signatureKey (parameters: FsParam list) =
                    parameters
                    |> List.map (fun p -> p.Optional, p.Rest, normalize Set.empty p.Type)

                let dedupeMethods (owner: string) (members: FsMember list) =
                    let mutable seen = Set.empty

                    members
                    |> List.filter (function
                        | FsProperty _ -> true
                        | FsMethod m ->
                            let key = (m.Name, signatureKey m.Parameters).ToString()

                            if Set.contains key seen then
                                findings <-
                                    findings
                                    @ [ Finding.make
                                            Widened
                                            $"{owner}.{m.Name}"
                                            "overload dropped: identical to an earlier one after widening" ]

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
                                    Members = dedupeMethods decl.Name decl.Members }
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
                                findings <-
                                    findings
                                    @ [ Finding.make
                                            Widened
                                            m.Name
                                            "overload dropped: identical to an earlier one after widening" ]

                                false
                            else
                                seenExports <- Set.add key seenExports
                                true)

                let model =
                    { model with
                        Decls = decls
                        ExportMembers = exportMembers }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// Fixes the output order the renderer will follow verbatim: declarations in source order with
/// name as the tiebreak, then the `Exports` type - its members in harvest order - last.
let orderDeclarations: Pass<ShapeModel> =
    Pass.pure' "order-declarations" (fun _ model ->
        let orderKey (order: DeclOrder option) (name: string) =
            (match order with
             | Some order -> order.File, order.NodeIndex
             | None -> "￿", System.Int32.MaxValue),
            name

        let decls =
            model.Decls
            |> List.sortBy (function
                | FsInterface decl -> orderKey decl.Order decl.Name
                | FsStringEnum decl -> orderKey decl.Order decl.Name
                | FsEnum decl -> orderKey decl.Order decl.Name
                | FsAbbrev decl -> orderKey decl.Order decl.Name
                | FsExports _ -> ("￿", System.Int32.MaxValue), "￿")

        let exports =
            model.ExportMembers
            |> List.sortBy (fun (index, m) -> index, m.Name)
            |> List.map snd

        { model with
            Decls =
                match exports with
                | [] -> decls
                | exports -> decls @ [ FsExports exports ]
            ExportMembers = [] })

/// The no-silent-drops check: every harvested export either appears in the declarations or is
/// the subject of a finding this pass adds. Passes that drop already say so, so overlap is
/// possible - this is the safety net, not the reporter of record.
let auditCoverage: Pass<ShapeModel> =
    { Name = "audit-coverage"
      Run =
        fun ctx model ->
            async {
                let generated =
                    model.Decls
                    |> List.collect (function
                        | FsInterface decl -> [ decl.Name ]
                        | FsStringEnum decl -> [ decl.Name ]
                        | FsEnum decl -> [ decl.Name ]
                        | FsAbbrev decl -> [ decl.Name ]
                        | FsExports members -> members |> List.map _.Name)
                    |> Set.ofList

                let name = fsName (defaultExportName ctx)

                let missing =
                    model.Harvest.Exports
                    |> List.filter (fun export -> not (Set.contains (name export) generated))
                    |> List.map (fun export ->
                        Finding.make Escape (name export) "export not represented in the generated output")

                return
                    if List.isEmpty missing then
                        Advanced model
                    else
                        Degraded(model, missing)
            } }

/// The tier's pass list, in execution order.
let passes: Pass<ShapeModel> list =
    [ nameExports
      synthesizeAnonymous
      classifyLiteralUnions
      shapeCallbacks
      shapeInterfaces
      shapeAliases
      shapeClasses
      shapeExports
      synthesizeParamObjects
      dedupeOverloads
      orderDeclarations
      auditCoverage ]
