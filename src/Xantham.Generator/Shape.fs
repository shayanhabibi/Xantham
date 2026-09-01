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

/// How a harvested value binds in JavaScript. An ambient global is already on `globalThis`, so
/// it takes `[<Global>]`; only a module export carries an import.
let bindingOf (export: HarvestedExport) =
    match export.Origin with
    | FromGlobal -> GlobalName export.ExportName
    | FromModule when export.ExportName = "default" -> ImportDefault
    | FromModule -> ImportNamed export.ExportName

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

/// The discriminant of a tagged union (D4, §4.5(2)), when the checker proves there is one:
/// every non-nullish member is an object type carrying the same property, and that property's
/// type is a string literal that is *distinct* across the members. Returns the property name
/// as TypeScript spells it, paired with each member's facts and its tag value, in the union's
/// own member order.
///
/// §4.5 says to detect this aggressively, and this is why: Fable erases the DU back to the
/// object, so the mapping costs nothing at runtime and buys pattern matching - the single
/// biggest ergonomic win available anywhere in the catalogue.
///
/// Candidate properties are considered in the *first* member's declaration order, so a union
/// discriminated by two properties at once picks the same one on every run.
let private taggedUnionShape (model: ShapeModel) (facts: TypeFacts) : (string * (TypeFacts * string) list) option =
    let members =
        facts.UnionMembers
        |> List.choose (fun id -> Map.tryFind id model.Types)
        |> List.filter (isNullish >> not)

    let isObjectMember (m: TypeFacts) =
        flag TypeFlags.Object m && not m.Members.IsEmpty

    if members.Length < 2 || not (members |> List.forall isObjectMember) then
        None
    else
        /// The member's own string-literal value for `tag`, when it has exactly one.
        let tagValue (m: TypeFacts) (tag: string) =
            m.Members
            |> List.tryFind (fun property -> property.Symbol.Name = tag)
            |> Option.bind (fun property -> Map.tryFind property.TypeId model.Types)
            |> Option.bind (fun propertyType ->
                match literalOf propertyType with
                | Some(LitString text) -> Some text
                | _ -> None)

        members
        |> List.head
        |> _.Members
        |> List.map _.Symbol.Name
        |> List.filter (isSymbolKeyed >> not)
        |> List.tryPick (fun tag ->
            let tagged = members |> List.map (fun m -> tagValue m tag |> Option.map (fun value -> m, value))

            if tagged |> List.forall Option.isSome then
                let tagged = tagged |> List.map Option.get
                let values = tagged |> List.map snd

                // Two members sharing a tag value are not discriminated by it - matching on
                // that case could not tell them apart.
                if List.distinct values = values then Some(tag, tagged) else None
            else
                None)

/// A property that exists only to make a type nominal: keyed by a unique symbol, so nothing
/// can name it; named with a leading underscore, so nothing is meant to; or typed `never`, so
/// nothing can construct it. An object whose every property is one of these carries nothing at
/// runtime, which is what separates a branding intersection from a shape (§4.6).
let private isMarkerMember (model: ShapeModel) (m: ResolvedMember) =
    isSymbolKeyed m.Symbol.Name
    || m.Symbol.Name.StartsWith "_"
    || (match Map.tryFind m.TypeId model.Types with
        | Some facts -> flag TypeFlags.Never facts
        | None -> false)

/// The primitive a branding intersection brands, where it is one (§4.6, D11): exactly one
/// primitive constituent, intersected with objects that carry markers and nothing else. Two
/// real shapes intersected, or a primitive intersected with an object that has a usable
/// member, are ordinary intersections and no brand - reading those as brands would throw
/// members away and call it exact.
let rec private brandedPrimitive (model: ShapeModel) (facts: TypeFacts) =
    // An intersection over anything but a bare primitive distributes: `boolean & Marker` is
    // handed back as `(true & Marker) | (false & Marker)`, and a branded literal union the same
    // way. The arms are the checker's own working and carry no names, so a union of anonymous
    // brands that agree on the primitive is one brand - while a union of *named* brands
    // (`UserId | SessionId`) is a real union and must stay one.
    if flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
        let arms = facts.UnionMembers |> List.choose (fun id -> Map.tryFind id model.Types)

        if
            arms.Length <> facts.UnionMembers.Length
            || arms.IsEmpty
            || arms
               |> List.exists (fun arm ->
                   not (flag TypeFlags.Intersection arm) || Map.containsKey arm.Response.Id model.DeclNames)
        then
            None
        else
            match arms |> List.map (brandedPrimitive model) |> List.distinct with
            | [ single ] -> single
            | _ -> None
    else

    let constituents =
        facts.IntersectionMembers |> List.choose (fun id -> Map.tryFind id model.Types)

    if constituents.Length <> facts.IntersectionMembers.Length then
        None
    else

    let objects, primitives = constituents |> List.partition (flag TypeFlags.Object)

    let primitive =
        match primitives with
        | [ only ] ->
            // Boolean first: it is a union of `true | false` wearing the Boolean flag.
            if flag TypeFlags.Boolean only || flag TypeFlags.BooleanLiteral only then Some FsBool
            elif flag TypeFlags.String only || flag TypeFlags.StringLiteral only then Some FsString
            elif flag TypeFlags.Number only || flag TypeFlags.NumberLiteral only then Some FsFloat
            else None
        | _ -> None

    match primitive with
    | Some primitive when
        not objects.IsEmpty
        && objects
           |> List.forall (fun o -> not o.Members.IsEmpty && o.Members |> List.forall (isMarkerMember model))
        ->
        Some primitive
    | _ -> None

/// The widest erased union D4 allows. Fable ships `U2`-`U9`; the decision is four, because
/// past that the consumer is doing runtime tests the type no longer helps them write.
[<Literal>]
let private ErasedUnionArity = 4

/// The widest tagged-union case worth generating. A DU case binds its fields positionally, so
/// past a dozen every `match` clause is a wall of wildcards and the erased union over the arm
/// interfaces - which keeps the properties named - reads better.
[<Literal>]
let private TaggedCaseFieldBudget = 12

/// A tuple type (§4.12). Fable compiles an F# tuple to a JS array, so a fixed tuple is an
/// exact match; the variadic forms are not.
let private isTuple (facts: TypeFacts) =
    facts.Response.IsTupleType = ValueSome true

/// A tuple element the checker marked `...rest` or variadic. F# tuples are fixed-arity, so a
/// tuple carrying one has no tuple form at all.
let private isVariadicElement (flags: ElementFlags) =
    flags.HasFlag ElementFlags.Rest || flags.HasFlag ElementFlags.Variadic

/// A tuple's element flags, one per type argument. The wire reports them off the tuple's
/// target and the two can only disagree if the schema changes under us; a disagreement reads
/// every element as required, which is the conservative shape.
let private tupleElementFlags (facts: TypeFacts) =
    if facts.TupleElements.Length = facts.TypeArguments.Length then
        facts.TupleElements
    else
        facts.TypeArguments |> List.map (fun _ -> ElementFlags.Required)

/// The generic declaration an instantiation points back at, when this run declares it. A
/// generic declaration is its own target, so only a genuine instantiation matches.
///
/// The checker substitutes members eagerly, so `Box<string>` arrives fully expanded and would
/// read perfectly well as a structure of its own; writing it as an application instead keeps
/// the two spellings tied together, which is what §4.9 asks for.
let private instantiationOf (model: ShapeModel) (facts: TypeFacts) =
    // Only a *reference* - `Ready<T>` over an interface or class - is an application. An
    // anonymous object type instantiated in some other scope also carries its original as a
    // target, but no arguments to write it with, so it is declared on its own as before.
    let isReference =
        facts.Response.ObjectFlags
        |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
        |> ValueOption.defaultValue false

    match facts.Response.Target with
    | ValueSome target when isReference && target <> facts.Response.Id ->
        Map.tryFind target model.DeclNames |> Option.map (fun name -> name, facts.TypeArguments)
    | _ -> None

/// The arguments a generic declaration stands for when it is named at a reference position:
/// its own parameters. F# has no bare spelling for a generic type, so the self-reference in
/// `map(next: T): Box<T>` has to re-apply them to come back out as it was written.
let private ownArguments (facts: TypeFacts) =
    match facts.Response.Target with
    | ValueSome target when target = facts.Response.Id -> facts.TypeArguments
    | _ -> []

/// The parameters a hoisted anonymous declaration reads from the scope it was written in
/// (§4.9, `DeclParams`) - what a reference to it applies back, and what its declaration binds
/// beside any parameters of its own.
let private freeParamsOf (model: ShapeModel) (typeId: int) =
    Map.tryFind typeId model.DeclParams |> Option.defaultValue []

// ---------------------------------------------------------------------------------------------
// Type references.
// ---------------------------------------------------------------------------------------------

/// The type ids on the current reference descent, so that a shape reached from itself is cut
/// rather than followed forever.
///
/// A *named* cycle terminates on its own: the declaration is in `DeclNames` and the second
/// visit renders as the name. An unnamed one has no such floor - `lib.dom.d.ts` writes several,
/// and every one of them is a union whose arm is an anonymous object with a member back in the
/// union - so the descent must remember where it has been. Per-thread because the shape tier
/// runs its passes sequentially but the pipeline is `Async`, and the state is a path, not a
/// cache: an id is removed on the way out, so a type referenced twice side by side is shaped
/// twice, as it must be.
type private Descent() =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private path: System.Collections.Generic.HashSet<int>

    static member Path =
        if isNull Descent.path then
            Descent.path <- System.Collections.Generic.HashSet<int>()

        Descent.path

/// The F# type written at a reference position, with the findings any widening produces.
/// `self` is the name of the declaration being shaped, so a polymorphic `this` return can
/// resolve to it. Flag-test order matters: `boolean` (a union wearing the Boolean flag) before
/// the union case, unions before the literal tests, literals before their base primitives.
let rec typeRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (typeId: int) : FsTypeRef * Finding list =
    if not (Descent.Path.Add typeId) then
        FsObj,
        [ Finding.make Widened owner "type refers to itself through unnamed shapes; widened to obj" ]
    else
        try
            typeRefOnPath ctx model self owner typeId
        finally
            Descent.Path.Remove typeId |> ignore

and private typeRefOnPath (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (typeId: int) : FsTypeRef * Finding list =
    match Map.tryFind typeId model.Types with
    | None ->
        match Map.tryFind typeId model.NotFollowed with
        | Some reason -> FsObj, [ Finding.make Widened owner $"type not resolved ({reason}); widened to obj" ]
        | None -> FsObj, [ Finding.make Escape owner $"type#{typeId} missing from the type table; widened to obj" ]
    | Some facts ->
        let has f = flag f facts

        if has TypeFlags.Boolean then
            FsBool, []
        elif has TypeFlags.Union && (brandedPrimitive model facts).IsSome then
            intersectionRef model owner facts
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
                // A key variable is not bound as a variable at all: `K extends keyof T` is
                // written as the support package's idiom over the operand (§4.10).
                match Map.tryFind typeId model.KeyVars with
                | Some(KeyOf operand) -> FsApp("keyof", [ FsTypeVar operand ]), []
                | Some(TypedKeyOf(operand, result)) -> FsApp("typekeyof", [ FsTypeVar operand; FsTypeVar result ]), []
                | None ->

                // In scope only where the declaration being shaped bound it (§4.9); a
                // parameter of some *other* declaration has no name here to write.
                match Map.tryFind typeId model.TypeVars with
                | Some name -> FsTypeVar name, []
                | None ->
                    // Its constraint is the tightest thing still true of it, and where the
                    // declaration bound one, `obj` is not merely loose but wrong: F# rejects
                    // `Ai<obj>` against `'AiModelList :> AiModelListType`. Only a plain named
                    // constraint is taken - a generic one would need an arity this position
                    // cannot supply - and another declaration's parameter can never be
                    // constrained by this same parameter, so the substitution cannot cycle.
                    let constraintName =
                        facts.Constraint
                        |> Option.filter (fun boundId -> boundId <> typeId)
                        |> Option.bind (fun boundId ->
                            match Map.tryFind boundId model.Types with
                            | Some bound when (ownArguments bound).IsEmpty -> Map.tryFind boundId model.DeclNames
                            | _ -> None)

                    match constraintName with
                    | Some name ->
                        FsNamed name,
                        [ Finding.make Widened owner $"type parameter is not in scope here; widened to its constraint {name}" ]
                    | None -> FsObj, [ Finding.make Widened owner "type parameter is not in scope here; widened to obj" ]
        elif has TypeFlags.Object then
            objectRef ctx model self owner facts
        elif has TypeFlags.Index then
            keyOfRef model owner facts
        elif has TypeFlags.IndexedAccess then
            indexedAccessRef model owner facts
        elif has TypeFlags.Intersection then
            intersectionRef model owner facts
        else
            FsObj, [ Finding.make Widened owner $"type flags {facts.Response.Flags} not mapped yet; widened to obj" ]

/// `keyof T` at an operand the checker could not finish (§4.10). A closed `keyof` never gets
/// here - the checker hands those back already expanded into their union of literal keys, which
/// shapes as a StringEnum - so this is the open regime, where the only honest carrier is the
/// support package's `keyof<'T>`: erased to the string it is at runtime, and phantom-typed by
/// the operand so a key of one type cannot be passed where another's is wanted.
and private keyOfRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let operand =
        facts.Response.Target
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id model.TypeVars)

    match operand with
    | Some name ->
        FsApp("keyof", [ FsTypeVar name ]),
        [ Finding.make Ergonomic owner $"keyof over an open operand reads as keyof<'{name}> (§4.10)" ]
    | None -> FsObj, [ Finding.make Widened owner "keyof over an operand not in scope here; widened to obj" ]

/// An intersection at a reference position. A brand (§4.6, D11) is the one intersection F# can
/// state exactly: the measure its declaration emitted, applied to the primitive it brands, which
/// enforces the same nominality TypeScript was buying and erases the same way. It costs no
/// finding here - the declaration records the idiom once - but a brand that never got a
/// declaration has no measure to name, and falls back to the bare primitive loudly. Intersections
/// of object types are a separate mapping (§4.6's first bullet) and are not shaped yet.
and private intersectionRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    match brandedPrimitive model facts with
    | Some primitive ->
        match Map.tryFind facts.Response.Id model.DeclNames with
        | Some name -> FsBranded(primitive, name), []
        | None ->
            primitive,
            [ Finding.make
                  Ergonomic
                  owner
                  "an unnamed brand has no measure to carry; widened to the primitive it brands (§4.6)" ]
    | None ->
        FsObj,
        [ Finding.make Widened owner "intersection of object types has no F# form yet; widened to obj (§4.6)" ]

/// `T[K]`. Where `K` is a key variable this signature bound as `typekeyof<'T,'R>`, the access is
/// exactly the `'R` that idiom introduced. Everything else - `T[keyof T]`, an access over an
/// operand not in scope - is a type-level computation with no F# form, and widens loudly.
and private indexedAccessRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let binding =
        facts.Response.IndexType
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id model.KeyVars)

    let objectName =
        facts.Response.ObjectType
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id model.TypeVars)

    match binding, objectName with
    | Some(TypedKeyOf(operand, result)), Some name when operand = name -> FsTypeVar result, []
    | _ -> FsObj, [ Finding.make Widened owner "indexed access has no F# form here; widened to obj" ]

and private objectRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    match arrayElement facts with
    | Some element ->
        let inner, findings = typeRef ctx model self owner element
        FsArray inner, findings
    | None ->

    match Map.tryFind facts.Response.Id model.DeclNames with
    | Some name ->
        match ownArguments facts @ freeParamsOf model facts.Response.Id with
        | [] -> FsNamed name, []
        | arguments -> appliedRef ctx model self owner name arguments
    | None ->
        if isTuple facts then
            tupleRef ctx model self owner facts
        elif isPureCallback facts then
            delegateRef ctx model self owner facts
        else

        match instantiationOf model facts with
        | Some(name, arguments) -> appliedRef ctx model self owner name arguments
        | None ->
            match libBinding ctx model self owner facts with
            | Some result -> result
            | None ->

            match GeneratorConfig.disposition ctx.Config facts.Origin, facts.SymbolName with
            | Reference, Some typeName ->
                // The O7 contract: a `ship` run of this group produces exactly this name.
                FsNamed $"{Naming.groupModule ctx.PackageName facts.Origin}.{typeName}", []
            | Reference, None ->
                FsObj,
                [ Finding.make Widened owner "anonymous type in a referenced group cannot be templated; widened to obj" ]
            | (Ship | Widen), Some "globalThis" ->
                FsObj, [ Finding.make Widened owner "typeof globalThis is the whole global scope; widened to obj" ]
            | (Ship | Widen), _ ->
                let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
                FsObj, [ Finding.make Widened owner $"{shown} is not among the generated declarations; widened to obj" ]

/// A tuple as an F# tuple (D7, §4.12) - Fable compiles the two to the same JS array, so a
/// fixed tuple is Exact. Element labels are cosmetic and drop.
///
/// Optional tail elements need no work of their own: the checker hands `[string, number?]`
/// over as `string` and `number | undefined`, so D1's hoist has already made that component an
/// `option`. That is exactly D7's decision - an `undefined` slot rather than a shorter array -
/// falling out of the representation instead of being imposed on it.
///
/// A rest or variadic element has no F# tuple form at all, so it widens to an array: the
/// element type when every component agrees, `obj[]` otherwise. §4.12 recommends an erased
/// carrier with typed accessors instead; that waits for a fixture that needs one, the way
/// class statics do.
/// A compiler-lib type a shipped Fable package already binds - `Promise` -> `JS.Promise<'T>`
/// from `Fable.Core`, `EventTarget` -> `Browser.Types.EventTarget` from the `Fable.Browser.*`
/// family - which is the compiler-lib group's disposition for the half of `lib.d.ts` that has
/// a binding at all.
///
/// The two tables are consulted in that order and do not fall through to each other: a name the
/// ECMAScript table knows is answered by it, arity rule included, because that table's `None`
/// means "this is not that type" rather than "look elsewhere".
///
/// The arity comparison is the safety argument, not a formality: TypeScript's lib made
/// `Uint8Array` generic in a buffer parameter Fable's abbreviation does not take, so a mapping
/// that ignored arity would emit code that does not compile. Extra arguments are dropped with
/// a finding; too few means this is some other type wearing a familiar name, and it widens.
and private libBinding (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) =
    match facts.Origin, facts.SymbolName with
    | CompilerLib, Some name when GeneratorConfig.disposition ctx.Config CompilerLib <> Ship ->
        let arguments = facts.TypeArguments

        // (F# name, arity it takes, the loss the mapping itself costs). A DOM binding costs
        // nothing beyond the arity rule below, so its loss list is always empty.
        let bound =
            match Naming.LibBindings.tryFind name with
            | Some(fsharpName, arity, loss) ->
                if arguments.Length < arity then
                    None
                else
                    Some(fsharpName, arity, Option.toList loss)
            | None ->
                Naming.BrowserBindings.tryFind name arguments.Length
                |> Option.map (fun (fsharpName, arity) -> fsharpName, arity, [])

        match bound with
        | None -> None
        | Some(fsharpName, arity, loss) ->
            let reference, findings =
                match arity with
                | 0 -> FsNamed fsharpName, []
                | _ -> appliedRef ctx model self owner fsharpName (List.truncate arity arguments)

            let dropped =
                if arguments.Length > arity then
                    [ Finding.make
                          Ergonomic
                          owner
                          $"{name} carries {arguments.Length} type arguments where {fsharpName} takes {arity}; the extras are dropped" ]
                else
                    []

            let lossy = loss |> List.map (Finding.make Ergonomic owner)

            Some(reference, findings @ dropped @ lossy)
    | _ -> None

/// A generic name applied to type arguments, each shaped at this position (§4.9).
and private appliedRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (name: string) (arguments: int list) : FsTypeRef * Finding list =
    let mutable findings = []

    let mapped =
        arguments
        |> List.map (fun argument ->
            let reference, argumentFindings = typeRef ctx model self owner argument
            findings <- findings @ argumentFindings
            reference)

    FsApp(name, mapped), findings

and private tupleRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let mutable findings = []

    let components =
        facts.TypeArguments
        |> List.map (fun element ->
            let reference, refFindings = typeRef ctx model self owner element
            findings <- findings @ refFindings
            reference)

    let widenToArray reason =
        let element =
            match List.distinct components with
            | [ single ] -> single
            | _ -> FsObj

        FsArray element, findings @ [ Finding.make Widened owner reason ]

    if tupleElementFlags facts |> List.exists isVariadicElement then
        widenToArray "tuple with a rest element widened to an array (§4.12 leaves the erased carrier to a fixture)"
    else
        match components with
        // F# has no zero- or one-component tuple, so neither maps; an array is the honest
        // shape for both, and both are vanishingly rare.
        | []
        | [ _ ] -> widenToArray $"{components.Length}-element tuple has no F# tuple form; widened to an array"
        | components -> FsTuple components, findings

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
            | None -> let reference, findings = erasedUnionRef ctx model self owner remaining in wrap reference findings

/// An unnamed heterogeneous union as Fable's `U2`-`U4` (D4, §4.5(4)). The threshold is four.
///
/// The arms are the members' own F# types, deduplicated: `boolean` re-expands to `true | false`
/// inside a union, and several string-literal members all widen to `string`, so the arm count
/// is only known after mapping. A union that collapses to one arm *is* that type - which is how
/// an unnamed literal union comes out `string` rather than `obj`.
///
/// One arm widening to `obj` collapses the whole union: `U2<obj, Foo>` type-checks against
/// anything at all, so it would trade a legible `obj` for an illegible one.
///
/// D4 asks for this by position - erased-union constructors at input, discriminable values at
/// output. `U_n` is both at once: `U2.Case1 x` is the input-position constructor §4.5 names,
/// and the DU is matchable on the way out. The position-specific thing still missing is
/// expanding an input union into overloads, which is a member rewrite rather than a reference
/// mapping, and which the Create budget's lesson about quadratic overload sets argues for
/// leaving until a fixture asks.
and private erasedUnionRef (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (memberIds: int list) : FsTypeRef * Finding list =
    let mutable findings = []

    let arms =
        memberIds
        |> List.map (fun id ->
            let reference, refFindings = typeRef ctx model self owner id
            findings <- findings @ refFindings
            reference)
        |> List.distinct

    match arms with
    | [] -> FsObj, findings @ [ Finding.make Widened owner "empty union widened to obj" ]
    | [ single ] -> single, findings
    | arms when arms |> List.contains FsObj ->
        FsObj,
        findings
        @ [ Finding.make Widened owner "union with an obj arm widened to obj (an erased union over obj is no safer)" ]
    | arms when arms.Length <= ErasedUnionArity ->
        FsErasedUnion arms, findings
    | arms ->
        FsObj,
        findings
        @ [ Finding.make
                Widened
                owner
                $"union of {arms.Length} distinct types widened to obj (D4 caps the erased union at {ErasedUnionArity})" ]

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
// Generics (§4.9).
// ---------------------------------------------------------------------------------------------

/// A declaration's own type parameters, and the scope its members must be shaped under.
///
/// Both come from the same walk because they have to agree: a parameter that earns a name is
/// the one a member is allowed to reference, and one that does not must not be in scope, or
/// the member would name a variable the declaration never binds.
///
/// A constraint survives only if it maps to a named type, which is the only bound F# can
/// state. `extends string` and `extends keyof T` are dropped with a finding: F# has no form
/// for them, and the nearest approximation would reject code TypeScript accepts.
let private typeParamsOf
    (ctx: Context)
    (model: ShapeModel)
    (owner: string)
    (ids: int list)
    : FsTypeParam list * Map<int, string> * Finding list =
    let mutable findings = []

    let named =
        ids
        |> List.choose (fun id ->
            match Map.tryFind id model.Types |> Option.bind _.SymbolName with
            | Some name -> Some(id, name)
            | None ->
                findings <-
                    findings
                    @ [ Finding.make Widened owner $"type parameter #{id} has no name to write; its uses widen to obj" ]

                None)

    // Layered onto whatever is already in scope rather than replacing it: a generic *method*
    // binds its own parameters on top of its declaration's, and `read<K extends keyof T>` has
    // to see both.
    let scope =
        named |> List.fold (fun bound (id, name) -> Map.add id name bound) model.TypeVars

    // The constraint is read under the scope being defined, so `T extends Node<T>` resolves
    // its own variable rather than widening it.
    let scoped = { model with TypeVars = scope }

    let parameters =
        named
        |> List.map (fun (id, name) ->
            let bound =
                Map.tryFind id model.Types
                |> Option.bind _.Constraint
                |> Option.map (fun boundId ->
                    // Only something that becomes an interface can be an F# base type. A union
                    // renders as an erased `U_n` or a StringEnum and both are sealed, so
                    // `'T :> Renderable` is not merely loose - FS0698 rejects it outright.
                    // Tuples, arrays and delegates are sealed the same way. `FsObj` here falls
                    // into the drop below, which is where the finding is written.
                    let expressible =
                        match Map.tryFind boundId model.Types with
                        | Some bound ->
                            flag TypeFlags.Object bound
                            && (arrayElement bound).IsNone
                            && not (isTuple bound)
                            && not (isPureCallback bound)
                        | None -> false

                    if expressible then
                        typeRef ctx scoped None owner boundId
                    else
                        FsObj, [])

            match bound with
            | Some((FsNamed _ | FsApp _) as reference, boundFindings) ->
                findings <- findings @ boundFindings
                { Name = name; Constraint = Some reference }
            | Some _ ->
                findings <-
                    findings
                    @ [ Finding.make Ergonomic owner $"constraint on '{name}' has no F# form and is dropped (§4.9)" ]

                { Name = name; Constraint = None }
            | None -> { Name = name; Constraint = None })

    parameters, scope, findings

/// The ids a declaration binds: its own where it is a genuine generic declaration, and the
/// alias's where it is a generic *alias*. `type Mapper<T> = (t: T) => T` leaves the function
/// type itself parameterless - the alias is the only place `T` appears - so both are read.
let private declParamIds (facts: TypeFacts) =
    (facts.Response.TypeParameters
     |> ValueOption.map Array.toList
     |> ValueOption.defaultValue [])
    @ facts.AliasTypeArguments
    |> List.distinct

/// The parameters a declaration binds on its left side.
let private declTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    declParamIds facts @ freeParamsOf model facts.Response.Id
    |> List.distinct
    |> typeParamsOf ctx model owner

/// The parameters a callback alias binds, which include the signature's own. F# has no rank-2
/// form, so a generic *function type* - `type F = <T>(t: T) => T`, where each caller picks `T`
/// - can only be approximated by hoisting the variable onto the alias, and that shift is worth
/// a finding. A generic alias to a plain function type binds nothing extra and costs nothing.
let private aliasTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    let declared = declParamIds facts
    let hoisted = facts.CallSignatures |> List.collect _.TypeParameters |> List.distinct
    let parameters, scope, findings = declared @ hoisted |> List.distinct |> typeParamsOf ctx model owner

    let hoistFindings =
        if hoisted |> List.exists (fun id -> not (List.contains id declared)) then
            [ Finding.make Ergonomic owner "generic function type hoisted onto the alias; F# has no rank-2 form (§4.9)" ]
        else
            []

    parameters, scope, findings @ hoistFindings

/// The type variables a rendered reference actually names.
let rec private typeVarsOf (reference: FsTypeRef) : Set<string> =
    let union = List.fold (fun acc item -> Set.union acc (typeVarsOf item)) Set.empty

    match reference with
    | FsTypeVar name -> Set.singleton name
    | FsOption inner
    | FsArray inner -> typeVarsOf inner
    | FsTuple items
    | FsErasedUnion items -> union items
    | FsDelegate(arguments, returns) -> Set.union (union arguments) (typeVarsOf returns)
    | FsApp(_, arguments) -> union arguments
    | _ -> Set.empty

/// The key variables a signature binds (§4.10): each type parameter whose bound is a `keyof`,
/// paired with the id of the operand that `keyof` was taken over.
let private keyCandidates (model: ShapeModel) (ids: int list) : (int * int) list =
    ids
    |> List.choose (fun id ->
        match Map.tryFind id model.Types |> Option.bind _.Constraint with
        | None -> None
        | Some boundId ->
            match Map.tryFind boundId model.Types with
            | Some bound when flag TypeFlags.Index bound ->
                bound.Response.Target |> ValueOption.toOption |> Option.map (fun operand -> id, operand)
            | _ -> None)

/// Whether any of `roots` reaches the indexed access `object[key]` - what tells `key: K` apart
/// from `key: K` *plus* the value it selects. Carriers are followed, members are not: the point
/// is to find `T[K]` where a signature returns it, bare or wrapped, not to walk object graphs.
let private mentionsAccess (model: ShapeModel) (objectId: int) (keyId: int) (roots: int list) : bool =
    let rec go visited pending =
        match pending with
        | [] -> false
        | id :: rest when Set.contains id visited -> go visited rest
        | id :: rest ->
            match Map.tryFind id model.Types with
            | None -> go (Set.add id visited) rest
            | Some facts ->
                if
                    flag TypeFlags.IndexedAccess facts
                    && facts.Response.ObjectType = ValueSome objectId
                    && facts.Response.IndexType = ValueSome keyId
                then
                    true
                else
                    let carried =
                        [ yield! facts.TypeArguments
                          yield! facts.UnionMembers
                          yield! facts.AliasTypeArguments
                          for info in facts.IndexInfos -> info.ValueTypeId
                          for signature in facts.CallSignatures do
                              yield! signature.Parameters |> List.map _.TypeId
                              yield signature.ReturnTypeId ]

                    go (Set.add id visited) (rest @ carried)

    go Set.empty roots

/// The name to write the value a key selects under: `R`, unless something in scope already
/// answers to it - a generated variable that shadows one the signature also mentions would
/// silently retype it.
let private resultName (taken: Set<string>) =
    let rec pick n =
        let candidate = if n = 0 then "R" else $"R{n}"
        if Set.contains candidate taken then pick (n + 1) else candidate

    pick 0

// ---------------------------------------------------------------------------------------------
// Shared shaping of members and signatures.
// ---------------------------------------------------------------------------------------------

/// A resolved signature as an F# type-parameter list, parameter list and return reference.
/// Rest parameters are marked from the signature flag; their array types read as-is.
///
/// A signature's *own* parameters (§4.9) are bound here rather than at the declaration:
/// `get<T>(source: T)` is a generic function, and F# writes that on the member. Without this
/// they were out of scope at every position that used them and the whole signature widened to
/// obj - `get` read `(source: obj, key: obj) : obj`, which is not a typed accessor at all.
let private shapeSignature
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (signature: ResolvedSignature)
    : FsTypeParam list * FsParam list * FsTypeRef * Finding list =
    // §4.10, the open keyof regime: a `K extends keyof T` variable is deliberately *not* bound
    // as an F# variable. Its bound is the whole of what it means, and F# cannot state it, so a
    // bare `'K` would be an unconstrained variable that lets any type through and drags `T[K]`
    // down to obj with it. The support package's idiom is written at its uses instead.
    let candidates = keyCandidates model signature.TypeParameters

    let plain =
        signature.TypeParameters
        |> List.filter (fun id -> candidates |> List.forall (fun (key, _) -> key <> id))

    let typeParameters, scope, parameterFindings =
        match plain with
        | [] -> [], model.TypeVars, []
        | ids -> typeParamsOf ctx model owner ids

    // A key over an operand that is nowhere in scope has no `'T` to be taken over, so it falls
    // back to an ordinary type parameter and widens like one.
    let bindable, loose =
        candidates |> List.partition (fun (_, operand) -> Map.containsKey operand scope)

    let looseParameters, scope, looseFindings =
        match loose |> List.map fst with
        | [] -> [], scope, []
        | ids -> typeParamsOf ctx { model with TypeVars = scope } owner ids

    let roots =
        (signature.Parameters |> List.map _.TypeId) @ [ signature.ReturnTypeId ]

    let mutable taken = scope |> Map.toList |> List.map snd |> Set.ofList
    let mutable keyVars = model.KeyVars
    let mutable resultParameters = []
    let mutable keyFindings = []

    for key, operand in bindable do
        let operandName = Map.find operand scope

        if mentionsAccess model operand key roots then
            let result = resultName taken
            taken <- Set.add result taken
            resultParameters <- resultParameters @ [ { Name = result; Constraint = None } ]
            keyVars <- Map.add key (TypedKeyOf(operandName, result)) keyVars

            keyFindings <-
                keyFindings
                @ [ Finding.make
                        Ergonomic
                        owner
                        $"key over '{operandName}' with its indexed access reads as \
                          typekeyof<'{operandName},'{result}> (§4.10)" ]
        else
            keyVars <- Map.add key (KeyOf operandName) keyVars

            keyFindings <-
                keyFindings
                @ [ Finding.make Ergonomic owner $"key over '{operandName}' reads as keyof<'{operandName}> (§4.10)" ]

    let typeParameters = typeParameters @ looseParameters @ resultParameters

    let model =
        { model with
            TypeVars = scope
            KeyVars = keyVars }

    let mutable findings = parameterFindings @ looseFindings @ keyFindings
    let parameterCount = signature.Parameters.Length

    let referenced =
        signature.Parameters
        |> List.mapi (fun i p ->
            let paramOwner = $"{owner}({p.Symbol.Name})"
            let reference, refFindings = typeRef ctx model self paramOwner p.TypeId
            findings <- findings @ refFindings
            let rest = signature.HasRest && i = parameterCount - 1
            p, paramOwner, reference, rest, (not rest && isOptionalParam p reference))

    // F# optional parameters are a tail: `?a: T, b: U` is FS1212. TypeScript forbids a `?`
    // before a required parameter too, but `undefined` in a parameter's type is admitted
    // anywhere (`createResource(source: S | undefined, fetcher, options?)`), and that is
    // what `isOptionalParam` reads as optional. Only the trailing run gets the `?`; an
    // admitting parameter ahead of a required one stays required, of `option` type - which
    // is what the union hoist already made it, so nothing is lost.
    let optionalTail =
        referenced
        |> List.rev
        |> List.takeWhile (fun (_, _, _, rest, admitsOptional) -> rest || admitsOptional)
        |> List.filter (fun (_, _, _, rest, _) -> not rest)
        |> List.length

    let parameters =
        referenced
        |> List.mapi (fun i (p, paramOwner, reference, rest, admitsOptional) ->
            let inTail = i >= parameterCount - (if signature.HasRest then 1 else 0) - optionalTail
            let optional = admitsOptional && inTail

            if p.Optional then
                findings <- findings @ [ Finding.make Ergonomic paramOwner "optional parameter reads as option" ]

            { Name = Naming.memberName p.Symbol.Name
              Optional = optional
              Rest = rest
              Type = optionalRef admitsOptional reference })

    let returns, returnFindings = typeRef ctx model self $"{owner}()" signature.ReturnTypeId
    findings <- findings @ returnFindings

    // A parameter no rendered position names has been erased - every use of it widened to obj
    // on the way here - and writing `<'T>` over a signature that mentions no `'T` says the
    // member is generic when nothing about it is. Drop it, and say so.
    let named =
        parameters |> List.map _.Type |> List.fold (fun acc t -> Set.union acc (typeVarsOf t)) (typeVarsOf returns)

    let live, erased =
        typeParameters |> List.partition (fun p -> Set.contains p.Name named)

    for p in erased do
        findings <-
            findings
            @ [ Finding.make Widened owner $"type parameter '{p.Name}' is erased: every use of it widened away" ]

    live, parameters, returns, findings

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
                    let typeParameters, parameters, returns, signatureFindings =
                        shapeSignature ctx model (Some self) owner signature

                    findings <- findings @ signatureFindings

                    FsMethod
                        { Name = Naming.memberName m.Symbol.Name
                          Docs = m.Docs
                          Tags = m.Tags
                          TypeParameters = typeParameters
                          Parameters = parameters
                          Return = returns })
            | None ->
                let reference, refFindings = typeRef ctx model (Some self) owner m.TypeId
                findings <- findings @ refFindings

                if m.Optional then
                    emit (Finding.make Ergonomic owner "optional member reads as option")

                [ FsProperty
                      { Name = Naming.memberName m.Symbol.Name
                        Docs = m.Docs
                        Tags = m.Tags
                        ReadOnly = m.ReadOnly
                        Type = optionalRef m.Optional reference } ])

    // Index signatures come after the named members, because that is where an `Item` member
    // reads most naturally and because the order has to be stable for the goldens. A type may
    // declare both a string and a number signature; each becomes its own `Item` overload.
    let indexers =
        facts.IndexInfos
        |> List.map (fun info ->
            let owner = $"{self}.[]"
            let key, keyFindings = typeRef ctx model (Some self) owner info.KeyTypeId
            let value, valueFindings = typeRef ctx model (Some self) owner info.ValueTypeId
            findings <- findings @ keyFindings @ valueFindings

            emit (Finding.make Ergonomic owner "index signature reads as an EmitIndexer Item member (§4.10)")

            FsIndexer
                { Key = key
                  Value = value
                  ReadOnly = info.IsReadonly })

    members @ indexers, findings

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
                // inline as delegates, arrays as arrays, tuples as F# tuples (D7). Constructor
                // objects (a class's static side) get their constructors on `Exports`, not a
                // declaration.
                GeneratorConfig.disposition ctx.Config facts.Origin = Ship
                && not (isPureCallback facts)
                && (arrayElement facts).IsNone
                && not (isTuple facts)
                && facts.ConstructSignatures.IsEmpty
                && not facts.Members.IsEmpty
                // An instantiation of a generic this run declares is written as an
                // application (§4.9). Naming it would declare the expansion a second time
                // under a made-up name and lose the tie to the generic it came from.
                && (instantiationOf { model with DeclNames = names } facts).IsNone
            else
                false

        let rec walk (path: string) (order: DeclOrder option) (typeId: int) =
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited

                match Map.tryFind typeId model.Types with
                | None -> ()
                | Some facts ->
                    // The generic declaration behind an instantiation is named ahead of it,
                    // so that `Ready<T>` reached only through `Resource<T> = Ready<T> | ...`
                    // declares `Ready<'T>` once and every instantiation is written as an
                    // application of it (§4.9) - never a second copy of the expansion under
                    // a made-up name.
                    match facts.Response.Target with
                    | ValueSome target when target <> typeId && Map.containsKey target model.Types ->
                        walk path order target
                    | _ -> ()

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

                    // An instantiation of a named declaration reads only its arguments: its
                    // members are the declaration's, substituted, and shaping happens there.
                    // Walking them would claim names for substituted anonymous member types
                    // that nothing then references.
                    if (instantiationOf { model with DeclNames = names } facts).IsSome then
                        for argument in facts.TypeArguments do
                            walk (into "Item") order argument
                    else

                    // A tuple declaration reads only its components, so its members - `length`
                    // and the numeric indices - are not part of the shape and must not claim
                    // names: `[number, number?]` would otherwise declare its own `1 | 2` length
                    // as an enum nothing references.
                    if not (isTuple facts) then
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

/// The type-parameter ids a declaration reads without binding, in first-use order (§4.9).
/// A signature's own parameters are bound inside it; another *named* declaration binds its
/// own, so the walk stops there and reads only the arguments it is applied with. A hoisted
/// anonymous declaration is walked into: what it reads, its parent reads through it.
let private freeTypeParams (model: ShapeModel) (root: int) : int list =
    let mutable found = []
    let mutable visited = Set.empty

    let rec go (bound: Set<int>) (typeId: int) =
        if not (Set.contains typeId visited) then
            visited <- Set.add typeId visited

            match Map.tryFind typeId model.Types with
            | None -> ()
            | Some facts ->
                if flag TypeFlags.TypeParameter facts then
                    if facts.Response.IsThisType <> ValueSome true
                       && not (Set.contains typeId bound)
                       && not (List.contains typeId found) then
                        found <- found @ [ typeId ]
                elif typeId <> root
                     && Map.containsKey typeId model.DeclNames
                     && (facts.SymbolName |> Option.exists (isSyntheticName >> not)) then
                    // A declaration of its own: it binds what it declares. Only an
                    // instantiation carries arguments worth reading; the declared form's
                    // arguments are its own parameters.
                    if (ownArguments facts).IsEmpty then
                        for argument in facts.TypeArguments do
                            go bound argument
                else
                    for m in facts.Members do
                        go bound m.TypeId

                    for info in facts.IndexInfos do
                        go bound info.KeyTypeId
                        go bound info.ValueTypeId

                    for signature in facts.CallSignatures @ facts.ConstructSignatures do
                        let inner = signature.TypeParameters |> List.fold (fun set id -> Set.add id set) bound

                        for p in signature.Parameters do
                            go inner p.TypeId

                        go inner signature.ReturnTypeId

                    for id in
                        facts.UnionMembers
                        @ facts.IntersectionMembers
                        @ facts.TypeArguments
                        @ facts.BaseTypes
                        @ facts.AliasTypeArguments do
                        go bound id

                    if flag TypeFlags.Index facts then
                        facts.Response.Target |> ValueOption.iter (go bound)

    match Map.tryFind root model.Types with
    | Some facts -> go (Set.ofList (declParamIds facts)) root
    | None -> ()

    found

/// Declares each hoisted object type over the type parameters it reads from the scope it was
/// written in (§4.9, `DeclParams`). `each<T, U>(props: { items: T[]; render: (item: T) => U })`
/// names `EachProps` for the parameter, and `EachProps` binds nothing - so it is declared as
/// `EachProps<'T, 'U>` and the parameter position applies them back, where `'T` and `'U` are
/// in scope. Without this every such member widened its parameter to obj.
let bindFreeTypeParams: Pass<ShapeModel> =
    Pass.pure' "bind-free-type-params" (fun ctx model ->
        let bound =
            model.DeclNames
            |> Map.toList
            |> List.sortBy fst
            |> List.choose (fun (typeId, _) ->
                match Map.tryFind typeId model.Types with
                | Some facts when
                    flag TypeFlags.Object facts
                    && GeneratorConfig.disposition ctx.Config facts.Origin = Ship
                    && (arrayElement facts).IsNone
                    && not (isTuple facts)
                    && not (isPureCallback facts)
                    ->
                    let own = declParamIds facts

                    match freeTypeParams model typeId |> List.filter (fun id -> not (List.contains id own)) with
                    | [] -> None
                    | free -> Some(typeId, free)
                | _ -> None)
            |> Map.ofList

        { model with DeclParams = bound })

/// Case names must be unique within one DU; a later duplicate takes a numeric suffix in member
/// order, deterministically.
let private uniqueCaseNames (names: string list) =
    let mutable seen = Set.empty

    names
    |> List.map (fun name ->
        let unique =
            if not (Set.contains name seen) then
                name
            else
                Seq.initInfinite (fun i -> $"{name}{i + 2}")
                |> Seq.find (fun candidate -> not (Set.contains candidate seen))

        seen <- Set.add unique seen
        unique)

let private dedupeUnionCases (cases: FsUnionCase list) =
    List.map2
        (fun (case: FsUnionCase) name -> { case with Name = name })
        cases
        (uniqueCaseNames (cases |> List.map _.Name))

let private dedupeEnumCases (cases: (string * int) list) =
    List.map2 (fun (_, value) name -> name, value) cases (uniqueCaseNames (cases |> List.map fst))

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

/// Declarations for the unions the checker proves are discriminated (D4, §4.5(2)): an F# DU
/// carrying one payload per case, tagged so Fable erases it straight back to the underlying
/// object. Runs after `classify-literal-unions` because the two are disjoint - a union of
/// literals has no members to carry a discriminant - and before `shape-aliases`, which would
/// otherwise abbreviate the same name structurally.
///
/// Each case carries the arm's own properties as case fields, because that is what Fable's
/// erasure actually writes: `Circle(radius = 2.0)` becomes `{ kind: "circle", radius: 2 }`. An
/// arm that is not plain data has no such form - a method would have to arrive as a delegate
/// field, which reads back as a value rather than a callable member - so a union with one is
/// left to `shape-aliases`, where it stays an erased union over the arm types.
let detectTaggedUnions: Pass<ShapeModel> =
    { Name = "detect-tagged-unions"
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
                            let nullish, _ = splitNullish model facts

                            // A nullable tagged union would have to drop its `null` case to fit
                            // the DU, so it stays an abbreviation and keeps the `option`.
                            if not (List.isEmpty nullish) then
                                None
                            else

                            match taggedUnionShape model facts with
                            | None -> None
                            | Some(tag, tagged) ->
                                // Fable writes the discriminant itself, so the tag property is
                                // not a field; everything else on the arm is.
                                let fieldsOf (arm: TypeFacts) =
                                    arm.Members
                                    |> List.filter (fun m -> m.Symbol.Name <> tag && not (isSymbolKeyed m.Symbol.Name))

                                let isPlainData (arm: TypeFacts) =
                                    arm.CallSignatures.IsEmpty
                                    && arm.ConstructSignatures.IsEmpty
                                    && (fieldsOf arm |> List.forall (fun m -> not (hasAny SymbolFlags.Method m.Symbol.Flags)))
                                    && (fieldsOf arm).Length <= TaggedCaseFieldBudget

                                if not (tagged |> List.forall (fst >> isPlainData)) then
                                    findings <-
                                        findings
                                        @ [ Finding.make
                                                Ergonomic
                                                name
                                                $"discriminated by '{tag}', but an arm is not plain data; left as an erased union" ]

                                    None
                                else

                                let caseNames =
                                    tagged |> List.map (snd >> Naming.enumCaseOfString) |> uniqueCaseNames

                                let cases =
                                    List.map2
                                        (fun (arm, value) caseName ->
                                            let fields =
                                                fieldsOf arm
                                                |> List.map (fun m ->
                                                    let reference, refFindings =
                                                        typeRef ctx model None $"{name}.{caseName}.{m.Symbol.Name}" m.TypeId

                                                    findings <- findings @ refFindings

                                                    { Name = Naming.memberName m.Symbol.Name
                                                      Type = optionalRef m.Optional reference })

                                            { Name = caseName
                                              CompiledName = (if value = caseName then None else Some value)
                                              Fields = fields })
                                        tagged
                                        caseNames

                                findings <-
                                    findings @ [ Finding.make Exact name $"discriminated union on '{tag}' (D4)" ]

                                Some(
                                    FsTaggedUnion
                                        { Name = name
                                          Docs = ""
                                          Tags = []
                                          Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                          Tag = tag
                                          Cases = cases }
                                )
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

        // The signature's own parameters are discarded here rather than written: a delegate
        // type has nowhere to put them. `aliasTypeParams` has already hoisted them onto the
        // alias around this callback, with the rank-2 finding that records the cost.
        let _, parameters, returns, signatureFindings = shapeSignature ctx model None name signature

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
                            let typeParameters, scope, parameterFindings = aliasTypeParams ctx model name facts

                            // The signature is read under the alias's own parameters, so
                            // `Callback<T> = (self: T) => void` writes `'T` rather than widening it.
                            let reference, refFindings =
                                delegateRefFor ctx { model with TypeVars = scope } name facts

                            findings <- findings @ parameterFindings @ refFindings

                            Some(
                                FsAbbrev
                                    { Name = name
                                      Docs = ""
                                      Tags = []
                                      Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                      TypeParameters = typeParameters
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
                            // An index signature is shape too: `interface Bag { [key: string]:
                            // number }` has no properties at all, and without this it reaches
                            // `shape-aliases` looking empty and abbreviates to obj (§4.10).
                            && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
                            && (arrayElement facts).IsNone
                            && not (isTuple facts)
                            // A named instantiation - `type StringBox = Box<string>` - is an
                            // abbreviation of the application, not a second copy of the
                            // expansion the checker substituted; `shape-aliases` writes it.
                            && (instantiationOf model facts).IsNone
                            ->
                            let typeParameters, scope, parameterFindings =
                                declTypeParams ctx model name facts

                            findings <- findings @ parameterFindings

                            // Members are shaped under the declaration's own parameters, so a
                            // `T` in a member position names the variable rather than widening.
                            let members, memberFindings =
                                shapeMembers ctx { model with TypeVars = scope } name facts

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
                                      TypeParameters = typeParameters
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
                        | FsTaggedUnion decl -> [ decl.Name ]
                        | FsEnum decl -> [ decl.Name ]
                        | FsAbbrev decl -> [ decl.Name ]
                        | FsPhantom decl -> [ decl.Name ]
                        | FsMeasure decl -> [ decl.Name ]
                        | FsExports _ -> [])
                    |> Set.ofList

                let fallback = defaultExportName ctx

                // An abbreviation that stands in for an export - `type StringBox = Box<string>`
                // reaches here rather than `shape-interfaces` - still carries that export's
                // documentation; it is the only declaration the reader will see for it.
                let exportDocs =
                    model.Harvest.Exports
                    |> List.choose (fun export ->
                        Map.tryFind export.Symbol.Id model.ExportTypes
                        |> Option.bind _.Declared
                        |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                    |> Map.ofList

                // A generic declaration cannot be named bare on the right of an abbreviation -
                // F# demands the full arity - so an alias to one repeats its parameters and
                // applies them straight through: `type Alias<'T> = Primary<'T>`.
                let parametersOf =
                    model.Decls
                    |> List.choose (function
                        | FsInterface decl -> Some(decl.Name, decl.TypeParameters)
                        | FsAbbrev decl -> Some(decl.Name, decl.TypeParameters)
                        | FsPhantom decl -> Some(decl.Name, decl.TypeParameters)
                        | _ -> None)
                    |> Map.ofList

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
                                    let typeParameters =
                                        Map.tryFind primary parametersOf |> Option.defaultValue []

                                    let target =
                                        if typeParameters.IsEmpty then
                                            FsNamed primary
                                        else
                                            FsApp(primary, typeParameters |> List.map (_.Name >> FsTypeVar))

                                    Some(
                                        FsAbbrev
                                            { Name = name
                                              Docs = export.Docs
                                              Tags = export.Tags
                                              Order = export.Order
                                              TypeParameters = typeParameters
                                              Target = target }
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
                                // A branding intersection is a name and nothing else in F#: a
                                // unit of measure, spelled at the uses as `string<Name>` rather
                                // than declared as an abbreviation, because the name can only be
                                // spent once and the measure is what spends it (§4.6, D11).
                                // Decided before the reference is shaped: shaping one would ask
                                // this declaration for the measure it has not emitted yet.
                                let brand = brandedPrimitive model facts

                                if brand.IsSome then
                                    findings <-
                                        findings
                                        @ [ Finding.make
                                                Ergonomic
                                                name
                                                "branding intersection emitted as a unit of measure; uses read \
                                                 as the branded primitive (§4.6, D11)" ]

                                    Some(
                                        FsMeasure
                                            { Name = name
                                              Docs = Map.tryFind typeId exportDocs |> Option.defaultValue ("", []) |> fst
                                              Tags = Map.tryFind typeId exportDocs |> Option.defaultValue ("", []) |> snd
                                              Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                              Primitive = brand.Value }
                                    )
                                else

                                let typeParameters, scope, parameterFindings =
                                    declTypeParams ctx model name facts

                                let scoped = { model with TypeVars = scope }

                                // The named cases earlier passes handle; what reaches here is
                                // referable without a declaration of its own.
                                let reference, refFindings =
                                    match arrayElement facts with
                                    | Some element ->
                                        let inner, innerFindings = typeRef ctx scoped None name element
                                        FsArray inner, innerFindings
                                    | None ->
                                        match Map.tryFind facts.Response.Id model.DeclNames with
                                        | Some primary when primary <> name -> FsNamed primary, []
                                        | _ -> typeRefIgnoringSelf ctx scoped name facts

                                findings <- findings @ parameterFindings @ refFindings

                                let docs, tags =
                                    Map.tryFind typeId exportDocs |> Option.defaultValue ("", [])

                                let order =
                                    Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                                // A generic declaration whose right side names none of its
                                // parameters is a type-level computation the checker could not
                                // finish: `DeepPartial<T>`, `Unwrap<T>`, `` `x-${T}` ``. F# has
                                // no unused type variable in an abbreviation, so this used to be
                                // dropped outright and every use of it widened to obj. An erased
                                // phantom keeps the name and the arity - enough for uses to stay
                                // distinct - and admits, by having no members at all, that a cast
                                // is the only thing anyone can do with it (§4.10, §4.11).
                                if
                                    not typeParameters.IsEmpty
                                    && typeParameters
                                       |> List.forall (fun p -> not (Set.contains p.Name (typeVarsOf reference)))
                                then
                                    findings <-
                                        findings
                                        @ [ Finding.make
                                                Widened
                                                name
                                                "type-level computation over an unresolved operand; emitted as an \
                                                 erased phantom, which casts are the only use of" ]

                                    Some(
                                        FsPhantom
                                            { Name = name
                                              Docs = docs
                                              Tags = tags
                                              Order = order
                                              TypeParameters = typeParameters
                                              Carrier =
                                                if
                                                    flag TypeFlags.TemplateLiteral facts
                                                    || flag TypeFlags.StringMapping facts
                                                then
                                                    FsString
                                                else
                                                    FsObj }
                                    )
                                else
                                    Some(
                                        FsAbbrev
                                            { Name = name
                                              Docs = docs
                                              Tags = tags
                                              Order = order
                                              TypeParameters = typeParameters
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
                                    let typeParameters, parameters, returns, signatureFindings =
                                        shapeSignature ctx model (Some name) name signature

                                    findings <- findings @ signatureFindings

                                    index,
                                    { Name = name
                                      Docs = export.Docs
                                      Tags = export.Tags
                                      TypeParameters = typeParameters
                                      Binding = bindingOf export
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

                            let binding = bindingOf export

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
                                    let typeParameters, parameters, returns, signatureFindings =
                                        shapeSignature ctx model None name signature

                                    findings <- findings @ signatureFindings

                                    index,
                                    { Name = name
                                      Docs = export.Docs
                                      Tags = export.Tags
                                      TypeParameters = typeParameters
                                      Binding = binding
                                      Body = ExportFunction(parameters, returns) })
                            | Some facts ->
                                let reference, refFindings = typeRef ctx model None name facts.Response.Id
                                findings <- findings @ refFindings

                                [ index,
                                  { Name = name
                                    Docs = export.Docs
                                    Tags = export.Tags
                                    TypeParameters = []
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
                                   // An index signature has no name to bind a Create
                                   // parameter to, so a type carrying one is not plain data.
                                   | FsMethod _
                                   | FsIndexer _ -> false)
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
                                    | FsMethod _
                                    | FsIndexer _ -> failwith "unreachable: filtered to properties")

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
                        // Two `Item` overloads differing only in key type are legal and
                        // wanted - a type may index by both string and number.
                        | FsIndexer _ -> true
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

// ---------------------------------------------------------------------------------------------
// Arity repair: the two ways a shaped declaration can still be un-writable F#.
// ---------------------------------------------------------------------------------------------

/// Every type reference inside a reference, rebuilt through `f` (applied outside-in, so a
/// widening stops the descent). Written once because both repairs below rewrite references in
/// place, and a hand-rolled traversal per repair is how a new `FsTypeRef` case gets silently
/// skipped by one of them.
let rec private mapRef (f: FsTypeRef -> FsTypeRef) (reference: FsTypeRef) : FsTypeRef =
    let recur = mapRef f

    match f reference with
    | FsOption inner -> FsOption(recur inner)
    | FsArray element -> FsArray(recur element)
    | FsTuple elements -> FsTuple(elements |> List.map recur)
    | FsErasedUnion arms -> FsErasedUnion(arms |> List.map recur)
    | FsDelegate(arguments, returns) -> FsDelegate(arguments |> List.map recur, recur returns)
    | FsApp(name, arguments) -> FsApp(name, arguments |> List.map recur)
    | other -> other

/// The type variables a reference mentions.
/// Every type reference in a declaration, rebuilt through `f`.
let private mapDeclRefs (f: FsTypeRef -> FsTypeRef) (decl: FsDecl) : FsDecl =
    let reference = mapRef f
    let parameter (p: FsParam) = { p with Type = reference p.Type }

    let typeParam (p: FsTypeParam) =
        { p with Constraint = p.Constraint |> Option.map reference }

    let declMember =
        function
        | FsProperty p -> FsProperty { p with Type = reference p.Type }
        | FsIndexer i ->
            FsIndexer
                { i with
                    Key = reference i.Key
                    Value = reference i.Value }
        | FsMethod m ->
            FsMethod
                { m with
                    Parameters = m.Parameters |> List.map parameter
                    Return = reference m.Return }

    match decl with
    | FsInterface d ->
        FsInterface
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Inherits = d.Inherits |> List.map reference
                Members = d.Members |> List.map declMember
                CreateOverloads = d.CreateOverloads |> List.map (List.map parameter) }
    | FsAbbrev d ->
        FsAbbrev
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Target = reference d.Target }
    | FsPhantom d ->
        FsPhantom
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Carrier = reference d.Carrier }
    | FsMeasure d -> FsMeasure { d with Primitive = reference d.Primitive }
    | FsTaggedUnion d ->
        FsTaggedUnion
            { d with
                Cases =
                    d.Cases
                    |> List.map (fun case ->
                        { case with
                            Fields = case.Fields |> List.map (fun field -> { field with Type = reference field.Type }) }) }
    | FsExports members ->
        FsExports(
            members
            |> List.map (fun m ->
                { m with
                    Body =
                        match m.Body with
                        | ExportFunction(parameters, returns) ->
                            ExportFunction(parameters |> List.map parameter, reference returns)
                        | ExportValue returns -> ExportValue(reference returns)
                        | ExportConstructor(parameters, returns) ->
                            ExportConstructor(parameters |> List.map parameter, reference returns) })
        )
    | FsStringEnum _
    | FsEnum _ -> decl

/// The name a declaration is written under, for the two repairs that work by name.
let private declName =
    function
    | FsInterface d -> Some d.Name
    | FsAbbrev d -> Some d.Name
    | FsPhantom d -> Some d.Name
    | FsMeasure d -> Some d.Name
    | FsTaggedUnion d -> Some d.Name
    | FsStringEnum d -> Some d.Name
    | FsEnum d -> Some d.Name
    | FsExports _ -> None

/// The repair itself, as a plain function: the model it produces plus what it had to widen.
let private repaired (model: ShapeModel) =
        let mutable findings = []

        // Only abbreviations can hit FS0035; an interface may leave a parameter unused.
        let dropped =
            model.Decls
            |> List.choose (function
                | FsAbbrev decl when not decl.TypeParameters.IsEmpty ->
                    let used = typeVarsOf decl.Target

                    if decl.TypeParameters |> List.forall (fun p -> Set.contains p.Name used) then
                        None
                    else
                        Some decl.Name
                | _ -> None)
            |> Set.ofList

        for name in dropped do
            findings <-
                findings
                @ [ Finding.make
                        Widened
                        name
                        "generic alias dropped: its target widened away every type parameter, and F# has no \
                         unused type variable in an abbreviation" ]

        let surviving =
            model.Decls
            |> List.filter (fun decl ->
                match declName decl with
                | Some name -> not (Set.contains name dropped)
                | None -> true)

        // Arity by name, over the survivors only - a dropped alias must not look applicable.
        let arity =
            surviving
            |> List.choose (function
                | FsInterface d -> Some(d.Name, d.TypeParameters.Length)
                | FsAbbrev d -> Some(d.Name, d.TypeParameters.Length)
                | FsPhantom d -> Some(d.Name, d.TypeParameters.Length)
                | _ -> None)
            |> Map.ofList

        let decls =
            surviving
            |> List.map (fun decl ->
                let owner = declName decl |> Option.defaultValue "Exports"

                let widen (message: string) =
                    findings <- findings @ [ Finding.make Widened owner message ]
                    FsObj

                // FS0252: a settable property must have a settable type, and `unit` is not one.
                // The type is right - a `never`-typed brand or an `undefined` slot holds no
                // value - so only the setter goes, and the member still reads.
                let demoteUnitSetters (decl: FsDecl) =
                    match decl with
                    | FsInterface d ->
                        FsInterface
                            { d with
                                Members =
                                    d.Members
                                    |> List.map (function
                                        | FsProperty p when not p.ReadOnly && p.Type = FsUnit ->
                                            findings <-
                                                findings
                                                @ [ Finding.make
                                                        Ergonomic
                                                        owner
                                                        $"{p.Name} reads but does not write: its type holds no value, \
                                                          and F# has no setter of type unit" ]

                                            FsProperty { p with ReadOnly = true }
                                        | other -> other)
                                // And no Create parameter either: there is no value to pass,
                                // and writing the key as `undefined` is not what the author
                                // declared. The property still reads on the result.
                                CreateOverloads =
                                    d.CreateOverloads
                                    |> List.map (List.filter (fun p -> p.Type <> FsUnit)) }
                    | other -> other

                decl
                |> demoteUnitSetters
                |> mapDeclRefs (fun reference ->
                    match reference with
                    | FsNamed name when Set.contains name dropped ->
                        widen $"reference to the dropped generic alias {name} widened to obj"
                    | FsApp(name, _) when Set.contains name dropped ->
                        widen $"reference to the dropped generic alias {name} widened to obj"
                    | FsNamed name when Map.tryFind name arity |> Option.exists (fun n -> n > 0) ->
                        widen $"{name} is generic and this position has no arguments to apply; widened to obj"
                    | FsApp(name, arguments) when
                        Map.tryFind name arity |> Option.exists (fun n -> n <> arguments.Length)
                        ->
                        widen $"{name} applied to {arguments.Length} arguments but declares {arity[name]}; widened to obj"
                    | other -> other))

        { model with Decls = decls }, findings

/// The last two ways a shaped model still fails to be F#, both repaired by widening - the type
/// exists, but this position cannot say which instantiation of it, and `obj` is what that means.
///
/// *A generic abbreviation whose target does not mention its parameters* is FS0035: F# has no
/// unused type variables in an abbreviation. It arises when the right side widened -
/// `type Params<'P> = obj` after `P`'s only use dropped to `obj`. Dropping the parameter instead
/// would silently change the alias's arity at every application, so the declaration goes and its
/// references widen.
///
/// *A generic declaration named bare* is FS0033: `PagesFunctionContext` needs three arguments,
/// and a member of some *other* declaration has no names to write for them. §4.9 already widens
/// an out-of-scope type *variable* to `obj` for the same reason; this is that rule one level up,
/// at the declaration head.
///
/// Runs after `order-declarations`, which is what folds the export members into an `FsExports`
/// declaration: references written there need the same repair as any other.
let repairArity: Pass<ShapeModel> =
    { Name = "repair-arity"
      Run =
        fun _ model ->
            async {
                let model, findings = repaired model

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
                | FsTaggedUnion decl -> orderKey decl.Order decl.Name
                | FsEnum decl -> orderKey decl.Order decl.Name
                | FsAbbrev decl -> orderKey decl.Order decl.Name
                | FsPhantom decl -> orderKey decl.Order decl.Name
                | FsMeasure decl -> orderKey decl.Order decl.Name
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
                        | FsTaggedUnion decl -> [ decl.Name ]
                        | FsEnum decl -> [ decl.Name ]
                        | FsAbbrev decl -> [ decl.Name ]
                        | FsPhantom decl -> [ decl.Name ]
                        | FsMeasure decl -> [ decl.Name ]
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
      bindFreeTypeParams
      classifyLiteralUnions
      detectTaggedUnions
      shapeCallbacks
      shapeInterfaces
      shapeAliases
      shapeClasses
      shapeExports
      synthesizeParamObjects
      dedupeOverloads
      orderDeclarations
      repairArity
      auditCoverage ]
