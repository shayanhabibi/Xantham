/// The facts and mappings every shaping pass is written against: what a resolved type *is*
/// (literal, tuple, callback, branded primitive, constructor object, tagged union), the F#
/// reference it maps to, the type parameters it declares, and the member and signature
/// shaping the declaration passes share. Passes hold the policy; this holds what they read.
module Xantham.Generator.Shape.Spec

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

open Xantham.TypeScript.Wire.Proto

let internal hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

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
    if export.ExportName <> "default" then
        export.ExportName
    elif export.Symbol.Name = "default" || export.Symbol.Name.StartsWith "__" then
        fallback
    else
        export.Symbol.Name

// ---------------------------------------------------------------------------------------------
// Facts the passes repeatedly ask of a type.
// ---------------------------------------------------------------------------------------------

let internal flag (f: TypeFlags) (facts: TypeFacts) = facts.Response.Flags.HasFlag f

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

let internal isNullish (facts: TypeFacts) =
    flag TypeFlags.Undefined facts
    || flag TypeFlags.Null facts
    || flag TypeFlags.Void facts

/// A union's members split into the hoisted nullish part (D1) and everything else.
let internal splitNullish (model: ShapeModel) (facts: TypeFacts) =
    facts.UnionMembers
    |> List.partition (fun id ->
        match Map.tryFind id model.Types with
        | Some m -> isNullish m
        | None -> false)

/// `true | false` after nullish hoisting: TS re-expands `boolean` inside larger unions, and
/// the pair is just `bool` again.
let internal isBooleanPair (model: ShapeModel) (memberIds: int list) =
    memberIds.Length = 2
    && memberIds
       |> List.forall (fun id ->
           match Map.tryFind id model.Types with
           | Some m -> flag TypeFlags.BooleanLiteral m
           | None -> false)

/// A union candidate's non-nullish member ids, sorted for member-set comparison.
let internal nonNullishMemberSet (model: ShapeModel) (candidate: TypeFacts) =
    candidate.UnionMembers
    |> List.filter (fun id ->
        match Map.tryFind id model.Types with
        | Some m -> not (isNullish m)
        | None -> true)
    |> List.sort

/// The declared union whose non-nullish member set matches, if any: what lets an
/// `"ms" | "s" | undefined` member position resolve to the exported `TimeUnit` rather than a
/// synthesized twin (literal types are interned, so the ids match across positions).
let internal namedUnionByMembers (model: ShapeModel) (memberIds: int list) : string option =
    let wanted = List.sort memberIds

    model.DeclNames
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.tryPick (fun (typeId, name) ->
        match Map.tryFind typeId model.Types with
        | Some candidate when flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate) ->
            if nonNullishMemberSet model candidate = wanted then
                Some name
            else
                None
        | _ -> None)

/// A *constructor object* (§4.4): the static side of a class, and the type `typeof X` names at
/// a member position. It carries construct signatures; its properties are the class's statics,
/// and `prototype` is the instance side, which is a declaration of its own.
///
/// A group resolved identity-only has no signatures at all, so a lib or dependency type never
/// reads as one here - which is what keeps this from claiming to know a shape it never asked
/// the checker for.
let internal isConstructorObject (facts: TypeFacts) =
    flag TypeFlags.Object facts && not facts.ConstructSignatures.IsEmpty

/// An object type that is only a callback: call signatures and nothing else worth keeping.
let internal isPureCallback (facts: TypeFacts) =
    not facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty
    && facts.Members.IsEmpty

/// The compiler's array types, recognized by identity so the check holds for every group
/// disposition (the entry package cannot declare a global `Array`).
let internal arrayElement (facts: TypeFacts) =
    match facts.SymbolName, facts.TypeArguments with
    | Some("Array" | "ReadonlyArray"), [ element ] -> Some element
    | _ -> None

/// A symbol name the checker made up for an anonymous shape rather than one the author wrote.
/// Module symbols are named by their quoted file path, which is no name either.
let internal isSyntheticName (name: string) =
    name.StartsWith "__" || name.StartsWith "\""

/// A member keyed by a JS well-known symbol (`__@iterator@<id>`): unrepresentable in F#, and
/// the embedded checker id is session-specific - keeping one would also break determinism.
let internal isSymbolKeyed (name: string) = name.StartsWith "__@"

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
let internal taggedUnionShape (model: ShapeModel) (facts: TypeFacts) : (string * (TypeFacts * string) list) option =
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
            let tagged =
                members
                |> List.map (fun m -> tagValue m tag |> Option.map (fun value -> m, value))

            if tagged |> List.forall Option.isSome then
                let tagged = tagged |> List.map Option.get
                let values = tagged |> List.map snd

                // Two members sharing a tag value are not discriminated by it - matching on
                // that case could not tell them apart.
                if List.distinct values = values then
                    Some(tag, tagged)
                else
                    None
            else
                None)

/// A property that exists only to make a type nominal: keyed by a unique symbol, so nothing
/// can name it; named with a leading underscore, so nothing is meant to; or typed `never`, so
/// nothing can construct it. An object whose every property is one of these carries nothing at
/// runtime, which is what separates a branding intersection from a shape (§4.6).
let internal isMarkerMember (model: ShapeModel) (m: ResolvedMember) =
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
let rec internal brandedPrimitive (model: ShapeModel) (facts: TypeFacts) =
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
                   not (flag TypeFlags.Intersection arm)
                   || Map.containsKey arm.Response.Id model.DeclNames)
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
                    if flag TypeFlags.Boolean only || flag TypeFlags.BooleanLiteral only then
                        Some FsBool
                    elif flag TypeFlags.String only || flag TypeFlags.StringLiteral only then
                        Some FsString
                    elif flag TypeFlags.Number only || flag TypeFlags.NumberLiteral only then
                        Some FsFloat
                    else
                        None
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
let internal ErasedUnionArity = 4

/// The widest tagged-union case worth generating. A DU case binds its fields positionally, so
/// past a dozen every `match` clause is a wall of wildcards and the erased union over the arm
/// interfaces - which keeps the properties named - reads better.
[<Literal>]
let internal TaggedCaseFieldBudget = 12

/// A tuple type (§4.12). Fable compiles an F# tuple to a JS array, so a fixed tuple is an
/// exact match; the variadic forms are not.
let internal isTuple (facts: TypeFacts) =
    facts.Response.IsTupleType = ValueSome true

/// A tuple element the checker marked `...rest` or variadic. F# tuples are fixed-arity, so a
/// tuple carrying one has no tuple form at all.
let internal isVariadicElement (flags: ElementFlags) =
    flags.HasFlag ElementFlags.Rest || flags.HasFlag ElementFlags.Variadic

/// A tuple's element flags, one per type argument. The wire reports them off the tuple's
/// target and the two can only disagree if the schema changes under us; a disagreement reads
/// every element as required, which is the conservative shape.
let internal tupleElementFlags (facts: TypeFacts) =
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
let internal instantiationOf (model: ShapeModel) (facts: TypeFacts) =
    // Only a *reference* - `Ready<T>` over an interface or class - is an application. An
    // anonymous object type instantiated in some other scope also carries its original as a
    // target, but no arguments to write it with, so it is declared on its own as before.
    let isReference =
        facts.Response.ObjectFlags
        |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
        |> ValueOption.defaultValue false

    match facts.Response.Target with
    | ValueSome target when isReference && target <> facts.Response.Id ->
        Map.tryFind target model.DeclNames
        |> Option.map (fun name -> name, facts.TypeArguments)
    | _ -> None

/// The arguments a generic declaration stands for when it is named at a reference position:
/// its own parameters. F# has no bare spelling for a generic type, so the self-reference in
/// `map(next: T): Box<T>` has to re-apply them to come back out as it was written.
let internal ownArguments (facts: TypeFacts) =
    match facts.Response.Target with
    | ValueSome target when target = facts.Response.Id -> facts.TypeArguments
    | _ -> []

/// The parameters a hoisted anonymous declaration reads from the scope it was written in
/// (§4.9, `DeclParams`) - what a reference to it applies back, and what its declaration binds
/// beside any parameters of its own.
let internal freeParamsOf (model: ShapeModel) (typeId: int) =
    Map.tryFind typeId model.DeclParams |> Option.defaultValue []

/// An intersection of object types that flattens into one interface (§4.6): not a brand, and
/// carrying the members the resolve tier read off it - which it only does when every operand
/// is an object, so a primitive or type-parameter operand leaves this false.
let internal isFlattenable (model: ShapeModel) (facts: TypeFacts) =
    flag TypeFlags.Intersection facts
    && (brandedPrimitive model facts).IsNone
    && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)

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
type internal Descent() =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private path: System.Collections.Generic.HashSet<int>

    static member Path =
        if isNull Descent.path then
            Descent.path <- System.Collections.Generic.HashSet<int>()

        Descent.path

/// The F# type written at a reference position, with the findings any widening produces.
/// `self` is the name of the declaration being shaped, so a polymorphic `this` return can
/// resolve to it. Flag-test order matters: `boolean` (a union wearing the Boolean flag) before
/// the union case, unions before the literal tests, literals before their base primitives, and
/// `unique symbol` before `symbol` - the two are distinct bits, but reading them in that order
/// keeps each pair's message about the narrower construct.
let rec typeRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (typeId: int)
    : FsTypeRef * Finding list =
    if not (Descent.Path.Add typeId) then
        FsObj, [ Finding.make owner TypeReference.SelfReferenceThroughUnnamed ]
    else
        try
            typeRefOnPath ctx model self owner typeId
        finally
            Descent.Path.Remove typeId |> ignore

and internal typeRefOnPath
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (typeId: int)
    : FsTypeRef * Finding list =
    match Map.tryFind typeId model.Types with
    | None ->
        match Map.tryFind typeId model.NotFollowed with
        | Some reason -> FsObj, [ Finding.make owner (TypeReference.TypeNotResolved reason) ]
        | None -> FsObj, [ Finding.make owner (TypeReference.MissingFromTypeTable typeId) ]
    | Some facts ->
        let has f = flag f facts

        if has TypeFlags.Boolean then
            FsBool, []
        elif has TypeFlags.Union && (brandedPrimitive model facts).IsSome then
            intersectionRef ctx model self owner facts
        elif has TypeFlags.Union then
            unionRef ctx model self owner facts
        elif has TypeFlags.BooleanLiteral then
            FsBool, []
        elif has TypeFlags.EnumLiteral then
            match literalOf facts with
            | Some(LitNumber _) -> FsFloat, [ Finding.make owner TypeReference.LoneEnumMemberToFloat ]
            | _ -> FsString, [ Finding.make owner TypeReference.LoneEnumMemberToString ]
        elif has TypeFlags.StringLiteral then
            FsString, [ Finding.make owner TypeReference.StringLiteralToString ]
        elif has TypeFlags.NumberLiteral then
            FsFloat, [ Finding.make owner TypeReference.NumericLiteralToFloat ]
        elif has TypeFlags.BigIntLiteral then
            FsBigInt, [ Finding.make owner TypeReference.BigIntLiteralToBigInt ]
        elif has TypeFlags.String then
            FsString, []
        elif has TypeFlags.Number then
            FsFloat, []
        elif has TypeFlags.BigInt then
            // Exact, and the one intrinsic here whose mapping costs nothing: F# `bigint` is
            // the native JavaScript `BigInt` under Fable 5 (proven by the run gate, not by
            // the compile gate - the F# type says nothing about what the erasure did).
            FsBigInt, []
        elif has TypeFlags.TemplateLiteral then
            // `` `on${string}` `` is a string at runtime, and the generator already knows how
            // to keep that much: the same trade TR006 makes for a string *literal* type
            // (§4.11). What is lost is the pattern, not the type - widening to `obj` threw
            // away both. A *closed* template literal never reaches here: the checker expands
            // one over finite unions into its union of literals, which takes the StringEnum
            // path and stays exact.
            FsString, [ Finding.make owner TypeReference.TemplateLiteralToString ]
        elif has TypeFlags.StringMapping then
            // `Uppercase<T>` over an operand the checker could not finish. Same argument: the
            // result is a string, and only the transform is lost.
            FsString, [ Finding.make owner TypeReference.StringMappingToString ]
        elif has TypeFlags.NonPrimitive then
            // TypeScript's `object` - anything that is not a primitive. `obj` is the mapping
            // §4.1 asks for and there is no closer one, but it is still a widening in the
            // direction that matters: `obj` admits the primitives `object` was written to
            // exclude. Reported as that, rather than as an unmapped flag.
            FsObj, [ Finding.make owner TypeReference.ObjectTypeToObj ]
        elif has TypeFlags.UniqueESSymbol then
            // A `unique symbol` is a nominal singleton. Nothing shipped binds even the
            // ordinary one (below), and F# has no form for the identity on top of it, so
            // both halves of the loss are named.
            FsObj, [ Finding.make owner TypeReference.UniqueSymbolNoBinding ]
        elif has TypeFlags.ESSymbol then
            // `symbol`. §4.1 wanted `JS.Symbol`, but Fable.Core 5.2.0 declares no such type -
            // checked against the shipped assembly rather than recalled - and inventing a
            // binding for a name the pinned package does not have is what the compile gate
            // exists to catch. Widened, with the reason.
            FsObj, [ Finding.make owner TypeReference.SymbolNoBinding ]
        elif has TypeFlags.Void || has TypeFlags.Undefined || has TypeFlags.Never then
            FsUnit, []
        elif has TypeFlags.Any then
            FsObj, [ Finding.make owner TypeReference.AnyToObj ]
        elif has TypeFlags.Unknown then
            FsObj, [ Finding.make owner TypeReference.UnknownToObj ]
        elif has TypeFlags.TypeParameter then
            if facts.Response.IsThisType = ValueSome true then
                match self with
                | Some name -> FsNamed name, [ Finding.make owner TypeReference.PolymorphicThisAsDeclaringType ]
                | None -> FsObj, [ Finding.make owner TypeReference.ThisOutsideDeclaration ]
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
                            [ Finding.make owner (TypeReference.TypeParameterOutOfScopeToConstraint name) ]
                        | None -> FsObj, [ Finding.make owner TypeReference.TypeParameterOutOfScope ]
        elif has TypeFlags.Object then
            objectRef ctx model self owner facts
        elif has TypeFlags.Index then
            keyOfRef model owner facts
        elif has TypeFlags.IndexedAccess then
            indexedAccessRef model owner facts
        elif has TypeFlags.Intersection then
            intersectionRef ctx model self owner facts
        else
            FsObj,
            [
                Finding.make owner (TypeReference.TypeFlagsNotMapped(string facts.Response.Flags))
            ]

/// `keyof T` at an operand the checker could not finish (§4.10). A closed `keyof` never gets
/// here - the checker hands those back already expanded into their union of literal keys, which
/// shapes as a StringEnum - so this is the open regime, where the only honest carrier is the
/// support package's `keyof<'T>`: erased to the string it is at runtime, and phantom-typed by
/// the operand so a key of one type cannot be passed where another's is wanted.
and internal keyOfRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let operand =
        facts.Response.Target
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id model.TypeVars)

    match operand with
    | Some name -> FsApp("keyof", [ FsTypeVar name ]), [ Finding.make owner (TypeReference.KeyOfOpenOperand name) ]
    | None -> FsObj, [ Finding.make owner TypeReference.KeyOfOperandOutOfScope ]

/// An intersection at a reference position. A brand (§4.6, D11) is the one intersection F# can
/// state exactly: the measure its declaration emitted, applied to the primitive it brands, which
/// enforces the same nominality TypeScript was buying and erases the same way. It costs no
/// finding here - the declaration records the idiom once - but a brand that never got a
/// declaration has no measure to name, and falls back to the bare primitive loudly. Intersections
/// of object types are a separate mapping (§4.6's first bullet) and are not shaped yet.
and internal intersectionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    match brandedPrimitive model facts with
    | Some primitive ->
        match Map.tryFind facts.Response.Id model.DeclNames with
        | Some name -> FsBranded(primitive, name), []
        | None -> primitive, [ Finding.make owner TypeReference.UnnamedBrandToPrimitive ]
    | None ->
        // A flattened intersection is declared under a name (§4.6), exactly as a hoisted
        // anonymous object is, and is applied over the parameters it reads the same way.
        match Map.tryFind facts.Response.Id model.DeclNames with
        | Some name when isFlattenable model facts ->
            match freeParamsOf model facts.Response.Id with
            | [] -> FsNamed name, []
            | arguments -> appliedRef ctx model self owner name arguments
        | _ ->
            let reason =
                if facts.Members.IsEmpty && facts.IndexInfos.IsEmpty then
                    TypeReference.IntersectionOverNonObject
                else
                    TypeReference.IntersectionNotDeclared

            FsObj, [ Finding.make owner reason ]

/// `T[K]`. Where `K` is a key variable this signature bound as `typekeyof<'T,'R>`, the access is
/// exactly the `'R` that idiom introduced. Everything else - `T[keyof T]`, an access over an
/// operand not in scope - is a type-level computation with no F# form, and widens loudly.
and internal indexedAccessRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
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
    | _ -> FsObj, [ Finding.make owner TypeReference.IndexedAccessNoForm ]

and internal objectRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
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
                | Some(name, arguments) ->
                    let parameters =
                        facts.Response.Target
                        |> ValueOption.toOption
                        |> Option.bind (fun target -> Map.tryFind target model.Types)
                        |> Option.map ownArguments
                        |> Option.defaultValue []

                    appliedRefTo ctx model self owner name parameters arguments
                | None ->
                    match libBinding ctx model self owner facts with
                    | Some result -> result
                    | None ->

                        match GeneratorConfig.disposition ctx.Config facts.Origin, facts.SymbolName with
                        | Reference, Some typeName ->
                            // The O7 contract: a `ship` run of this group produces exactly this name.
                            FsNamed $"{Naming.groupModule ctx.PackageName facts.Origin}.{typeName}", []
                        | Reference, None -> FsObj, [ Finding.make owner TypeReference.AnonymousInReferencedGroup ]
                        | (Ship | Widen), Some "globalThis" ->
                            FsObj, [ Finding.make owner TypeReference.GlobalThisToObj ]
                        | (Ship | Widen), _ when isConstructorObject facts ->
                            // A constructor object this run did not name: the generic message
                            // would say `__type is not among the generated declarations`, which
                            // names the checker's placeholder rather than the construct.
                            let constructs =
                                facts.ConstructSignatures
                                |> List.tryPick (fun signature -> Map.tryFind signature.ReturnTypeId model.DeclNames)
                                |> Option.orElse (facts.SymbolName |> Option.filter (isSyntheticName >> not))
                                |> Option.defaultValue "an anonymous class"

                            FsObj, [ Finding.make owner (TypeReference.ConstructorObjectNotDeclared constructs) ]
                        | (Ship | Widen), _ ->
                            let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
                            FsObj, [ Finding.make owner (TypeReference.NotAmongGeneratedDeclarations shown) ]

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
and internal libBinding (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) =
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
                    [
                        Finding.make
                            owner
                            (TypeReference.LibExtraTypeArgumentsDropped(name, arguments.Length, fsharpName, arity))
                    ]
                else
                    []

            let lossy =
                loss
                |> List.map (fun note -> Finding.make owner (TypeReference.LibBindingLoss note))

            Some(reference, findings @ dropped @ lossy)
    | _ -> None

/// A generic name applied to type arguments, each shaped at this position (§4.9).
and internal appliedRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (name: string)
    (arguments: int list)
    : FsTypeRef * Finding list =
    appliedRefTo ctx model self owner name [] arguments

/// An application of a generic declaration whose own parameters are known, so that each
/// argument can be checked against the constraint the declaration will state (§4.9). Where
/// the declaration writes `'Event :> Event`, F# rejects `Listener<obj>` outright, and a type
/// variable without that constraint just the same - so an argument that widened to `obj`, or
/// that is a variable bound without the constraint (a `typekeyof` result, another
/// declaration's unconstrained parameter), is written as the constraint itself: the tightest
/// thing still true of it, as an out-of-scope parameter already is. Only a constraint that
/// the declaration states - a plain named interface of this run - is substituted; one it
/// dropped needs no help.
///
/// A *named* argument is the third case, and F# nominal subtyping is what makes it one:
/// `WorkerGlobalScopeEventMap` has the members `EventCurrentTargetItem`'s index signature asks
/// for, which is all TypeScript wants, but it does not inherit it, so `EventTarget<
/// WorkerGlobalScopeEventMap>` is FS0001. It goes the same way as the other two - written as
/// the constraint, with a finding - and "is a subtype" is read off the declared bases rather
/// than assumed, since that is exactly the relation `shape-interfaces` emits as `inherit`.
and internal appliedRefTo
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (name: string)
    (parameters: int list)
    (arguments: int list)
    : FsTypeRef * Finding list =
    let mutable findings = []

    let statedConstraint (typeId: int) =
        Map.tryFind typeId model.Types
        |> Option.bind _.Constraint
        |> Option.filter (fun boundId -> boundId <> typeId)
        |> Option.bind (fun boundId ->
            match Map.tryFind boundId model.Types with
            | Some bound when
                flag TypeFlags.Object bound
                && (ownArguments bound).IsEmpty
                && (arrayElement bound).IsNone
                && not (isTuple bound)
                && not (isPureCallback bound)
                ->
                Map.tryFind boundId model.DeclNames |> Option.map (fun name -> boundId, name)
            | _ -> None)

    /// Whether an argument is the bound, or reaches it through the bases and intersection
    /// operands `shape-interfaces` turns into `inherit` lines - the F# subtyping the
    /// application needs. An instantiation is asked of its target, which is the declaration
    /// that carries the heritage.
    let satisfies (boundId: int) (argument: int) =
        let boundName = Map.tryFind boundId model.DeclNames

        let rec walk seen typeId =
            typeId = boundId
            || (boundName.IsSome && Map.tryFind typeId model.DeclNames = boundName)
            || (not (Set.contains typeId seen)
                && (match Map.tryFind typeId model.Types with
                    | Some facts ->
                        facts.BaseTypes
                        @ facts.IntersectionMembers
                        @ (facts.Response.Target |> ValueOption.toOption |> Option.toList)
                        |> List.exists (walk (Set.add typeId seen))
                    | None -> false))

        walk Set.empty argument

    let parameters =
        if parameters.Length = arguments.Length then
            parameters |> List.map Some
        else
            arguments |> List.map (fun _ -> None)

    let mapped =
        List.zip parameters arguments
        |> List.map (fun (parameter, argument) ->
            let reference, argumentFindings = typeRef ctx model self owner argument
            findings <- findings @ argumentFindings

            match parameter |> Option.bind statedConstraint, reference with
            | Some(_, bound), FsObj ->
                findings <-
                    findings
                    @ [ Finding.make owner (TypeReference.ConstrainedArgumentWidened(name, bound)) ]

                FsNamed bound
            | Some(boundId, bound), FsTypeVar variable when statedConstraint argument <> Some(boundId, bound) ->
                findings <-
                    findings
                    @ [
                        Finding.make owner (TypeReference.ArgumentNotBoundWithConstraint(variable, name, bound))
                    ]

                FsNamed bound
            | Some(boundId, bound), (FsNamed shown | FsApp(shown, _)) when
                shown <> bound && not (satisfies boundId argument)
                ->
                findings <-
                    findings
                    @ [
                        Finding.make owner (TypeReference.ArgumentNotASubtypeOfConstraint(shown, name, bound))
                    ]

                FsNamed bound
            | _ -> reference)

    FsApp(name, mapped), findings

and internal tupleRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
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

        FsArray element, findings @ [ Finding.make owner reason ]

    if tupleElementFlags facts |> List.exists isVariadicElement then
        widenToArray TypeReference.TupleRestToArray
    else
        match components with
        // F# has no zero- or one-component tuple, so neither maps; an array is the honest
        // shape for both, and both are vanishingly rare.
        | []
        | [ _ ] -> widenToArray (TypeReference.TupleArityNoForm components.Length)
        | components -> FsTuple components, findings

/// A callback as a delegate (D5): guaranteed arity at the boundary. Only the first signature
/// shapes the delegate; further overloads on a callback are a finding.
and internal delegateRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    match facts.CallSignatures with
    | [] -> FsObj, [ Finding.make owner TypeReference.CallableWithoutSignatures ]
    | signature :: rest ->
        let mutable findings =
            if rest.IsEmpty then
                []
            else
                [
                    Finding.make owner (TypeReference.CallbackOverloadsFromFirst(rest.Length + 1))
                ]

        let parameters =
            signature.Parameters
            |> List.map (fun p ->
                let reference, refFindings =
                    typeRef ctx model self $"{owner}({p.Symbol.Name})" p.TypeId

                findings <- findings @ refFindings
                optionalRef (isOptionalParam p reference) reference)

        let returns, returnFindings =
            typeRef ctx model self $"{owner}()" signature.ReturnTypeId

        FsDelegate(parameters, returns), findings @ returnFindings

/// A union hoists its `null`/`undefined` members into `option` (D1). What remains resolves as
/// a single member, a named literal union (classified by `classify-literal-unions`), or widens
/// - position-aware union treatment (D4) is phase C.
and internal unionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
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

            wrapped, Finding.make owner TypeReference.NullableHoistedToOption :: findings

    match remaining with
    | [] -> FsUnit, [ Finding.make owner TypeReference.OnlyNullUndefinedToUnit ]
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
and internal erasedUnionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (memberIds: int list)
    : FsTypeRef * Finding list =
    let mutable findings = []

    let arms =
        memberIds
        |> List.map (fun id ->
            let reference, refFindings = typeRef ctx model self owner id
            findings <- findings @ refFindings
            reference)
        |> List.distinct

    match arms with
    | [] -> FsObj, findings @ [ Finding.make owner TypeReference.EmptyUnionToObj ]
    | [ single ] -> single, findings
    | arms when arms |> List.contains FsObj -> FsObj, findings @ [ Finding.make owner TypeReference.UnionWithObjArm ]
    | arms when arms.Length <= ErasedUnionArity -> FsErasedUnion arms, findings
    | arms ->
        FsObj,
        findings
        @ [
            Finding.make owner (TypeReference.UnionTooWide(arms.Length, ErasedUnionArity))
        ]

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
and internal isOptionalParam (p: ResolvedMember) (reference: FsTypeRef) =
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
let internal typeParamsOf
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
                findings <- findings @ [ Finding.make owner (TypeParameters.UnnamedTypeParameter id) ]

                None)

    // Layered onto whatever is already in scope rather than replacing it: a generic *method*
    // binds its own parameters on top of its declaration's, and `read<K extends keyof T>` has
    // to see both.
    let scope =
        named
        |> List.fold (fun bound (id, name) -> Map.add id name bound) model.TypeVars

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

                {
                    Name = name
                    Constraint = Some reference
                }
            | Some _ ->
                findings <- findings @ [ Finding.make owner (TypeParameters.ConstraintDropped name) ]

                { Name = name; Constraint = None }
            | None -> { Name = name; Constraint = None })

    parameters, scope, findings

/// The ids a declaration binds: its own where it is a genuine generic declaration, and the
/// alias's where it is a generic *alias*. `type Mapper<T> = (t: T) => T` leaves the function
/// type itself parameterless - the alias is the only place `T` appears - so both are read.
let internal declParamIds (facts: TypeFacts) =
    (facts.Response.TypeParameters
     |> ValueOption.map Array.toList
     |> ValueOption.defaultValue [])
    @ facts.AliasTypeArguments
    |> List.distinct

/// The parameters a declaration binds on its left side.
let internal declTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    declParamIds facts @ freeParamsOf model facts.Response.Id
    |> List.distinct
    |> typeParamsOf ctx model owner

/// The parameters a callback alias binds, which include the signature's own. F# has no rank-2
/// form, so a generic *function type* - `type F = <T>(t: T) => T`, where each caller picks `T`
/// - can only be approximated by hoisting the variable onto the alias, and that shift is worth
/// a finding. A generic alias to a plain function type binds nothing extra and costs nothing.
let internal aliasTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    let declared = declParamIds facts
    let hoisted = facts.CallSignatures |> List.collect _.TypeParameters |> List.distinct

    let parameters, scope, findings =
        declared @ hoisted |> List.distinct |> typeParamsOf ctx model owner

    let hoistFindings =
        if hoisted |> List.exists (fun id -> not (List.contains id declared)) then
            [ Finding.make owner TypeParameters.GenericFunctionHoisted ]
        else
            []

    parameters, scope, findings @ hoistFindings

/// The type variables a rendered reference actually names.
let rec internal typeVarsOf (reference: FsTypeRef) : Set<string> =
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
let internal keyCandidates (model: ShapeModel) (ids: int list) : (int * int) list =
    ids
    |> List.choose (fun id ->
        match Map.tryFind id model.Types |> Option.bind _.Constraint with
        | None -> None
        | Some boundId ->
            match Map.tryFind boundId model.Types with
            | Some bound when flag TypeFlags.Index bound ->
                bound.Response.Target
                |> ValueOption.toOption
                |> Option.map (fun operand -> id, operand)
            | _ -> None)

/// Whether any of `roots` reaches the indexed access `object[key]` - what tells `key: K` apart
/// from `key: K` *plus* the value it selects. Carriers are followed, members are not: the point
/// is to find `T[K]` where a signature returns it, bare or wrapped, not to walk object graphs.
let internal mentionsAccess (model: ShapeModel) (objectId: int) (keyId: int) (roots: int list) : bool =
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
                        [
                            yield! facts.TypeArguments
                            yield! facts.UnionMembers
                            yield! facts.AliasTypeArguments
                            for info in facts.IndexInfos -> info.ValueTypeId
                            for signature in facts.CallSignatures do
                                yield! signature.Parameters |> List.map _.TypeId
                                yield signature.ReturnTypeId
                        ]

                    go (Set.add id visited) (rest @ carried)

    go Set.empty roots

/// The name to write the value a key selects under: `R`, unless something in scope already
/// answers to it - a generated variable that shadows one the signature also mentions would
/// silently retype it.
let internal resultName (taken: Set<string>) =
    let rec pick n =
        let candidate = if n = 0 then "R" else $"R{n}"

        if Set.contains candidate taken then
            pick (n + 1)
        else
            candidate

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
let internal shapeSignature
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

    let roots = (signature.Parameters |> List.map _.TypeId) @ [ signature.ReturnTypeId ]

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
                @ [
                    Finding.make owner (TypeParameters.KeyWithIndexedAccess(operandName, result))
                ]
        else
            keyVars <- Map.add key (KeyOf operandName) keyVars

            keyFindings <- keyFindings @ [ Finding.make owner (TypeParameters.KeyOverOperand operandName) ]

    let typeParameters = typeParameters @ looseParameters @ resultParameters

    let model =
        { model with
            TypeVars = scope
            KeyVars = keyVars
        }

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
    // A `[<ParamArray>]` parameter is not optional, and it is last, so a signature with a rest
    // parameter has no tail for `?` to go on: `setTimeout(callback, ?msDelay, ...args)` is
    // FS1212 as surely as `?a, b` is, and `msDelay` stays required, of `option` type.
    let optionalTail =
        if signature.HasRest then
            0
        else
            referenced
            |> List.rev
            |> List.takeWhile (fun (_, _, _, _, admitsOptional) -> admitsOptional)
            |> List.length

    let parameters =
        referenced
        |> List.mapi (fun i (p, paramOwner, reference, rest, admitsOptional) ->
            let inTail =
                i >= parameterCount - (if signature.HasRest then 1 else 0) - optionalTail

            let optional = admitsOptional && inTail

            if p.Optional then
                findings <- findings @ [ Finding.make paramOwner Members.OptionalParameterAsOption ]

            {
                Name = Naming.memberName p.Symbol.Name
                Optional = optional
                Rest = rest
                Type = optionalRef admitsOptional reference
            })

    let returns, returnFindings =
        typeRef ctx model self $"{owner}()" signature.ReturnTypeId

    findings <- findings @ returnFindings

    // A parameter no rendered position names has been erased - every use of it widened to obj
    // on the way here - and writing `<'T>` over a signature that mentions no `'T` says the
    // member is generic when nothing about it is. Drop it, and say so.
    let named =
        parameters
        |> List.map _.Type
        |> List.fold (fun acc t -> Set.union acc (typeVarsOf t)) (typeVarsOf returns)

    let live, erased =
        typeParameters |> List.partition (fun p -> Set.contains p.Name named)

    for p in erased do
        findings <- findings @ [ Finding.make owner (TypeParameters.TypeParameterErased p.Name) ]

    live, parameters, returns, findings

/// The interface members of an object type: methods for method symbols (each call signature an
/// overload), properties otherwise, callbacks as delegate-typed properties (D5).
let internal shapeMembers
    (ctx: Context)
    (model: ShapeModel)
    (self: string)
    (facts: TypeFacts)
    : FsMember list * Finding list =
    let mutable findings = []
    let emit finding = findings <- findings @ [ finding ]

    let members =
        facts.Members
        |> List.filter (fun m ->
            if isSymbolKeyed m.Symbol.Name then
                // The name is cut at the checker id (`__@iterator@1469` -> `__@iterator`):
                // the id is session-specific and would break run-to-run determinism.
                let stable = m.Symbol.Name.Substring(0, m.Symbol.Name.LastIndexOf '@')
                emit (Finding.make $"{self}.{stable}" Members.SymbolKeyedMemberDropped)
                false
            elif isConstructorObject facts && m.Symbol.Name = "prototype" then
                // On a constructor object, `prototype` is the instance side, which is a
                // declaration of its own - `shape-classes` drops it for the same reason.
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
                        {
                            Name = Naming.memberName m.Symbol.Name
                            Docs = m.Docs
                            Tags = m.Tags
                            TypeParameters = typeParameters
                            Parameters = parameters
                            Return = returns
                        })
            | None ->
                let reference, refFindings = typeRef ctx model (Some self) owner m.TypeId
                findings <- findings @ refFindings

                if m.Optional then
                    emit (Finding.make owner Members.OptionalMemberAsOption)

                [
                    FsProperty
                        {
                            Name = Naming.memberName m.Symbol.Name
                            Docs = m.Docs
                            Tags = m.Tags
                            ReadOnly = m.ReadOnly
                            Type = optionalRef m.Optional reference
                        }
                ])

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

            emit (Finding.make owner Members.IndexSignatureAsIndexer)

            FsIndexer
                {
                    Key = key
                    Value = value
                    ReadOnly = info.IsReadonly
                })

    // A constructor object's construct signatures become `[<EmitConstructor>] Create` members
    // (§4.4). They come last so the declaration reads properties first, exactly as the static
    // side is written in TypeScript, and so an existing golden's member order only grows.
    let constructors =
        facts.ConstructSignatures
        |> List.map (fun signature ->
            let owner = $"{self}.Create"

            let typeParameters, parameters, returns, signatureFindings =
                shapeSignature ctx model (Some self) owner signature

            findings <- findings @ signatureFindings

            FsConstructor
                {
                    Docs = ""
                    Tags = []
                    TypeParameters = typeParameters
                    Parameters = parameters
                    Return = returns
                })

    members @ indexers @ constructors, findings

/// Case names must be unique within one DU; a later duplicate takes a numeric suffix in member
/// order, deterministically.
let uniqueCaseNames (names: string list) =
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
