/// The facts and mappings every shaping pass is written against: what a resolved type *is*
/// (literal, tuple, callback, branded primitive, constructor object, tagged union), the F#
/// reference it maps to, its declared type parameters, and shared member and signature shaping.
module Xantham.Generator.Shape.Spec

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Measure

let internal hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// F# identifiers a bare single-case `[<StringEnum>]` case collides with if written without
/// `RequireQualifiedAccess`: the core library's own single-case union members.
let reservedCaseNames =
    set [ "Ok"; "Error"; "Some"; "None"; "ValueSome"; "ValueNone" ]

/// The fallback name for a default export - `Naming.defaultExport` over the entry package.
let defaultExportName (ctx: Context) = Naming.defaultExport ctx.PackageName

/// How a harvested value binds in JavaScript. An ambient global is already on `globalThis`, so
/// it takes `[<Global>]`; a module export carries an import, and an ambient module's export
/// carries the specifier its declaration quotes.
let bindingOf (export: HarvestedExport) =
    match export.Origin with
    | FromGlobal -> GlobalName export.ExportName
    | FromAmbientModule specifier -> ImportFrom(export.ExportName, specifier)
    | FromModule when export.ExportName = "default" -> ImportDefault
    | FromModule -> ImportNamed export.ExportName

/// The F# name a harvested export generates under: the exported name; a default export takes
/// its declaring symbol's name - except `export default function name` binds the symbol itself
/// as `default`, which falls back to the package-derived name.
let fsName (fallback: string) (export: HarvestedExport) =
    if export.ExportName <> "default" then
        export.ExportName
    elif
        export.Symbol.Name = "default"
        || export.Symbol.Name.StartsWith "__"
        || export.Symbol.Name.StartsWith "\""
    then
        fallback
    else
        export.Symbol.Name

/// The name a shape reached at `segment` of `owner` declares under: `Widget` and `Options` give
/// `Widget.Options`, which `render-source` writes as `type Options` inside `module Widget`. The
/// result carries a dot only where both ends take the identifier shape a module opens under;
/// a JavaScript key like `beta channel` concatenates and stays one backticked name.
let nestUnder (owner: string) (segment: string) =
    let leaf = owner.Split '.' |> Array.last

    if Naming.nestable leaf && Naming.nestable segment then
        $"{owner}.{segment}"
    else
        owner + segment

/// The source namespace's dotted F# module path, keyed by the owning symbol reference.
let internal namespacePath namespaces parent =
    parent
    |> Option.bind (fun id -> Map.tryFind id namespaces)
    |> Option.map (fun (value: string<symbolName>) ->
        (value / uom<symbolName>).Split '.'
        |> Array.map Naming.pascalSegment
        |> String.concat ".")

/// The public path of an export occurrence, which can differ from its source namespace.
let internal exportNamespacePath namespaces (export: HarvestedExport) =
    match export.NamespacePath with
    | Some path -> Some(path.Split '.' |> Array.map Naming.pascalSegment |> String.concat ".")
    | None -> namespacePath namespaces export.DeclaringNamespace

/// Places a declaration inside its source namespace, when it has one.
let internal underNamespace path name =
    match path with
    | Some ns -> nestUnder ns name
    | None -> name

/// The declaration's source namespace path followed by its name.
let internal namespaceQualified (facts: TypeFacts) name =
    (facts.NamespacePath |> List.map Naming.pascalSegment) @ [ name ]
    |> String.concat "."

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

/// The TypeScript spelling of a literal: `2`, `0.5`, `"method"`.
let typeScriptLiteral (literal: FsLiteral) =
    match literal with
    | LitString text ->
        let escaped =
            text
                .Replace("\\", "\\\\")
                .Replace("\"", "\\\"")
                .Replace("\n", "\\n")
                .Replace("\r", "\\r")
                .Replace("\t", "\\t")

        "\"" + escaped + "\""
    | LitNumber value when System.Double.IsInteger value && abs value < 2147483648.0 -> string (int value)
    | LitNumber value -> value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    | LitBool value -> if value then "true" else "false"

/// The value of a literal member: a required property, readonly or writable, typed by a single
/// non-enum string or numeric literal type.
let internal literalMemberValue (model: ShapeModel) (m: ResolvedMember) (typeId: int<Measure.typeId>) =
    if not m.Optional then
        match Map.tryFind typeId model.Types with
        | Some facts when
            (flag TypeFlags.StringLiteral facts || flag TypeFlags.NumberLiteral facts)
            && not (flag TypeFlags.EnumLiteral facts || flag TypeFlags.Union facts)
            ->
            literalOf facts
        | _ -> None
    else
        None

/// The spellings of absence a type is written with. TypeScript keeps `null`, `undefined` and
/// `void` apart and F# offers one `option` for all three, so the spelling survives here rather
/// than in the rendered binding: a KV miss reads `null`, a Durable Object storage miss reads
/// `undefined`, and JSON carries `null` alone. Every field is false for a type that is present.
type internal Absence =
    {
        FromNull: bool
        FromUndefined: bool
        FromVoid: bool
    }

    member this.IsAbsent = this.FromNull || this.FromUndefined || this.FromVoid

let internal absenceOf (facts: TypeFacts) =
    {
        FromNull = flag TypeFlags.Null facts
        FromUndefined = flag TypeFlags.Undefined facts
        FromVoid = flag TypeFlags.Void facts
    }

/// A type hoists to `option` (D1) exactly where it is written with a spelling of absence.
/// Defined through `absenceOf`, so the hoist and the spelling reported at the site agree by
/// construction: a hoisted member sets at least one flag of the finding it raises.
let internal isNullish (facts: TypeFacts) = (absenceOf facts).IsAbsent

/// The spellings of absence a union arm is written with. An intersection with a nullish operand
/// (`undefined & T`) is inhabited by that operand's absence alone, and carries its spelling.
let internal absenceIn (model: ShapeModel) (facts: TypeFacts) =
    if flag TypeFlags.Intersection facts then
        let operands =
            facts.IntersectionMembers
            |> List.choose (fun id -> Map.tryFind id model.Types |> Option.map absenceOf)

        {
            FromNull = operands |> List.exists _.FromNull
            FromUndefined = operands |> List.exists _.FromUndefined
            FromVoid = operands |> List.exists _.FromVoid
        }
    else
        absenceOf facts

/// Every spelling appearing across a set of types.
let internal absenceAcross (model: ShapeModel) (ids: int<Measure.typeId> list) : Absence =
    let spellings =
        ids
        |> List.choose (fun id -> Map.tryFind id model.Types |> Option.map (absenceIn model))

    {
        FromNull = spellings |> List.exists _.FromNull
        FromUndefined = spellings |> List.exists _.FromUndefined
        FromVoid = spellings |> List.exists _.FromVoid
    }

/// A union's members split into the hoisted nullish part (D1) and everything else. An arm
/// `undefined & I` beside an arm `I` is a subset of that arm, and belongs to neither part.
let internal splitNullish (model: ShapeModel) (facts: TypeFacts) =
    let subsumed (m: TypeFacts) =
        flag TypeFlags.Intersection m
        && m.IntersectionMembers
           |> List.forall (fun id ->
               List.contains id facts.UnionMembers
               || Map.tryFind id model.Types |> Option.exists isNullish)

    facts.UnionMembers
    |> List.choose (fun id ->
        match Map.tryFind id model.Types with
        | Some m when (absenceIn model m).IsAbsent && subsumed m -> None
        | Some m -> Some(id, (absenceIn model m).IsAbsent)
        | None -> Some(id, false))
    |> List.partition snd
    |> fun (nullish, rest) -> List.map fst nullish, List.map fst rest

/// `true | false` after nullish hoisting: TS re-expands `boolean` inside larger unions, and
/// the pair is just `bool` again.
let internal isBooleanPair (model: ShapeModel) (memberIds: int<Measure.typeId> list) =
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

/// The declared union whose non-nullish member set matches, if any, by type id and name: what
/// lets an `"ms" | "s" | undefined` member position resolve to the exported `TimeUnit` rather
/// than a synthesized twin (literal types are interned, so the ids match across positions).
/// Catalog runs keep inline literal unions independent of reachable named aliases;
/// direct alias references retain their declaration ownership.
let internal namedUnionByMembers
    (ctx: Context)
    (model: ShapeModel)
    (memberIds: int<Measure.typeId> list)
    : (int<Measure.typeId> * string) option =
    let wanted = List.sort memberIds

    model.DeclNames
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.tryPick (fun (typeId, name) ->
        match Map.tryFind typeId model.Types with
        | Some candidate when flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate) ->
            let nullish, members = splitNullish model candidate

            let literalEnum =
                members.Length >= 2
                && members
                   |> List.forall (fun id -> Map.tryFind id model.Types |> Option.bind literalOf |> Option.isSome)

            if
                (List.isEmpty nullish || literalEnum)
                && not (
                    literalEnum
                    && not (List.isEmpty candidate.AliasDeclarations)
                    && (ctx.Config.DeclarationCatalog
                        || not (List.isEmpty ctx.Config.DeclarationReferences))
                )
                && nonNullishMemberSet model candidate = wanted
            then
                Some(typeId, name)
            else
                None
        | _ -> None)

/// A *constructor object* (§4.4): the static side of a class, carrying construct signatures.
/// Its properties are the class's statics; `prototype` is the instance side, a declaration of
/// its own. An identity-only group carries no signatures, so a lib type is not one.
let internal isConstructorObject (facts: TypeFacts) =
    flag TypeFlags.Object facts && not facts.ConstructSignatures.IsEmpty

/// An object type that is only a callback: call signatures and nothing else worth keeping.
let internal isPureCallback (facts: TypeFacts) =
    not facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty
    && facts.Members.IsEmpty

/// Whether a type is the checker's `any` intrinsic, the type `typeRef` reports as `TR008`.
let internal isSourceAny (facts: TypeFacts) =
    flag TypeFlags.Any facts && facts.Response.IntrinsicName = ValueSome "any"

/// The identity of a hoisted type parameter: its symbol name paired with its constraint, or the
/// id itself for an anonymous parameter. Two ids share an identity when they resolve to the same
/// symbol under the same constraint.
let private typeParamIdentity (model: ShapeModel) (id: int<typeId>) =
    let paramFacts = Map.tryFind id model.Types

    match
        paramFacts
        |> Option.bind (_.SymbolName >> Option.map (fun value -> value / uom<symbolName>))
    with
    | Some name -> Ok(name, paramFacts |> Option.bind _.Constraint)
    | None -> Error id

/// An object type whose call signatures hoist a shared type-parameter name under two different
/// bounds — the F# head produced by `aliasTypeParams` is unwritable.
let hasIncompatibleOverloadedTypeParameters (model: ShapeModel) (facts: TypeFacts) =
    match facts.CallSignatures with
    | []
    | [ _ ] -> false
    | signatures ->
        signatures
        |> List.collect _.TypeParameters
        |> List.distinct
        |> List.choose (fun id ->
            match typeParamIdentity model id with
            | Ok identity -> Some identity
            | Error _ -> None)
        |> List.distinct
        |> List.countBy fst
        |> List.exists (fun (_, count) -> count > 1)

/// Whether a callback's signature closes a cycle through abbreviation bodies. Object members
/// belong to nominal declarations and terminate this traversal.
let private hasRecursiveCallbackSignature (model: ShapeModel) (facts: TypeFacts) =
    let signatureTypes (signature: ResolvedSignature) =
        signature.ReturnTypeId :: (signature.Parameters |> List.map _.TypeId)

    let visited = System.Collections.Generic.HashSet<int<typeId>>()

    let rec reaches id =
        id = facts.Response.TypeId
        || (visited.Add id
            && match Map.tryFind id model.Types with
               | None -> false
               | Some current ->
                   let callbackTypes =
                       if isPureCallback current && current.IndexInfos.IsEmpty then
                           match List.distinct current.CallSignatures with
                           | [ signature ] when signature.TypeParameters.IsEmpty && signature.Parameters.Length <= 1 ->
                               signatureTypes signature
                           | _ -> []
                       else
                           []

                   current.UnionMembers @ current.TypeArguments @ callbackTypes
                   |> List.exists reaches)

    isPureCallback facts
    && facts.IndexInfos.IsEmpty
    && match List.distinct facts.CallSignatures with
       | [ signature ] when signature.Parameters.Length <= 1 -> signatureTypes signature |> List.exists reaches
       | _ -> false

/// Call signatures requiring invocation-owned type parameters or a nominal recursive type.
let internal hasInterfaceCallSignatures (model: ShapeModel) (facts: TypeFacts) =
    match List.distinct facts.CallSignatures with
    | [ signature ] ->
        not signature.TypeParameters.IsEmpty
        || hasRecursiveCallbackSignature model facts
    | _ -> hasIncompatibleOverloadedTypeParameters model facts

/// Callable values retain their interface rather than exposing only an exported method.
let internal isPolymorphicCallableValue model symbolFlags facts =
    not (hasAny SymbolFlags.Function symbolFlags)
    && hasInterfaceCallSignatures model facts

/// An object type whose whole content is one index signature: `interface Bag { [key:
/// string]: number }`, or the anonymous shape `Record`/`ReadonlyRecord` express directly
/// (§4.10, TR059).
let internal isPureIndexSignature (facts: TypeFacts) =
    flag TypeFlags.Object facts
    && facts.Members.IsEmpty
    && facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty
    && facts.IndexInfos.Length = 1

/// A tuple type (§4.12). Fable compiles an F# tuple to a JS array, so a fixed tuple is an
/// exact match; the variadic forms are not.
let internal isTuple (facts: TypeFacts) =
    facts.Response.IsTupleType = ValueSome true

/// The members `ReadonlyArray<T>` declares in `lib.es5.d.ts`. Every later library revision adds
/// to this set and `Array<T>` extends it, so an array carries all of them whichever `lib` the
/// entry package compiles against.
let internal ArrayMembers =
    set
        [
            "concat"
            "every"
            "filter"
            "forEach"
            "indexOf"
            "join"
            "lastIndexOf"
            "length"
            "map"
            "reduce"
            "reduceRight"
            "slice"
            "some"
        ]

/// The element of an array-shaped type. `Array<T>` and `ReadonlyArray<T>` arrive from the
/// compiler lib as identity and a type argument, so the name answers for them. Every other
/// spelling - an interface extending one, an intersection over one, a mapped type over a
/// deferred tuple - carries `Array`'s members over a numeric index signature, and the element is
/// that signature's value. A tuple has the same members and a mapping of its own (§4.12).
let internal arrayElement (model: ShapeModel) (facts: TypeFacts) =
    match (facts.SymbolName |> Option.map (fun value -> value / uom<symbolName>)), facts.TypeArguments with
    | Some("Array" | "ReadonlyArray"), [ element ] -> Some element
    | _ when
        isTuple facts
        || not (Set.isSubset ArrayMembers (facts.Members |> List.map _.Symbol.Name |> Set.ofList))
        ->
        None
    | _ ->
        facts.IndexInfos
        |> List.tryPick (fun info ->
            match Map.tryFind info.KeyTypeId model.Types with
            | Some key when flag TypeFlags.Number key -> Some info.ValueTypeId
            | _ -> None)

/// A symbol name the checker made up for an anonymous shape rather than one the author wrote.
/// Module symbols are named by their quoted file path, which is no name either.
let internal isSyntheticName (name: string<symbolName>) =
    let name = string name
    name.StartsWith "__" || name.StartsWith "\""

/// Whether the union arm is an inline shape, named only by the checker.
let internal isAnonymousArm (arm: TypeFacts) =
    (arm.SymbolName |> Option.forall isSyntheticName)
    && ValueOption.isNone arm.Response.AliasSymbolId

/// A pure index signature the checker gave no symbol of its own: reached through a type
/// alias or written inline, rather than declared as an interface. `Bag`'s and
/// `FrozenBag`'s own names are what their own consumers call them, and stay; this shape has
/// none to lose.
/// The ids a declaration binds: its own where it is a genuine generic declaration, and the
/// alias's where it is a generic *alias*. `type Mapper<T> = (t: T) => T` leaves the function
/// type itself parameterless - the alias is the only place `T` appears - so both are read.
let internal declParamIds (facts: TypeFacts) =
    (facts.Response.ParameterTypeIds
     |> ValueOption.map Array.toList
     |> ValueOption.defaultValue [])
    @ facts.AliasTypeArguments
    |> List.distinct

/// Whether `typeId` reads `paramId` anywhere in its own type arguments or union/intersection
/// operands - `T[]`, `T | U`, `T & U` all carry a type parameter this way.
let rec private mentionsTypeParam
    (model: ShapeModel)
    (paramId: int<Measure.typeId>)
    (typeId: int<Measure.typeId>)
    : bool =
    typeId = paramId
    || (match Map.tryFind typeId model.Types with
        | None -> false
        | Some facts ->
            facts.TypeArguments |> List.exists (mentionsTypeParam model paramId)
            || facts.UnionMembers |> List.exists (mentionsTypeParam model paramId)
            || facts.IntersectionMembers |> List.exists (mentionsTypeParam model paramId))

/// Whether the type graph starting at `root` reaches `target` through type arguments, union or
/// intersection operands, or an alias's own target. An interface's members are not walked: a
/// cycle closed through one is nominal, not a cycle a plain abbreviation would inherit.
let private reachesTypeId (model: ShapeModel) (target: int<Measure.typeId>) (root: int<Measure.typeId>) : bool =
    let mutable visited = Set.empty

    let rec go typeId =
        if typeId = target then
            true
        elif Set.contains typeId visited then
            false
        else
            visited <- Set.add typeId visited

            match Map.tryFind typeId model.Types with
            | None -> false
            | Some facts ->
                facts.TypeArguments |> List.exists go
                || facts.UnionMembers |> List.exists go
                || facts.IntersectionMembers |> List.exists go
                || (facts.Response.TargetTypeId
                    |> ValueOption.map go
                    |> ValueOption.defaultValue false)

    go root

/// A pure index signature the checker gave no symbol of its own: reached through a type alias
/// or written inline, rather than declared as an interface. `Bag`'s and `FrozenBag`'s own names
/// are what their own consumers call them, and stay; this shape has none to lose.
///
/// A generic alias's own parameters must all reach the index signature's key or value: `type
/// Loose<P> = { [key: string]: string }` declares `P` and never reads it, and collapsing it to
/// `Record<string, string>` would leave `repair-arity` nothing to bind `P` to but an erased
/// phantom (FS0035), which drops the index signature entirely. The alias keeps its own name
/// instead.
///
/// Nor may the value or key type read back through to the index signature's own type: `type
/// JsonObject = { [key: string]: JsonValue }` beside `type JsonValue = ... | JsonObject` would
/// collapse `JsonObject` to `type JsonObject = Record<string, JsonValue>`, an abbreviation
/// cycling through another abbreviation (FS0953). An interface closes the cycle nominally
/// instead.
let internal isAnonymousIndexSignature (model: ShapeModel) (facts: TypeFacts) =
    isPureIndexSignature facts
    && (facts.SymbolName |> Option.forall isSyntheticName)
    && (declParamIds facts
        |> List.forall (fun p ->
            let info = List.exactlyOne facts.IndexInfos

            mentionsTypeParam model p info.KeyTypeId
            || mentionsTypeParam model p info.ValueTypeId))
    && (let info = List.exactlyOne facts.IndexInfos

        not (
            reachesTypeId model facts.Response.TypeId info.ValueTypeId
            || reachesTypeId model facts.Response.TypeId info.KeyTypeId
        ))

/// A member keyed by a JS well-known symbol (`__@iterator@<id>`): unrepresentable in F#, and
/// the embedded checker id is session-specific - keeping one would also break determinism.
let internal isSymbolKeyed (name: string) = name.StartsWith "__@"

/// The F# name a well-known symbol member is declared under: `SymbolDispose` for
/// `[Symbol.dispose]`.
let internal symbolMemberName (symbol: string) = "Symbol" + Naming.pascalSegment symbol

/// The well-known symbol a member is emitted under: the member is required, and its F# name is
/// free on the declaration.
let internal emittedSymbol (facts: TypeFacts) (m: ResolvedMember) =
    m.WellKnownSymbol
    |> Option.filter (fun symbol ->
        not m.Optional
        && facts.Members
           |> List.forall (fun other -> Naming.memberName other.Symbol.Name <> symbolMemberName symbol))

/// The element of an inherited `seq`.
let internal sequenceElement (reference: FsTypeRef) =
    match reference with
    | FsApp("seq", [ element ]) -> Some element
    | _ -> None

/// How many members an array-shaped intersection's other operands contribute (§4.6). An F# array
/// carries the element alone, so `readonly T[] & { tag: "t" }` loses `tag`. Every other spelling
/// of an array contributes zero.
let internal arrayMembersDropped (model: ShapeModel) (facts: TypeFacts) =
    facts.IntersectionMembers
    |> List.choose (fun id -> Map.tryFind id model.Types)
    |> List.filter (fun operand -> (arrayElement model operand).IsNone)
    |> List.collect (fun operand -> operand.Members |> List.map _.Symbol.Name)
    |> List.filter (isSymbolKeyed >> not)
    |> List.distinct
    |> List.length

/// One arm of a discriminated union, as one case: its facts, its tag value, and the test
/// separating it from the other arms carrying the same value.
type internal TaggedArm =
    {
        Facts: TypeFacts
        Value: string
        Test: FsCaseTest option
    }

/// What a union offers a `match` (D4, §4.5(2)).
type internal TaggedShape =
    /// The discriminant's TypeScript spelling, one case per arm in union order, and the tag
    /// values more than one arm carries.
    | Discriminated of tag: string * cases: TaggedArm list * shared: string list
    /// A discriminant every arm carries a string literal for, with a value whose arms no member
    /// test separates.
    | TagCollides of tag: string * value: string
    /// Every candidate property is absent or non-literal on some arm.
    | Untagged

/// An object type with members only: no symbol of its own, type arguments, signatures or index
/// signatures.
let private plainAnonymousObject (facts: TypeFacts) =
    flag TypeFlags.Object facts
    && (facts.SymbolName |> Option.forall isSyntheticName)
    && facts.TypeArguments.IsEmpty
    && facts.IndexInfos.IsEmpty
    && facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty

/// Whether two types read as one F# type: one id; two plain anonymous objects whose members pair
/// by name, optionality and readonly with types that read as one; two unions each of whose
/// members reads as one of the other's; or two applications of one generic target whose
/// arguments do.
let internal sameShapeUnder bindings (model: ShapeModel) (left: int<Measure.typeId>) (right: int<Measure.typeId>) =
    let rec go (assumed: Set<int<Measure.typeId> * int<Measure.typeId>>) left right =
        left = right
        || Set.contains (left, right) assumed
        || (match Map.tryFind left model.Types, Map.tryFind right model.Types with
            | Some l, Some r ->
                let assumed = Set.add (left, right) assumed

                if plainAnonymousObject l && plainAnonymousObject r then
                    l.Members.Length = r.Members.Length
                    && l.Members
                       |> List.forall (fun m ->
                           r.Members
                           |> List.exists (fun o ->
                               o.Symbol.Name = m.Symbol.Name
                               && o.Optional = m.Optional
                               && o.ReadOnly = m.ReadOnly
                               && go assumed m.TypeId o.TypeId))
                elif flag TypeFlags.Union l && flag TypeFlags.Union r then
                    l.UnionMembers
                    |> List.forall (fun m -> r.UnionMembers |> List.exists (go assumed m))
                    && r.UnionMembers
                       |> List.forall (fun m -> l.UnionMembers |> List.exists (go assumed m))
                else
                    match l.Response.TargetTypeId, r.Response.TargetTypeId with
                    | ValueSome lt, ValueSome rt when lt = rt && lt <> left && rt <> right ->
                        l.TypeArguments.Length = r.TypeArguments.Length
                        && List.forall2 (go assumed) l.TypeArguments r.TypeArguments
                    | _ -> false
            | _ -> false)

    go (Set.union bindings (bindings |> Set.map (fun (left, right) -> right, left))) left right

let internal sameShape model left right =
    sameShapeUnder Set.empty model left right

/// Whether the member is an optional property typed `never`, which an object never carries. The
/// checker reports `never | undefined` as `undefined`.
let private isAbsentOnly (model: ShapeModel) (m: ResolvedMember) =
    m.Optional
    && (match Map.tryFind m.TypeId model.Types with
        | Some facts -> flag TypeFlags.Never facts || flag TypeFlags.Undefined facts
        | None -> false)

/// The literal a member's type is, once `undefined` is set aside from an optional member.
let private memberLiteral (model: ShapeModel) (m: ResolvedMember) =
    match Map.tryFind m.TypeId model.Types with
    | Some facts when m.Optional && flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
        match
            facts.UnionMembers
            |> List.choose (fun id -> Map.tryFind id model.Types)
            |> List.filter (fun arm -> not (flag TypeFlags.Undefined arm))
        with
        | [ single ] -> literalOf single
        | _ -> None
    | Some facts -> literalOf facts
    | None -> None

/// The JavaScript `typeof` class every value of the type has.
let rec private typeofClass (model: ShapeModel) (facts: TypeFacts) =
    if flag TypeFlags.String facts || flag TypeFlags.StringLiteral facts then
        Some TypeofString
    elif flag TypeFlags.Number facts || flag TypeFlags.NumberLiteral facts then
        Some TypeofNumber
    elif flag TypeFlags.Boolean facts || flag TypeFlags.BooleanLiteral facts then
        Some TypeofBoolean
    elif flag TypeFlags.BigInt facts || flag TypeFlags.BigIntLiteral facts then
        Some TypeofBigInt
    elif flag TypeFlags.ESSymbol facts || flag TypeFlags.UniqueESSymbol facts then
        Some TypeofSymbol
    elif flag TypeFlags.Union facts then
        match
            facts.UnionMembers
            |> List.map (fun id -> Map.tryFind id model.Types |> Option.bind (typeofClass model))
            |> List.distinct
        with
        | [ Some single ] -> Some single
        | _ -> None
    elif
        flag TypeFlags.Object facts
        && (not facts.CallSignatures.IsEmpty || not facts.ConstructSignatures.IsEmpty)
    then
        Some TypeofFunction
    else
        // `object` and structural object types also admit functions. An absent call signature
        // does not prove a non-callable value, so these types cannot justify a typeof test.
        None

/// The tests separating arms that carry one tag value, one per arm in order, in the order
/// TypeScript narrows by: a second literal discriminant, then a property only some arms carry,
/// then a `typeof` class. One test form serves the whole group.
let internal separateArms (model: ShapeModel) (tag: string) (arms: TypeFacts list) : FsCaseTest list option =
    let fieldNames (arm: TypeFacts) =
        arm.Members
        |> List.map _.Symbol.Name
        |> List.filter (fun name -> name <> tag && not (isSymbolKeyed name))

    let memberOf (name: string) (arm: TypeFacts) =
        arm.Members |> List.tryFind (fun m -> m.Symbol.Name = name)

    let distinct values = List.distinct values = values

    let byLiteral () =
        fieldNames (List.head arms)
        |> List.tryPick (fun name ->
            let members = arms |> List.map (memberOf name)

            if members |> List.exists Option.isNone then
                None
            else
                let members = members |> List.map Option.get
                let literals = members |> List.map (memberLiteral model)

                if
                    literals |> List.forall Option.isSome
                    && distinct literals
                    && (members |> List.filter _.Optional |> List.length) <= 1
                then
                    Some(
                        List.map2
                            (fun (m: ResolvedMember) literal -> LiteralEquals(name, Option.get literal, m.Optional))
                            members
                            literals
                    )
                else
                    None)

    let byPresence () =
        let declares (arm: TypeFacts) (name: string) =
            memberOf name arm |> Option.exists (fun m -> not (isAbsentOnly model m))

        let own (arm: TypeFacts) =
            arm.Members
            |> List.tryFind (fun m ->
                not m.Optional
                && m.Symbol.Name <> tag
                && not (isSymbolKeyed m.Symbol.Name)
                && arms
                   |> List.forall (fun other ->
                       other.Response.TypeId = arm.Response.TypeId
                       || not (declares other m.Symbol.Name)))
            |> Option.map _.Symbol.Name

        let owned = arms |> List.map own

        if (owned |> List.filter Option.isNone |> List.length) > 1 then
            None
        else
            Some(
                owned
                |> List.map (function
                    | Some name -> HasProperty name
                    | None -> LacksProperties(owned |> List.choose id))
            )

    let byTypeof () =
        fieldNames (List.head arms)
        |> List.tryPick (fun name ->
            let classes =
                arms
                |> List.map (fun arm ->
                    memberOf name arm
                    |> Option.filter (fun m -> not m.Optional)
                    |> Option.bind (fun m -> Map.tryFind m.TypeId model.Types)
                    |> Option.bind (typeofClass model))

            if classes |> List.forall Option.isSome && distinct classes then
                Some(classes |> List.map (fun c -> TypeofIs(name, Option.get c)))
            else
                None)

    byLiteral () |> Option.orElseWith byPresence |> Option.orElseWith byTypeof

/// An arm's own string-literal value for the discriminant `tag`, when it has exactly one.
let internal taggedArmValue (model: ShapeModel) (tag: string) (arm: TypeFacts) =
    arm.Members
    |> List.tryFind (fun property -> property.Symbol.Name = tag)
    |> Option.bind (fun property -> Map.tryFind property.TypeId model.Types)
    |> Option.bind (fun propertyType ->
        match literalOf propertyType with
        | Some(LitString text) -> Some text
        | _ -> None)

/// The discriminant of a tagged union (D4, §4.5(2)): the property every non-nullish object
/// member carries with a string-literal type. A candidate whose values are all distinct wins,
/// in the first member's order. Where every candidate collides, the first candidate whose
/// shared values `separateArms` separates wins, and each arm stays a case of its own.
let internal taggedUnionShape (model: ShapeModel) (facts: TypeFacts) : TaggedShape =
    let members =
        facts.UnionMembers
        |> List.choose (fun id -> Map.tryFind id model.Types)
        |> List.filter (isNullish >> not)

    /// An intersection distributed over a union arrives flagged `Intersection` rather than
    /// `Object`, with its members already resolved onto the arm.
    let isObjectMember (m: TypeFacts) =
        (flag TypeFlags.Object m || flag TypeFlags.Intersection m)
        && not m.Members.IsEmpty

    if members.Length < 2 || not (members |> List.forall isObjectMember) then
        Untagged
    else
        let tagValue (m: TypeFacts) (tag: string) = taggedArmValue model tag m

        let candidates =
            members
            |> List.head
            |> _.Members
            |> List.map _.Symbol.Name
            |> List.filter (isSymbolKeyed >> not)
            |> List.choose (fun tag ->
                let tagged =
                    members
                    |> List.map (fun m -> tagValue m tag |> Option.map (fun value -> m, value))

                if tagged |> List.forall Option.isSome then
                    Some(tag, tagged |> List.map Option.get)
                else
                    None)

        let discriminates (_, tagged) =
            let values = tagged |> List.map snd
            List.distinct values = values

        let untested (tagged: (TypeFacts * string) list) =
            tagged
            |> List.map (fun (arm, value) ->
                {
                    Facts = arm
                    Value = value
                    Test = None
                })

        // Each shared value's arms, with their tests in arm order; `Error` carries the first
        // value no test separates.
        let separated (tag: string, tagged: (TypeFacts * string) list) =
            let shared =
                tagged |> List.countBy snd |> List.filter (snd >> (<) 1) |> List.map fst

            let tests =
                shared
                |> List.map (fun value ->
                    let group = tagged |> List.filter (snd >> (=) value) |> List.map fst
                    value, separateArms model tag (group))

            match tests |> List.tryFind (snd >> Option.isNone) with
            | Some(value, _) -> Error value
            | None ->
                let byArm =
                    tests
                    |> List.collect (fun (value, armTests) ->
                        let group = tagged |> List.filter (snd >> (=) value) |> List.map fst
                        List.zip (group |> List.map _.Response.TypeId) (Option.get armTests))
                    |> Map.ofList

                Ok(
                    Discriminated(
                        tag,
                        tagged
                        |> List.map (fun (arm, value) ->
                            {
                                Facts = arm
                                Value = value
                                Test = Map.tryFind arm.Response.TypeId byArm
                            }),
                        shared
                    )
                )

        match candidates |> List.tryFind discriminates with
        | Some(tag, tagged) -> Discriminated(tag, untested tagged, [])
        | None ->
            match candidates |> List.map (fun candidate -> fst candidate, separated candidate) with
            | [] -> Untagged
            | outcomes ->
                match outcomes |> List.tryPick (snd >> Result.toOption) with
                | Some shape -> shape
                | None ->
                    match List.head outcomes with
                    | tag, Error value -> TagCollides(tag, value)
                    | _, Ok shape -> shape

/// One arm's own contribution to an exclusive-arm union: the arm's facts, and the members it
/// carries a real type for where every other arm marks the same name `never`.
type internal ExclusiveArm =
    {
        Facts: TypeFacts
        Own: ResolvedMember list
    }

/// A union of object arms exclusive on some of their members (§4.5, wave fourteen item 4):
/// every arm agrees, by name, type, optionality and readonly, on every member it shares with the rest,
/// and every member exclusive to one arm is typed `never` on every other arm that carries it -
/// never silently absent. `ExclusiveArmsFold` carries at least one arm whose own member is
/// required in that arm, giving F# a required-arity fact to resolve `Create` overloads on;
/// `ExclusiveArmsDecline` is the same shape where every arm's own members are optional, so no
/// such fact exists anywhere in the union.
type internal ExclusiveArmsShape =
    | NotExclusiveArms
    | ExclusiveArmsFold of shared: ResolvedMember list * arms: ExclusiveArm list
    | ExclusiveArmsDecline of arms: int

// An optional property typed `never` is not carrying `TypeFlags.Never` by the time it reaches
// here: the checker's own union rule collapses `never | undefined` (the type an optional
// property widens to) to plain `undefined`, so the placeholder surfaces as `TypeFlags.Undefined`
// alone. `m.Optional` rules out a genuinely required `undefined`-typed member reaching the same
// branch.
let private isNeverTyped (model: ShapeModel) (m: ResolvedMember) =
    m.Optional
    && (match Map.tryFind m.TypeId model.Types with
        | Some facts -> flag TypeFlags.Never facts || flag TypeFlags.Undefined facts
        | None -> false)

/// Callable or constructable object arms with a defined property unique to each arm.
let internal erasedObjectUnionShape (model: ShapeModel) (facts: TypeFacts) =
    let arms = facts.UnionMembers |> List.choose (fun id -> Map.tryFind id model.Types)

    let rec excludesUndefined typeId =
        match Map.tryFind typeId model.Types with
        | Some t when flag TypeFlags.Union t ->
            not t.UnionMembers.IsEmpty && List.forall excludesUndefined t.UnionMembers
        | Some t when flag TypeFlags.Intersection t -> List.exists excludesUndefined t.IntersectionMembers
        | Some t ->
            [
                TypeFlags.Object
                TypeFlags.String
                TypeFlags.StringLiteral
                TypeFlags.Number
                TypeFlags.NumberLiteral
                TypeFlags.Boolean
                TypeFlags.BooleanLiteral
                TypeFlags.BigInt
                TypeFlags.BigIntLiteral
                TypeFlags.ESSymbol
                TypeFlags.UniqueESSymbol
                TypeFlags.Null
            ]
            |> List.exists (fun flag_ -> flag flag_ t)
        | None -> false

    let testFor (arm: TypeFacts) =
        arm.Members
        |> List.tryFind (fun m ->
            not m.Optional
            && not (isSymbolKeyed m.Symbol.Name)
            && excludesUndefined m.TypeId
            && arms
               |> List.forall (fun other ->
                   other.Response.TypeId = arm.Response.TypeId
                   || (other.Members
                       |> List.tryFind (fun candidate -> candidate.Symbol.Name = m.Symbol.Name)
                       |> Option.exists (isNeverTyped model))))
        |> Option.map (fun m -> arm, IsDefined m.Symbol.Name)

    if
        not (flag TypeFlags.Union facts)
        || arms.Length < 2
        || arms.Length <> facts.UnionMembers.Length
        || not (
            arms
            |> List.forall (fun arm -> flag TypeFlags.Object arm || flag TypeFlags.Intersection arm)
        )
        || not (
            arms
            |> List.exists (fun arm -> not arm.CallSignatures.IsEmpty || not arm.ConstructSignatures.IsEmpty)
        )
    then
        None
    else
        let tested = List.map testFor arms

        if List.forall Option.isSome tested then
            Some(List.choose id tested)
        else
            None

let internal exclusiveArmShape (model: ShapeModel) (facts: TypeFacts) : ExclusiveArmsShape =
    let arms =
        facts.UnionMembers
        |> List.choose (fun id -> Map.tryFind id model.Types)
        |> List.filter (isNullish >> not)

    let isObjectArm (m: TypeFacts) =
        (flag TypeFlags.Object m || flag TypeFlags.Intersection m)
        && not m.Members.IsEmpty

    if
        arms.Length < 2
        || not (arms |> List.forall isObjectArm)
        || arms
           |> List.exists (fun arm -> not arm.CallSignatures.IsEmpty || not arm.ConstructSignatures.IsEmpty)
    then
        NotExclusiveArms
    else
        let names =
            arms
            |> List.collect (fun a -> a.Members |> List.map (fun m -> m.Symbol.Name))
            |> List.filter (isSymbolKeyed >> not)
            |> List.distinct

        let declaredBy (name: string) (arm: TypeFacts) =
            arm.Members |> List.tryFind (fun m -> m.Symbol.Name = name)

        // `None` here means the whole shape falls outside this construct: a member silent on
        // some arm cannot be told apart from one that arm marks `never`, two arms cannot both
        // carry a member's own type with disagreeing shapes, and a member shared by every arm
        // must read identically everywhere.
        let classify (name: string) =
            let onArm = arms |> List.map (fun arm -> arm, declaredBy name arm)

            if onArm |> List.exists (snd >> Option.isNone) then
                None
            else
                let real, never =
                    onArm
                    |> List.map (fun (arm, m) -> arm, Option.get m)
                    |> List.partition (fun (_, m) -> not (isNeverTyped model m))

                if real.IsEmpty then
                    None
                else
                    let first = snd (List.head real)

                    if
                        not (
                            real
                            |> List.forall (fun (_, m) ->
                                sameShape model m.TypeId first.TypeId && m.ReadOnly = first.ReadOnly)
                        )
                    then
                        None
                    elif never.IsEmpty then
                        if real |> List.forall (fun (_, m) -> m.Optional = first.Optional) then
                            Some(Choice1Of2 first)
                        else
                            None
                    else
                        Some(Choice2Of2(real |> List.map (fun (arm, m) -> arm.Response.TypeId, m)))

        let classified = names |> List.map classify

        if classified |> List.exists Option.isNone then
            NotExclusiveArms
        else
            let shared =
                classified
                |> List.choose (function
                    | Some(Choice1Of2 m) -> Some m
                    | _ -> None)

            let exclusive =
                classified
                |> List.choose (function
                    | Some(Choice2Of2 owners) -> Some owners
                    | _ -> None)

            let ownFor (arm: TypeFacts) =
                exclusive
                |> List.choose (List.tryFind (fst >> (=) arm.Response.TypeId) >> Option.map snd)

            let armShapes = arms |> List.map (fun arm -> { Facts = arm; Own = ownFor arm })

            let parameterTypes arm =
                let required, optional =
                    shared @ arm.Own |> List.partition (fun member_ -> not member_.Optional)

                required @ optional
                |> List.map (fun member_ -> member_.TypeId, member_.Optional)

            // An arm contributing nothing of its own is not exclusive - it is the same shape
            // as another arm, reached twice, and belongs to `namedUnionByMembers` instead.
            if armShapes |> List.exists (fun a -> a.Own.IsEmpty) then
                NotExclusiveArms
            elif
                (armShapes |> List.map parameterTypes |> List.distinct).Length
                <> armShapes.Length
            then
                // F# overload signatures omit parameter names. Keep colliding arms as separate types.
                NotExclusiveArms
            // Folds the moment one arm's own member is required: that fact alone gives F# a
            // required-arity anchor to resolve the whole set of `Create` overloads on, even
            // where every other arm's own member stays optional (`ContainerLikeOptions`,
            // `docs/.ai/handovers/lane-cp.md`). Declines only where no arm anywhere has one.
            elif
                armShapes
                |> List.exists (fun a -> a.Own |> List.exists (fun m -> not m.Optional))
            then
                ExclusiveArmsFold(shared, armShapes)
            else
                ExclusiveArmsDecline arms.Length

/// A property that exists only to make a type nominal: keyed by a unique symbol, spelled with a
/// leading underscore, or typed `never`. An object of only these carries nothing at runtime,
/// separating a branding intersection from a shape (§4.6).
let internal isMarkerMember (model: ShapeModel) (m: ResolvedMember) =
    isSymbolKeyed m.Symbol.Name
    || m.Symbol.Name.StartsWith "_"
    || (match Map.tryFind m.TypeId model.Types with
        | Some facts -> flag TypeFlags.Never facts
        | None -> false)

/// The primitive a branding intersection brands, where it is one (§4.6, D11): exactly one
/// primitive constituent, intersected with objects carrying markers and nothing else. Two real
/// shapes, or a primitive with an object having a usable member, are ordinary intersections.
let rec internal brandedPrimitive (model: ShapeModel) (facts: TypeFacts) =
    // An intersection over anything but a bare primitive distributes: `boolean & Marker` comes
    // back as `(true & Marker) | (false & Marker)`. The arms carry no names, so a union of
    // anonymous brands agreeing on the primitive is one brand; `UserId | SessionId` is not.
    if flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
        let arms = facts.UnionMembers |> List.choose (fun id -> Map.tryFind id model.Types)

        if
            arms.Length <> facts.UnionMembers.Length
            || arms.IsEmpty
            || arms
               |> List.exists (fun arm ->
                   not (flag TypeFlags.Intersection arm)
                   || Map.containsKey arm.Response.TypeId model.DeclNames)
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

/// The widest arity `Fable.Core` 5.2.0 ships a `U`-type for. A union past this width still
/// renders as `U<n>` (D4); the render tier declares the type in the emitting file's own
/// footer rather than reaching into `Fable.Core`.
[<Literal>]
let internal ErasedUnionArity = 9

/// The widest tagged-union case worth generating. A DU case binds its fields positionally, so
/// past a dozen every `match` clause is a wall of wildcards and the erased union over the arm
/// interfaces - which keeps the properties named - reads better.
[<Literal>]
let internal TaggedCaseFieldBudget = 12

/// An arm's fields: every member but the discriminant, which Fable writes from the case itself.
let internal taggedCaseFields (tag: string) (arm: TypeFacts) =
    arm.Members
    |> List.filter (fun m -> m.Symbol.Name <> tag && not (isSymbolKeyed m.Symbol.Name))

/// Whether the arm's case carries the arm whole: every member is the tag or a case field, and
/// the arm has no index signature.
let internal caseCarriesArmWhole (tag: string) (arm: TypeFacts) =
    arm.IndexInfos.IsEmpty
    && arm.Members
       |> List.forall (fun m -> m.Symbol.Name = tag || not (isSymbolKeyed m.Symbol.Name))

/// Whether an arm carries data a DU case can bind: properties within the budget, and neither a
/// method nor a signature.
let internal isTaggedCaseData (tag: string) (arm: TypeFacts) =
    let fields = taggedCaseFields tag arm

    arm.CallSignatures.IsEmpty
    && arm.ConstructSignatures.IsEmpty
    && (fields |> List.forall (fun m -> not (hasAny SymbolFlags.Method m.Symbol.Flags)))
    && fields.Length <= TaggedCaseFieldBudget

/// Whether a value of the type is a class instance: of a class, or of an interface an ambient
/// `declare var` constructs. An instantiation reads its target's.
let internal isClassInstance (model: ShapeModel) (facts: TypeFacts) =
    let variable =
        SymbolFlags.FunctionScopedVariable ||| SymbolFlags.BlockScopedVariable

    let own (facts: TypeFacts) =
        facts.AmbientClass.IsSome
        || hasAny SymbolFlags.Class facts.SymbolFlags
        || (hasAny SymbolFlags.Interface facts.SymbolFlags
            && hasAny variable facts.SymbolFlags)
        || facts.Response.ObjectFlags
           |> ValueOption.exists (fun flags -> flags.HasFlag ObjectFlags.Class)

    own facts
    || facts.Response.TargetTypeId
       |> ValueOption.toOption
       |> Option.bind (fun target -> Map.tryFind target model.Types)
       |> Option.exists own

/// Whether an arm's case carries the arm's interface as its one `[<InlineInterface>]` field: the
/// arm is neither callable, constructable nor a class instance, and is named, or is more than its
/// case's fields carry.
let internal caseInlinesArm (model: ShapeModel) (tag: string) (arm: TypeFacts) =
    //FOR-REVIEW Constructing an inlined case copies the value onto its prototype. The copy of a
    //FOR-REVIEW function is not callable, and the copy of a class instance loses its internal state,
    //FOR-REVIEW so both stay out until the fork constructs by identity.
    arm.CallSignatures.IsEmpty
    && arm.ConstructSignatures.IsEmpty
    && not (isClassInstance model arm)
    && (not (isAnonymousArm arm)
        || not (caseCarriesArmWhole tag arm)
        || not (isTaggedCaseData tag arm))

/// Whether an arm is a case of a declared DU: as its fields, or as its interface.
let internal isTaggedCase (model: ShapeModel) (tag: string) (arm: TypeFacts) =
    isTaggedCaseData tag arm || caseInlinesArm model tag arm

/// Whether a union meets the conditions `detect-tagged-unions` declares a DU under (D4,
/// §4.5(2)): every member non-nullish, a discriminant whose shared values `separateArms`
/// separates, and every arm a case. A reference to such a union applies the parameters its
/// declaration binds.
let internal isTaggedDeclaration (model: ShapeModel) (facts: TypeFacts) =
    flag TypeFlags.Union facts
    && not (flag TypeFlags.Boolean facts)
    && List.isEmpty (fst (splitNullish model facts))
    && (match taggedUnionShape model facts with
        | Discriminated(tag, tagged, _) -> tagged |> List.forall (fun arm -> isTaggedCase model tag arm.Facts)
        | TagCollides _
        | Untagged -> false)

/// A named object union whose declaration binds its own and captured type parameters.
let internal isObjectUnionDeclaration model facts =
    isTaggedDeclaration model facts || (erasedObjectUnionShape model facts).IsSome

/// The declared generic erased-union alias a union applies, with its name, parameters and the
/// site's arguments: `FunctionMaybe<Live>` for `{ (): Live } | Live`. Every declared parameter
/// is an arm of the declared form. The site corresponds arm for arm: each parameter arm
/// contributes its argument's constituents, and each other declared arm has exactly one
/// instance arm of the same type, generic target or symbol.
let internal erasedAliasApplication
    (model: ShapeModel)
    (site: TypeFacts)
    : (string * int<Measure.typeId> list * int<Measure.typeId> list) option =
    match site.Response.AliasSymbolId, site.Response.AliasArgumentTypeIds with
    | ValueSome alias, ValueSome arguments when
        arguments.Length > 0
        && flag TypeFlags.Union site
        && not (flag TypeFlags.Boolean site)
        && not (Map.containsKey site.Response.TypeId model.DeclNames)
        && arguments |> Array.forall (fun argument -> Map.containsKey argument model.Types)
        ->
        let arguments = Array.toList arguments

        let declared =
            model.DeclNames
            |> Map.toSeq
            |> Seq.tryPick (fun (typeId, name) ->
                match Map.tryFind typeId model.Types with
                | Some declared when
                    typeId <> site.Response.TypeId
                    && declared.Response.AliasSymbolId = ValueSome alias
                    && flag TypeFlags.Union declared
                    && not (flag TypeFlags.Boolean declared)
                    && not (Map.containsKey typeId model.AliasApplications)
                    && not (isObjectUnionDeclaration model declared)
                    ->
                    let parameters = declParamIds declared

                    if
                        parameters.Length = arguments.Length
                        && declared.Response.AliasArgumentTypeIds |> ValueOption.map Array.toList =
                            ValueSome parameters
                        && parameters
                           |> List.forall (fun parameter -> List.contains parameter declared.UnionMembers)
                    then
                        Some(name, declared, parameters)
                    else
                        None
                | _ -> None)

        match declared with
        | Some(name, declared, parameters) ->
            let instanceArms = site.UnionMembers
            let substitution = List.zip parameters arguments |> Map.ofList

            let constituents (argument: int<Measure.typeId>) =
                match Map.tryFind argument model.Types with
                | Some facts when flag TypeFlags.Union facts && not facts.UnionMembers.IsEmpty -> facts.UnionMembers
                | Some facts when flag TypeFlags.Boolean facts ->
                    instanceArms
                    |> List.filter (fun id ->
                        Map.tryFind id model.Types |> Option.exists (flag TypeFlags.BooleanLiteral))
                | _ -> [ argument ]

            let substituted =
                parameters
                |> List.collect (fun parameter -> constituents substitution[parameter])

            let corresponds (declaredArm: TypeFacts) (instanceArm: TypeFacts) =
                declaredArm.Response.TypeId = instanceArm.Response.TypeId
                || (declaredArm.Response.TargetTypeId.IsSome
                    && declaredArm.Response.TargetTypeId = instanceArm.Response.TargetTypeId)
                || (flag TypeFlags.Object declaredArm
                    && flag TypeFlags.Object instanceArm
                    && declaredArm.Response.SymbolId.IsSome
                    && declaredArm.Response.SymbolId = instanceArm.Response.SymbolId)

            let others =
                instanceArms |> List.filter (fun id -> not (List.contains id substituted))

            let matched =
                declared.UnionMembers
                |> List.filter (fun arm -> not (List.contains arm parameters))
                |> List.map (fun arm ->
                    match Map.tryFind arm model.Types with
                    | Some declaredArm ->
                        others
                        |> List.filter (fun id ->
                            Map.tryFind id model.Types |> Option.exists (corresponds declaredArm))
                    | None -> [])

            let corresponding =
                substituted |> List.forall (fun id -> List.contains id instanceArms)
                && matched |> List.forall (fun candidates -> candidates.Length = 1)
                && (matched |> List.concat |> List.distinct |> List.sort) = List.sort others

            if corresponding then
                Some(name, parameters, arguments)
            else
                None
        | None -> None
    | _ -> None

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

/// A tuple without a variadic element: one F# parameter per element.
let private isFixedTuple (facts: TypeFacts) =
    isTuple facts && not (tupleElementFlags facts |> List.exists isVariadicElement)

/// `signature` with its rest parameter replaced by the elements of the fixed tuple `facts`.
/// An element is named by the rest parameter's binding pattern (`...[value]`), then by the
/// tuple's label (`[start: number]`), then by position. A name already taken by another
/// parameter falls back to position.
let private spreadTuple (signature: ResolvedSignature) (rest: ResolvedMember) (facts: TypeFacts) =
    let flags = tupleElementFlags facts
    let leading = List.truncate (signature.Parameters.Length - 1) signature.Parameters

    let positional index =
        if flags.Length = 1 then
            rest.Symbol.Name
        else
            $"{rest.Symbol.Name}{index}"

    let written index =
        match List.tryItem index signature.RestPatternNames |> Option.flatten with
        | Some name -> Some name
        | None -> List.tryItem index facts.TupleLabels |> Option.flatten

    let declared = List.init flags.Length written

    let distinct =
        let names = declared |> List.choose id
        let taken = leading |> List.map _.Symbol.Name |> Set.ofList

        List.length (List.distinct names) = names.Length
        && names |> List.forall (fun name -> not (Set.contains name taken))

    let elementName index =
        match declared[index] with
        | Some name when distinct -> name
        | _ -> positional index

    let expanded =
        List.mapi2
            (fun index (element: ElementFlags) typeId ->
                { rest with
                    Symbol =
                        { rest.Symbol with
                            Name = elementName index
                        }
                    Optional = element.HasFlag ElementFlags.Optional
                    TypeId = typeId
                })
            flags
            facts.TypeArguments

    { signature with
        Parameters = leading @ expanded
        HasRest = false
        RestPatternNames = []
    }

/// A signature whose tuple-typed rest parameter is written out as the parameters it stands for
/// (§4.12). `(...args: [value: T]) => R` is TypeScript's spelling of `(value: T) => R` and
/// `(...args: []) => R` of `() => R`: each element becomes an ordinary parameter, an optional
/// element an optional one. A tuple carrying a variadic element keeps the rest form.
let internal expandTupleRest (model: ShapeModel) (signature: ResolvedSignature) : ResolvedSignature =
    match signature.HasRest, List.tryLast signature.Parameters with
    | true, Some rest ->
        match Map.tryFind rest.TypeId model.Types with
        | Some facts when isFixedTuple facts -> spreadTuple signature rest facts
        | _ -> signature
    | _ -> signature

/// The overloads a signature stands for (§4.12): a rest parameter over a union of fixed tuples,
/// `(...args: [end: number] | [start: number, end: number])`, is one overload per tuple, in
/// ascending arity. Arms that differ only by trailing `undefined` or `void` elements, or only by
/// which elements are optional, are written once: as the arm with the most optional elements,
/// then the shortest. Every other signature stands for itself, through `expandTupleRest`.
let internal restOverloads (model: ShapeModel) (signature: ResolvedSignature) : ResolvedSignature list =
    let arms =
        match signature.HasRest, List.tryLast signature.Parameters with
        | true, Some rest ->
            match Map.tryFind rest.TypeId model.Types with
            | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
                let members = facts.UnionMembers |> List.map (fun id -> Map.tryFind id model.Types)

                if members.Length > 1 && members |> List.forall (Option.exists isFixedTuple) then
                    Some(rest, List.choose id members)
                else
                    None
            | _ -> None
        | _ -> None

    match arms with
    | None -> [ expandTupleRest model signature ]
    | Some(rest, tuples) ->
        let unitLike id =
            Map.tryFind id model.Types
            |> Option.exists (fun facts -> flag TypeFlags.Undefined facts || flag TypeFlags.Void facts)

        let callKey (facts: TypeFacts) =
            facts.TypeArguments |> List.rev |> List.skipWhile unitLike |> List.rev

        let optionalElements (facts: TypeFacts) =
            tupleElementFlags facts
            |> List.filter (fun element -> element.HasFlag ElementFlags.Optional)
            |> List.length

        tuples
        |> List.sortBy _.TypeArguments.Length
        |> List.groupBy callKey
        |> List.map (fun (_, arms) -> arms |> List.maxBy optionalElements)
        |> List.map (spreadTuple signature rest)

/// The generic declaration an instantiation points back at, when this run declares it. A
/// generic declaration is its own target, so only a genuine instantiation matches.
let internal instantiationOf (model: ShapeModel) (facts: TypeFacts) =
    // Only a *reference* - `Ready<T>` over an interface or class - is an application. An
    // anonymous object type instantiated in some other scope also carries its original as a
    // target, but no arguments to write it with, so it is declared on its own as before.
    let isReference =
        facts.Response.ObjectFlags
        |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
        |> ValueOption.defaultValue false

    match facts.Response.TargetTypeId with
    | ValueSome target when isReference && target <> facts.Response.TypeId ->
        Map.tryFind target model.DeclNames
        |> Option.map (fun name -> name, facts.TypeArguments)
    | _ -> None

/// The arguments a generic declaration stands for when it is named at a reference position:
/// its own parameters. F# has no bare spelling for a generic type, so the self-reference in
/// `map(next: T): Box<T>` has to re-apply them to come back out as it was written.
let internal ownArguments (facts: TypeFacts) =
    match facts.Response.TargetTypeId with
    | ValueSome target when target = facts.Response.TypeId -> facts.TypeArguments
    | _ -> []

/// The parameters a hoisted anonymous declaration reads from the scope it was written in
/// (§4.9, `DeclParams`) - what a reference to it applies back, and what its declaration binds
/// beside any parameters of its own.
let internal freeParamsOf (model: ShapeModel) (typeId: int<Measure.typeId>) =
    Map.tryFind typeId model.DeclParams |> Option.defaultValue []

/// An intersection of object types that flattens into one interface (§4.6): not a brand, not an
/// array, and carrying the members the resolve tier read off it - which it only does when every
/// operand is an object, so a primitive or type-parameter operand leaves this false.
let internal isFlattenable (model: ShapeModel) (facts: TypeFacts) =
    flag TypeFlags.Intersection facts
    && (brandedPrimitive model facts).IsNone
    && (arrayElement model facts).IsNone
    && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)

/// Whether a type stands over a type parameter, through its own arguments or its alias's. Its
/// content is answered again at each instantiation.
let internal standsOverTypeParameter (model: ShapeModel) (facts: TypeFacts) =
    not facts.AliasTypeArguments.IsEmpty
    || facts.TypeArguments
       |> List.exists (fun id ->
           match Map.tryFind id model.Types with
           | Some argument -> flag TypeFlags.TypeParameter argument
           | None -> false)

/// An intersection operand that constrains nothing: an anonymous object type whose members,
/// index signatures and signatures are all empty, at every instantiation. `"in" | "out" | (string
/// & {})` is TypeScript's autocomplete idiom, where `{}` keeps the literals visible and `string`
/// is the type (§4.6).
///
/// The operand must also be anonymous and free of type parameters: a type this run only
/// references arrives identity-only, and an operand over a type parameter fills its members in at
/// each use (§4.6).
let internal isVacuousOperand (model: ShapeModel) (facts: TypeFacts) =
    flag TypeFlags.Object facts
    && facts.Members.IsEmpty
    && facts.IndexInfos.IsEmpty
    && facts.CallSignatures.IsEmpty
    && facts.ConstructSignatures.IsEmpty
    && facts.BaseTypes.IsEmpty
    && facts.ImplementedTypes.IsEmpty
    && not (standsOverTypeParameter model facts)
    && (match facts.SymbolName with
        | None -> true
        | Some name -> isSyntheticName name)

/// The operands of an intersection, where every one of them is in the type table.
let internal operandsOf (model: ShapeModel) (facts: TypeFacts) =
    let operands =
        facts.IntersectionMembers |> List.choose (fun id -> Map.tryFind id model.Types)

    if operands.Length = facts.IntersectionMembers.Length then
        operands
    else
        []

/// The single operand an intersection reduces to, once the operands that constrain nothing are
/// dropped (§4.6). An intersection standing as a generic alias's body is left alone: its
/// operands read empty at the declaration and fill in at each instantiation, which is the round
/// trip `Resolve` declines (§4.6).
let internal reducedOperand (model: ShapeModel) (facts: TypeFacts) =
    if standsOverTypeParameter model facts then
        None
    else
        match operandsOf model facts |> List.partition (isVacuousOperand model) with
        | _ :: _, [ remaining ] -> Some remaining
        | _ -> None

/// The operand an intersection reduces to when the checker reduces the whole intersection to
/// `never` (§4.6): two object operands share a property name, one types it a nullable marker,
/// and that collision empties every member from the aggregate rather than leaving the property
/// itself typed `never`. The survivor is the operand that did not mark the shared property
/// nullable, carrying the property that a caller can actually reach.
let internal uninhabitedOperand (model: ShapeModel) (facts: TypeFacts) =
    if not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty) then
        None
    else
        let operands = operandsOf model facts

        if
            operands.IsEmpty
            || not (operands |> List.forall (flag TypeFlags.Object))
            || not (operands |> List.exists (fun operand -> not operand.Members.IsEmpty))
        then
            None
        else
            let isNullable (m: ResolvedMember) =
                match Map.tryFind m.TypeId model.Types with
                | Some t -> flag TypeFlags.Null t || flag TypeFlags.Undefined t
                | None -> false

            operands
            |> List.collect (fun operand -> operand.Members |> List.map (fun m -> m, operand))
            |> List.groupBy (fun (m, _) -> m.Symbol.Name)
            |> List.tryPick (fun (name, group) ->
                match group |> List.partition (fun (m, _) -> isNullable m) with
                | _ :: _, [ (_, survivor) ] -> Some(survivor, name)
                | _ -> None)

/// Whether every property of an anonymous object operand is declared by a type-parameter
/// operand's bound, under the same type id and at least as optional (§4.6). `R & { req: Base }`
/// over `R extends { req: Base }` is `R`, and so is `T & {}`.
let internal restatesBound (model: ShapeModel) (parameters: TypeFacts list) (operand: TypeFacts) =
    let declaredByBound (m: ResolvedMember) =
        parameters
        |> List.exists (fun parameter ->
            parameter.Constraint
            |> Option.bind (fun id -> Map.tryFind id model.Types)
            |> Option.exists (fun bound ->
                bound.Members
                |> List.exists (fun declared ->
                    declared.Symbol.Name = m.Symbol.Name
                    && declared.TypeId = m.TypeId
                    && (m.Optional || not declared.Optional))))

    // A mapped or generic operand is never read as restating the bound.
    let mapped =
        operand.Response.ObjectFlags
        |> ValueOption.exists (fun flags -> flags.HasFlag ObjectFlags.Mapped)

    flag TypeFlags.Object operand
    && not mapped
    && not (standsOverTypeParameter model operand)
    && (operand.SymbolName |> Option.forall isSyntheticName)
    && operand.IndexInfos.IsEmpty
    && operand.CallSignatures.IsEmpty
    && operand.ConstructSignatures.IsEmpty
    && operand.BaseTypes.IsEmpty
    && operand.Members |> List.forall declaredByBound

/// The operands an intersection over at least one type parameter is written as (§4.6): every
/// operand except an object operand that restates the parameters' bounds. `None` for an
/// intersection without a type-parameter operand (`string & { count: number }`), and for one
/// with a nullish operand (`undefined & T`), which a union reads as absence. `string & U` keeps
/// both operands.
let internal openOperands (model: ShapeModel) (facts: TypeFacts) =
    let operands = operandsOf model facts
    let parameters = operands |> List.filter (flag TypeFlags.TypeParameter)

    if parameters.IsEmpty || operands |> List.exists isNullish then
        None
    else
        operands
        |> List.filter (fun operand ->
            flag TypeFlags.TypeParameter operand
            || not (restatesBound model parameters operand))
        |> Some

/// Whether an argument *is* the bound, or reaches it through the bases and intersection
/// operands `shape-interfaces` emits as `inherit`. An instantiation is asked of its target,
/// the declaration carrying the heritage. This is F# subtyping, not TypeScript's: it is the
/// question `:>` asks, and it is asked in two places - of an argument at a reference, and of a
/// parameter's own default when the head is written.
let internal satisfiesNominally (model: ShapeModel) (boundId: int<Measure.typeId>) (argument: int<Measure.typeId>) =
    let boundName = Map.tryFind boundId model.DeclNames

    let rec walk seen typeId =
        typeId = boundId
        || (boundName.IsSome && Map.tryFind typeId model.DeclNames = boundName)
        || (not (Set.contains typeId seen)
            && (match Map.tryFind typeId model.Types with
                | Some facts ->
                    facts.BaseTypes
                    @ facts.IntersectionMembers
                    @ (facts.Response.TargetTypeId |> ValueOption.toOption |> Option.toList)
                    |> List.exists (walk (Set.add typeId seen))
                | None -> false))

    walk Set.empty argument

/// TypeScript's `extends` is structural and F#'s `:>` is nominal, so a constraint is only
/// worth writing where some nominal relation could hold (§4.9). The declaration's own default
/// argument is the one argument the run always knows about and the one every bare use resolves
/// to: `class Geometry<Attributes extends Wide = Narrow>` is written `Geometry<Narrow>` at
/// every bare site, so a default that does not inherit its own bound makes the bound reject
/// the declaration's ordinary use. That is `three`'s `BufferGeometry<NormalBufferAttributes>`,
/// 328 times.
///
/// The answer is read at both ends of the same constraint, which is what keeps `TP008` and
/// `TR044` from contradicting each other: where this is false the head drops the bound
/// (`TP008`) *and* the reference stops rewriting arguments to it (`TR044` falls silent), so
/// the argument TypeScript resolved survives. Where it is true the head keeps `:>` and
/// `TR044` still widens whatever cannot satisfy it.
/// A `never` default retains the bound; concrete uses apply the normal argument check.
let internal constraintProvenNominal
    (model: ShapeModel)
    (parameterId: int<Measure.typeId>)
    (boundId: int<Measure.typeId>)
    =
    match Map.tryFind parameterId model.Types |> Option.bind _.Default with
    | Some fallback when fallback <> parameterId ->
        (Map.tryFind fallback model.Types |> Option.exists (flag TypeFlags.Never))
        || satisfiesNominally model boundId fallback
    | _ -> true

/// A reference as the phrase a finding message names it by. Source-file spelling - module
/// qualification, line breaks, the tick on a variable - belongs to the renderer.
let rec internal typeSpelling (reference: FsTypeRef) : string =
    let spell (separator: string) items =
        items |> List.map typeSpelling |> String.concat separator

    let commas = spell ", "

    match reference with
    | FsBool -> "bool"
    | FsString -> "string"
    | FsFloat -> "float"
    | FsBigInt -> "bigint"
    | FsUnit -> "unit"
    | FsObj -> "obj"
    | FsOption inner -> $"{typeSpelling inner} option"
    | FsArray inner -> $"{typeSpelling inner}[]"
    | FsTuple items -> spell " * " items
    | FsErasedUnion arms -> $"U{arms.Length}<{commas arms}>"
    | FsDelegate(arguments, returns) -> $"Func<{commas (arguments @ [ returns ])}>"
    | FsFunc(argument, returns) -> $"({typeSpelling argument} -> {typeSpelling returns})"
    | FsTypeVar name -> $"'{name}"
    | FsApp(name, arguments) -> $"{name}<{commas arguments}>"
    | FsBranded(primitive, measure) -> $"{typeSpelling primitive}<{measure}>"
    | FsNamed name -> name

/// The spelling a callback takes (D5, revised by D5a). An F# function type carries the declared
/// arity across the Fable boundary in both directions for one argument or none, and only where the
/// return is not itself a callback: Fable flattens `A -> (B -> C)` into a single JavaScript
/// function of the summed arity, and compiles a call site of two or more arguments one argument at
/// a time. `System.Func`/`System.Action` guarantee the arity everywhere else, and the finding
/// records the cost.
let internal callbackRef (owner: string) (parameters: FsTypeRef list) (returns: FsTypeRef) =
    let retainedFor reason =
        FsDelegate(parameters, returns), [ Finding.make owner (TypeReference.CallbackKeptAsDelegate reason) ]

    match parameters, returns with
    | _, FsFunc _ -> retainedFor "its return is itself a callback"
    | [], _ -> FsFunc(FsUnit, returns), []
    | [ argument ], _ -> FsFunc(argument, returns), []
    | _ -> retainedFor $"it takes {parameters.Length} arguments"

/// Whether a callback is written as an F# function type at a reference position: one argument or
/// none, and a return that is written the same way (D5a). `names` is the naming in force, because
/// a callback this run declares under a name of its own is written as that name and a callback
/// returning one is retained. `seen` cuts a callback reached from its own return.
let rec private functionShapedCallback
    (model: ShapeModel)
    (names: Map<int<Measure.typeId>, string>)
    (seen: Set<int<Measure.typeId>>)
    (facts: TypeFacts)
    =
    isPureCallback facts
    && not (Map.containsKey facts.Response.TypeId names)
    && not (Set.contains facts.Response.TypeId seen)
    && (match facts.CallSignatures with
        | [] -> false
        | first :: _ ->
            let signature = expandTupleRest model first

            signature.Parameters.Length <= 1
            && (match Map.tryFind signature.ReturnTypeId model.Types with
                | Some returns -> not (functionShapedCallback model names (Set.add facts.Response.TypeId seen) returns)
                | None -> true))

/// Whether `callbackRef` will retain this callback as a delegate rather than write it as an F#
/// function type. Read before shaping, so `synthesize-anonymous` can declare a name for the ones
/// that will need it.
let internal callbackRetainedAsDelegate
    (model: ShapeModel)
    (names: Map<int<Measure.typeId>, string>)
    (facts: TypeFacts)
    =
    isPureCallback facts && not (functionShapedCallback model names Set.empty facts)

/// The model for shaping the property or value `name` at `owner`. Where `anyReturnAsUnit`
/// matches `name`, a callback typed directly at `owner` returns `unit` for a source `any`;
/// otherwise every callback keeps its `any` return.
let internal selectAnyReturn (ctx: Context) (model: ShapeModel) (name: string) (owner: string) =
    let selected =
        if GeneratorConfig.selectsAnyReturn ctx.Config name then
            Some owner
        else
            None

    if model.AnyReturnOwner = selected then
        model
    else
        { model with AnyReturnOwner = selected }

/// A call signature's parameter types, in order, ahead of any type resolution: the identity an
/// overload set separates on everywhere but its return type.
let private overloadParameterKey (model: ShapeModel) (signature: ResolvedSignature) : int<Measure.typeId> list =
    (expandTupleRest model signature).Parameters |> List.map _.TypeId

/// Whether every call signature on a callback is distinguishable from every other by parameter
/// count and parameter types alone (§4.2's overload separation, extended to callbacks): two
/// signatures of the same arity and identical parameter types, differing only in their return
/// type, collapse to one key and make the whole set inseparable - .NET overload resolution never
/// consults a return type.
let internal callSignaturesSeparable (model: ShapeModel) (facts: TypeFacts) =
    let keys = facts.CallSignatures |> List.map (overloadParameterKey model)
    List.distinct keys |> List.length = List.length keys

// ---------------------------------------------------------------------------------------------
// Literal-typed parameters that separate an overload set (§4.2).
// ---------------------------------------------------------------------------------------------

/// How far into a parameter type a literal is read for. `KVNamespaceGetOptions<"text"> |
/// undefined` reaches its literal at depth two, and the corpus carries nothing deeper.
[<Literal>]
let private LiteralReach = 4

/// The string literals a parameter type carries at the positions widening erases: the type
/// itself, the members of a union, and the arguments of an instantiation.
let rec internal literalsCarried
    (model: ShapeModel)
    (depth: int)
    (typeId: int<Measure.typeId>)
    : (int<Measure.typeId> * string) list =
    if depth >= LiteralReach then
        []
    else
        match Map.tryFind typeId model.Types with
        | None -> []
        | Some facts when flag TypeFlags.StringLiteral facts ->
            match literalOf facts with
            | Some(LitString text) -> [ typeId, text ]
            | _ -> []
        | Some facts ->
            facts.UnionMembers @ facts.TypeArguments
            |> List.collect (literalsCarried model (depth + 1))
            |> List.distinct

/// The key a type takes where it carries no literal: the F# type it shapes to, rather than the
/// checker's id for it. Two unions agreeing on their non-nullish members share a key, and that
/// is the equivalence `dedupe-overloads` compares under — the second such union reaches the
/// first's name directly through `namedUnionByMembers`, or through the abbreviation
/// `shape-aliases` writes where the second is declared too.
let internal shapedKey (model: ShapeModel) (typeId: int<Measure.typeId>) : string =
    match Map.tryFind typeId model.Types with
    | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
        match splitNullish model facts with
        // One remaining member shapes as that member; two or more take a union name. The
        // hoisted part is the `option` wrapper, so it separates.
        | hoisted, (_ :: _ :: _ as remaining) ->
            let members = remaining |> List.sort |> List.map string |> String.concat "|"
            let absence = if List.isEmpty hoisted then "" else "?"
            $"union({members}){absence}"
        | _ -> string typeId
    | _ -> string typeId

/// A parameter type with its string literals erased. Two overloads share this key exactly where
/// the literal is the only thing between them, which is the collision `dedupe-overloads` reports
/// and the one a retained literal repairs.
let rec internal literalErasedKey (model: ShapeModel) (depth: int) (typeId: int<Measure.typeId>) : string =
    if depth >= LiteralReach || List.isEmpty (literalsCarried model depth typeId) then
        shapedKey model typeId
    else
        match Map.tryFind typeId model.Types with
        | None -> string typeId
        | Some facts when flag TypeFlags.StringLiteral facts -> "literal"
        | Some facts ->
            let target =
                facts.Response.TargetTypeId
                |> ValueOption.map string
                |> ValueOption.defaultValue ""

            let parts =
                facts.UnionMembers @ facts.TypeArguments
                |> List.map (literalErasedKey model (depth + 1))
                |> String.concat ","

            $"{target}({parts})"

/// The string literals keying a method overload (W5), by parameter position: each required,
/// non-rest parameter typed by a lone string literal. A literal holding Emit macro syntax
/// (`$`, `{` or `}`) is excluded.
let internal literalKeys (model: ShapeModel) (signature: ResolvedSignature) : (int * string) list =
    let last = signature.Parameters.Length - 1

    signature.Parameters
    |> List.indexed
    |> List.choose (fun (index, p) ->
        if p.Optional || (signature.HasRest && index = last) then
            None
        else
            match Map.tryFind p.TypeId model.Types with
            | Some facts when flag TypeFlags.StringLiteral facts && not (flag TypeFlags.EnumLiteral facts) ->
                match literalOf facts with
                | Some(LitString text) when text.IndexOfAny [| '$'; '{'; '}' |] < 0 -> Some(index, text)
                | _ -> None
            | _ -> None)

/// Whether a member's overloads are written as one method per literal key (W5): two or more
/// call signatures, at least one keyed by a literal, on a method or on a required property
/// holding a pure callback whose signatures F# tells apart. An intersection's member is such a
/// property: `Element & { setAttribute(name: "draw", ...): void }`. `literalOverloadSets` and
/// `shapeMembers` share this test.
let internal isLiteralKeyed (model: ShapeModel) (m: ResolvedMember) (memberFacts: TypeFacts) =
    let signatures = memberFacts.CallSignatures

    (hasAny SymbolFlags.Method m.Symbol.Flags
     || (not m.Optional
         && isPureCallback memberFacts
         && callSignaturesSeparable model memberFacts))
    && not (isSymbolKeyed m.Symbol.Name)
    && not (m.Symbol.Name.Contains '`')
    && signatures.Length >= 2
    && signatures |> List.exists (literalKeys model >> List.isEmpty >> not)

/// One member whose overload set stays distinct because a literal-typed parameter separates it.
type internal LiteralOverloadSet =
    {
        /// The member as the finding names it: `KVNamespace.get`.
        Member: string
        /// The parameter the literals are read off.
        Parameter: string
        /// Parameter owner as `typeRef` names it, the literal type id kept there, and the
        /// declaration it is written as.
        Sites: (string * int<Measure.typeId> * string) list
        /// Declaration name, the literal it stands for, and the position it sorts under.
        Declared: (string * string * DeclOrder option) list
    }

/// The overload sets a literal-typed parameter separates, over every declaration of the run.
///
/// A member's call signatures group by their parameter types with literals erased: a group of
/// two or more is a collision F# would reject. Within a group, a position whose signatures each
/// carry at most one literal and disagree on which one is a position the literal keeps. A
/// method's literal-keyed overloads are methods of their own (`isLiteralKeyed`), so retention
/// covers nested literals, optional callbacks and exported functions.
let private literalOverloadSets (model: ShapeModel) : LiteralOverloadSet list =
    let existing = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList

    let setsFor (owner: string) (order: DeclOrder option) (members: (string * ResolvedSignature list) list) =
        let mutable taken = existing
        // One declaration per literal per owner: `get` and `getWithMetadata` both keep `"text"`,
        // and both read it as the same type.
        let mutable declarations = Map.empty

        let declarationOf (text: string) =
            match Map.tryFind text declarations with
            | Some name -> name
            | None ->
                let candidate = nestUnder owner (Naming.enumCaseOfString text)

                let name =
                    if Set.contains candidate taken then
                        Seq.initInfinite (fun i -> $"{candidate}{i + 2}")
                        |> Seq.find (fun name -> not (Set.contains name taken))
                    else
                        candidate

                taken <- Set.add name taken
                declarations <- Map.add text name declarations
                name

        members
        |> List.choose (fun (memberName, signatures) ->
            if signatures.Length < 2 then
                None
            else

                let colliding =
                    signatures
                    |> List.groupBy (fun signature ->
                        signature.Parameters
                        |> List.map (fun p -> p.Optional, literalErasedKey model 0 p.TypeId))
                    |> List.map snd
                    |> List.filter (fun group -> group.Length > 1)

                // A position keeps its literals where each signature of the group carries one
                // literal at most and two of them disagree on which. A union of literals is left
                // alone: one literal type stands for `"a"`, and for `"a" | "b"` there is none.
                let separating =
                    colliding
                    |> List.collect (fun group ->
                        let arity = group |> List.map (fun s -> s.Parameters.Length) |> List.min

                        [ 0 .. arity - 1 ]
                        |> List.choose (fun position ->
                            let carried =
                                group
                                |> List.map (fun s ->
                                    let p = s.Parameters[position]
                                    p, literalsCarried model 0 p.TypeId)

                            let lone = carried |> List.forall (fun (_, literals) -> literals.Length <= 1)

                            let distinct =
                                carried |> List.map (snd >> List.map snd) |> List.distinct |> List.length

                            if lone && distinct > 1 then
                                Some(
                                    carried
                                    |> List.collect (fun (p, literals) -> literals |> List.map (fun l -> p, l))
                                )
                            else
                                None))
                    |> List.concat
                    |> List.distinctBy (fun (p, (literalId, _)) -> p.Symbol.Name, literalId)

                match separating with
                | [] -> None
                | separating ->
                    let kept =
                        separating
                        |> List.map (fun (p, (literalId, text)) -> p, literalId, text, declarationOf text)

                    let sites =
                        kept
                        |> List.collect (fun (p, literalId, _, name) ->
                            [ memberName; Naming.memberName memberName ]
                            |> List.distinct
                            |> List.map (fun spelling -> $"{owner}.{spelling}({p.Symbol.Name})", literalId, name))

                    // The return position keeps the literal too, where a signature of the set
                    // returns one of the literals it was separated on.
                    let returnSites =
                        signatures
                        |> List.collect (fun s ->
                            match literalsCarried model 0 s.ReturnTypeId with
                            | [ literalId, text ] when
                                kept |> List.exists (fun (_, _, keptText, _) -> keptText = text)
                                ->
                                [ memberName; Naming.memberName memberName ]
                                |> List.distinct
                                |> List.map (fun spelling -> $"{owner}.{spelling}()", literalId, declarationOf text)
                            | _ -> [])

                    Some
                        {
                            // A well-known symbol's escaped name ends in a session-specific
                            // checker id (`__@toPrimitive@1085`); the label omits it.
                            Member =
                                let at = memberName.LastIndexOf '@'

                                if
                                    isSymbolKeyed memberName
                                    && at > 2
                                    && memberName.Substring(at + 1) |> Seq.forall System.Char.IsDigit
                                then
                                    $"{owner}.{memberName.Substring(0, at)}"
                                else
                                    $"{owner}.{Naming.memberName memberName}"
                            Parameter = (separating |> List.head |> fst).Symbol.Name
                            Sites = sites @ returnSites
                            Declared =
                                kept
                                |> List.map (fun (_, _, text, name) -> name, text, order)
                                |> List.distinctBy (fun (name, _, _) -> name)
                        })

    let typeSets =
        model.DeclNames
        |> Map.toList
        |> List.sortBy fst
        |> List.collect (fun (typeId, owner) ->
            match Map.tryFind typeId model.Types with
            // DeclNames contains declarations this run shapes, including shipped groups.
            // Their nested literal types follow the parent's group in Pipeline.groupModules.
            // Restricting this to the entry package changes a shared declaration's API when
            // reached from a consumer and drops otherwise distinguishable overloads.
            | Some facts ->
                let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                // A literal-keyed method's keyed overloads are methods of their own (W5), so
                // only its other overloads can collide.
                let members =
                    facts.Members
                    |> List.choose (fun m ->
                        Map.tryFind m.TypeId model.Types
                        |> Option.map (fun memberFacts ->
                            let signatures =
                                if isLiteralKeyed model m memberFacts then
                                    memberFacts.CallSignatures |> List.filter (literalKeys model >> List.isEmpty)
                                else
                                    memberFacts.CallSignatures

                            m.Symbol.Name, signatures))

                setsFor owner order members
            | _ -> [])

    // An exported function's overloads separate the same way, under the container that binds
    // them: `Exports.dispatch(kind)` keeps `"left"` as `Exports.Left`.
    let exportSets =
        let containers = ExportLayout.containers model

        model.Harvest.Exports
        |> List.choose (fun export ->
            if
                not export.HasValueExport
                || hasAny SymbolFlags.Class export.Symbol.Flags
                || export.ExportName = "default"
            then
                None
            else
                let owner = ExportLayout.ownerOf model.RuntimePackage export.Origin

                Map.tryFind owner containers
                |> Option.bind (fun container ->
                    Map.tryFind export.Symbol.SymbolId model.ExportTypes
                    |> Option.bind _.Value
                    |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)
                    |> Option.map (fun facts -> container, export.Order, export.ExportName, facts.CallSignatures)))
        |> List.groupBy (fun (container, _, _, _) -> container)
        |> List.collect (fun (container, exports) ->
            let order = exports |> List.tryPick (fun (_, order, _, _) -> order)
            setsFor container order (exports |> List.map (fun (_, _, name, signatures) -> name, signatures)))

    typeSets @ exportSets |> List.distinctBy _.Member

/// The analysis is a function of the type table and the names the run declares, both of which
/// are fixed before any reference is written, so it is computed once per run rather than at
/// every literal.
let private literalOverloadCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<obj, LiteralOverloadSet list>()

let internal literalOverloads (model: ShapeModel) : LiteralOverloadSet list =
    literalOverloadCache.GetValue(
        model.DeclNames,
        System.Runtime.CompilerServices.ConditionalWeakTable<_, _>.CreateValueCallback(fun _ ->
            literalOverloadSets model)
    )

/// Parameter owner and literal type id -> the declaration the literal is written as.
let private retainedSites (model: ShapeModel) =
    literalOverloads model
    |> List.collect _.Sites
    |> List.map (fun (owner, literalId, name) -> (owner, literalId), name)
    |> Map.ofList

let private retainedCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<obj, Map<string * int<Measure.typeId>, string>>()

/// The declaration a string literal keeps at this position, where keeping it separates an
/// overload set.
let internal retainedLiteral (model: ShapeModel) (owner: string) (typeId: int<Measure.typeId>) : string option =
    let sites =
        retainedCache.GetValue(
            model.DeclNames,
            System.Runtime.CompilerServices.ConditionalWeakTable<_, _>.CreateValueCallback(fun _ -> retainedSites model)
        )

    Map.tryFind (owner, typeId) sites

// ---------------------------------------------------------------------------------------------
// Overload sets a `keyof` bound separates in TypeScript alone (§4.9, §4.10).
// ---------------------------------------------------------------------------------------------

/// What `keyof` reaches the shape as: the open form, where the checker deferred the operand,
/// and the union of literal keys a closed operand expands to.
let private isKeySet (model: ShapeModel) (boundId: int<Measure.typeId>) =
    match Map.tryFind boundId model.Types with
    | None -> false
    | Some bound ->
        flag TypeFlags.Index bound
        || (flag TypeFlags.Union bound
            && not bound.UnionMembers.IsEmpty
            && bound.UnionMembers
               |> List.forall (fun id ->
                   match Map.tryFind id model.Types with
                   | Some m -> flag TypeFlags.StringLiteral m
                   | None -> false))

/// Whether a signature binds a type parameter over a key set.
let private boundByKeySet (model: ShapeModel) (signature: ResolvedSignature) =
    signature.TypeParameters
    |> List.exists (fun id ->
        match Map.tryFind id model.Types with
        | Some parameter -> parameter.Constraint |> Option.exists (isKeySet model)
        | None -> false)

/// The members whose overload set is told apart by a `keyof` bound and by nothing else F# reads.
/// `closest<K extends keyof HTMLElementTagNameMap>(selector: K)` writes `'K` bare: .NET keeps
/// constraints out of a method signature, and the three tag-name maps carry no F# name for
/// `keyof<'T>` to stand over, so every overload of the set arrives at one parameter type.
///
/// Each member is named under both spellings a drop is reported by: the symbol's own and F#'s.
let internal keyBoundedOverloads (model: ShapeModel) : Set<string> =
    model.DeclNames
    |> Map.toList
    |> List.collect (fun (typeId, owner) ->
        match Map.tryFind typeId model.Types with
        | None -> []
        | Some facts ->
            facts.Members
            |> List.collect (fun m ->
                match Map.tryFind m.TypeId model.Types with
                | Some memberFacts when
                    (memberFacts.CallSignatures |> List.filter (boundByKeySet model) |> List.length) > 1
                    ->
                    [ m.Symbol.Name; Naming.memberName m.Symbol.Name ]
                    |> List.distinct
                    |> List.map (fun spelling -> $"{owner}.{spelling}")
                | _ -> []))
    |> Set.ofList

// ---------------------------------------------------------------------------------------------
// Indexed access over an operand whose own keys confine the index (§4.10).
// ---------------------------------------------------------------------------------------------

/// How far a bound is followed before an operand or a key set is called unenumerable. Three
/// links reach `Sink extends Source extends Record<string, Event>`; a chain longer than that is
/// a type-level computation the mapping declines rather than approximates.
[<Literal>]
let private BoundReach = 3

/// What an index type confines an access to.
type private KeySet =
    /// `keyof T`, deferred at the operand carried here.
    | AllKeysOf of operand: int<Measure.typeId>
    /// A literal key, or a finite union of them.
    | Named of keys: string list

/// The type an operand stands for. A type parameter stands for its bound, which is the tightest
/// thing true of every instantiation.
let rec private operandShape (model: ShapeModel) (depth: int) (typeId: int<Measure.typeId>) : TypeFacts option =
    if depth > BoundReach then
        None
    else
        match Map.tryFind typeId model.Types with
        | Some facts when flag TypeFlags.TypeParameter facts ->
            facts.Constraint
            |> Option.filter (fun bound -> bound <> typeId)
            |> Option.bind (operandShape model (depth + 1))
        | facts -> facts

/// The keys an index type admits, where they can be enumerated. A key variable stands for its
/// bound, the same way an operand does.
let rec private keySetOf (model: ShapeModel) (depth: int) (typeId: int<Measure.typeId>) : KeySet option =
    if depth > BoundReach then
        None
    else
        match Map.tryFind typeId model.Types with
        | None -> None
        | Some facts when flag TypeFlags.Index facts ->
            facts.Response.TargetTypeId |> ValueOption.toOption |> Option.map AllKeysOf
        | Some facts when flag TypeFlags.StringLiteral facts ->
            match literalOf facts with
            | Some(LitString text) -> Some(Named [ text ])
            | _ -> None
        | Some facts when flag TypeFlags.TypeParameter facts ->
            facts.Constraint
            |> Option.filter (fun bound -> bound <> typeId)
            |> Option.bind (keySetOf model (depth + 1))
        | Some facts when flag TypeFlags.Union facts && not facts.UnionMembers.IsEmpty ->
            let keys =
                facts.UnionMembers
                |> List.choose (fun id ->
                    match Map.tryFind id model.Types |> Option.bind literalOf with
                    | Some(LitString text) -> Some text
                    | _ -> None)

            if keys.Length = facts.UnionMembers.Length then
                Some(Named keys)
            else
                None
        | _ -> None

/// The value types `T[K]` selects, where `K` is confined to keys `T` declares. `None` leaves the
/// access widened: an operand with no enumerable keys, a key set taken over some other type, or
/// a key the operand does not declare all reach past what the operand's own members pin down.
let internal indexedAccessValues (model: ShapeModel) (facts: TypeFacts) : int<Measure.typeId> list option =
    match facts.Response.ObjectTypeId |> ValueOption.toOption, facts.Response.IndexTypeId |> ValueOption.toOption with
    | None, _
    | _, None -> None
    | Some objectId, Some indexId ->
        match operandShape model 0 objectId, keySetOf model 0 indexId with
        | Some operand, Some(AllKeysOf keyOperand) when keyOperand = objectId ->
            // Every value the operand carries, however it carries it: an index signature is as
            // much a key of the operand as a declared member is.
            match
                [
                    for m in operand.Members -> m.TypeId
                    for index in operand.IndexInfos -> index.ValueTypeId
                ]
            with
            | [] -> None
            | values -> Some values
        | Some operand, Some(Named keys) when not keys.IsEmpty ->
            let selected =
                keys
                |> List.choose (fun key ->
                    operand.Members
                    |> List.tryFind (fun m -> m.Symbol.Name = key)
                    |> Option.map _.TypeId)

            if selected.Length = keys.Length then
                Some selected
            else
                None
        | _ -> None

/// The type parameter `T` of a `T[keyof T]` access: the index is `keyof T`, or a type parameter
/// whose constraint and default are both `keyof T` (type-fest `ValueOf`).
let internal valueOfOperand (model: ShapeModel) (facts: TypeFacts) : int<Measure.typeId> option =
    let keyOfTarget (id: int<Measure.typeId>) =
        Map.tryFind id model.Types
        |> Option.filter (flag TypeFlags.Index)
        |> Option.bind (fun index -> index.Response.TargetTypeId |> ValueOption.toOption)

    match facts.Response.ObjectTypeId |> ValueOption.toOption, facts.Response.IndexTypeId |> ValueOption.toOption with
    | Some objectId, Some indexId when flag TypeFlags.IndexedAccess facts ->
        let operandIsParameter =
            Map.tryFind objectId model.Types |> Option.exists (flag TypeFlags.TypeParameter)

        let selectsEveryKey =
            match Map.tryFind indexId model.Types with
            | Some index when flag TypeFlags.Index index -> keyOfTarget indexId = Some objectId
            | Some index when flag TypeFlags.TypeParameter index ->
                (index.Constraint |> Option.bind keyOfTarget) = Some objectId
                && (index.Default |> Option.bind keyOfTarget) = Some objectId
            | _ -> false

        if operandIsParameter && selectsEveryKey then
            Some objectId
        else
            None
    | _ -> None

// ---------------------------------------------------------------------------------------------
// Type references.
// ---------------------------------------------------------------------------------------------

/// The type ids on the current reference descent, cutting a shape reached from itself. A named
/// cycle terminates through `DeclNames`; an unnamed one, as `lib.dom.d.ts` writes, needs this.
/// A path, not a cache: an id is removed on the way out, so side-by-side uses shape twice.
type internal Descent() =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private path: System.Collections.Generic.HashSet<int<Measure.typeId>>

    static member Path =
        if isNull Descent.path then
            Descent.path <- System.Collections.Generic.HashSet<int<Measure.typeId>>()

        Descent.path

/// The name a type parameter is bound under in scope: by its type id, or by its symbol. A
/// conditional's check type is a type distinct from the declaration's parameter it was written
/// as, sharing that parameter's symbol.
let internal typeVarOf (model: ShapeModel) (typeId: int<Measure.typeId>) (facts: TypeFacts) =
    match Map.tryFind typeId model.TypeVars with
    | Some name -> Some name
    | None ->
        facts.Response.SymbolId
        |> ValueOption.toOption
        |> Option.bind (fun symbol ->
            model.TypeVars
            |> Map.tryPick (fun boundId name ->
                match Map.tryFind boundId model.Types with
                | Some bound when bound.Response.SymbolId = ValueSome symbol -> Some name
                | _ -> None))

/// `typeVarOf` for a type named by id alone, an operand read off another type's facts.
let internal typeVarById (model: ShapeModel) (typeId: int<Measure.typeId>) =
    match Map.tryFind typeId model.Types with
    | Some facts -> typeVarOf model typeId facts
    | None -> Map.tryFind typeId model.TypeVars

/// The F# type written at a reference position, with the findings any widening produces.
/// `self` is the name of the declaration being shaped, so a polymorphic `this` return resolves
/// to it. A type carries several flags at once, so the arm order below picks which finding.
let rec typeRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (typeId: int<Measure.typeId>)
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
    (typeId: int<Measure.typeId>)
    : FsTypeRef * Finding list =
    match Map.tryFind typeId model.Types with
    | None ->
        match Map.tryFind typeId model.NotFollowed with
        | Some reason -> FsObj, [ Finding.make owner (TypeReference.TypeNotResolved reason) ]
        | None -> FsObj, [ Finding.make owner TypeReference.MissingFromTypeTable ]
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
            match retainedLiteral model owner typeId, literalOf facts with
            | Some name, Some(LitString text) ->
                FsNamed name, [ Finding.make owner (TypeReference.StringLiteralKeptForOverload text) ]
            | _ -> FsString, [ Finding.make owner TypeReference.StringLiteralToString ]
        elif has TypeFlags.NumberLiteral then
            FsFloat, [ Finding.make owner TypeReference.NumericLiteralToFloat ]
        elif has TypeFlags.BigIntLiteral then
            FsBigInt, [ Finding.make owner TypeReference.BigIntLiteralToBigInt ]
        elif has TypeFlags.String then
            FsString, []
        elif has TypeFlags.Number then
            FsFloat, []
        elif has TypeFlags.BigInt then
            // F# `bigint` is the native JavaScript `BigInt` under Fable 5, proven by the run
            // gate rather than the compile gate.
            FsBigInt, []
        elif has TypeFlags.TemplateLiteral then
            // `` `on${string}` `` is a string at runtime; the pattern is what is lost (§4.11).
            // A *closed* template literal does not reach here: the checker expands one over
            // finite unions into its union of literals, which takes the StringEnum path.
            FsString, [ Finding.make owner TypeReference.TemplateLiteralToString ]
        elif has TypeFlags.StringMapping then
            // `Uppercase<T>` over an operand the checker could not finish. Same argument: the
            // result is a string, and only the transform is lost.
            FsString, [ Finding.make owner TypeReference.StringMappingToString ]
        elif has TypeFlags.NonPrimitive then
            // TypeScript's `object`. `obj` is the mapping §4.1 asks for, and still a widening:
            // `obj` admits the primitives `object` was written to exclude.
            FsObj, [ Finding.make owner TypeReference.ObjectTypeToObj ]
        elif has TypeFlags.UniqueESSymbol || has TypeFlags.ESSymbol then
            // A `unique symbol` is a nominal singleton, and F# has no form for that identity.
            let identity =
                if has TypeFlags.UniqueESSymbol then
                    [ Finding.make owner TypeReference.UniqueSymbolNoBinding ]
                else
                    []

            match symbolRef ctx model self owner facts with
            | Some(reference, findings) -> reference, findings @ identity
            | None -> FsObj, Finding.make owner TypeReference.SymbolNoBinding :: identity
        elif has TypeFlags.Void || has TypeFlags.Undefined || has TypeFlags.Never then
            FsUnit, []
        elif has TypeFlags.Any then
            // Four intrinsic names carry the Any flag: `any` (a written or omitted annotation, a
            // shorthand ambient module, or a diagnosed circular return, accessor or import-alias
            // annotation), `error` and `unresolved` (a reference the program leaves unresolved or
            // invalid, and a circular type alias), and `intrinsic` (the body of a
            // compiler-implemented alias).
            match facts.Response.IntrinsicName with
            | ValueSome "any" -> FsObj, [ Finding.make owner TypeReference.AnyToObj ]
            | ValueSome "intrinsic" -> FsObj, [ Finding.make owner TypeReference.IntrinsicMarkerToObj ]
            //FOR-REVIEW the pinned server names every Any intrinsic (proto.go:984-986); an absent or unknown name is not claimed as a written any
            | _ ->
                let written =
                    facts.UnresolvedName
                    |> Option.map (fun value -> value / uom<symbolName>)
                    |> Option.defaultValue ""

                FsObj, [ Finding.make owner (TypeReference.ErrorTypeToObj written) ]
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
                | Some(KeyOf operand) -> FsApp("keyof", [ operand ]), []
                | Some(TypedKeyOf(operand, _, result, _)) -> FsApp("typekeyof", [ operand; FsTypeVar result ]), []
                | None ->

                    // In scope only where the declaration being shaped bound it (§4.9); a
                    // parameter of some *other* declaration has no name here to write.
                    match typeVarOf model typeId facts with
                    | Some name -> FsTypeVar name, []
                    | None ->
                        // Its constraint is the tightest thing still true of it, and `obj` is
                        // wrong where the declaration bound one: F# rejects `Ai<obj>` against
                        // `'AiModelList :> AiModelListType`. Only a plain named constraint fits.
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
            // A shipped compiler lib declares `Promise` and still writes references to it as
            // `JS.Promise`, ahead of its own declaration.
            match
                (if GeneratorConfig.disposition ctx.Config CompilerLib = Ship then
                     libBinding ctx model self owner facts
                 else
                     None)
            with
            | Some result -> result
            | None -> objectRef ctx model self owner facts
        elif has TypeFlags.Index then
            keyOfRef model owner facts
        elif has TypeFlags.IndexedAccess then
            indexedAccessRef ctx model self owner facts
        elif has TypeFlags.Intersection then
            intersectionRef ctx model self owner facts
        elif has TypeFlags.Conditional then
            conditionalRef ctx model self owner facts
        else
            FsObj,
            [
                Finding.make owner (TypeReference.TypeFlagsNotMapped(string facts.Response.Flags))
            ]

/// `symbol` as the compiler lib's `Symbol` interface, under the name the lib's owner gives it:
/// the shipped Core.TS producer's, or this run's own declaration where the run ships the lib.
/// `None` where that interface has no name in the run.
and internal symbolRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : (FsTypeRef * Finding list) option =
    facts.SymbolApparentType
    |> Option.filter (fun apparent -> Map.containsKey apparent model.Types)
    |> Option.bind (fun apparent ->
        match typeRef ctx model self owner apparent with
        | FsObj, _ -> None
        | reference, findings ->
            let name =
                match reference with
                | FsNamed name -> name
                | _ -> "Symbol"

            Some(reference, findings @ [ Finding.make owner (TypeReference.SymbolAsLibInterface name) ]))

/// `T extends U ? X : Y` at a reference position (§4.11). F# defers no type, so the mapping is
/// a branch wherever the run can name one: the condition holds for every argument the head
/// admits, or one branch is `never` and no application lands in it. A branch that maps to `obj`
/// itself carries what the undifferentiated widening already did, so it reads as deferred. Where
/// no branch maps, a conditional over a bounded type parameter reads its value at that bound
/// under `resolveConditionalConstraints` (W9).
and internal conditionalRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    let name =
        facts.Conditional
        |> Option.bind _.Name
        |> Option.defaultValue "an inline conditional"

    let deferred =
        FsObj, [ Finding.make owner (TypeReference.ConditionalTypeDeferred name) ]

    let constrained () =
        match facts.Conditional |> Option.bind _.Constraint with
        | Some bound ->
            match typeRef ctx model self owner bound with
            | FsObj, _ -> deferred
            | reference, findings ->
                reference,
                findings
                @ [ Finding.make owner (TypeReference.ConditionalResolvedToConstraint name) ]
        | None -> deferred

    match facts.Conditional |> Option.bind _.Branch with
    | None -> constrained ()
    | Some(side, branch) ->
        match typeRef ctx model self owner branch with
        | FsObj, _ -> constrained ()
        | reference, findings ->
            reference,
            findings
            @ [ Finding.make owner (TypeReference.ConditionalResolvedToBranch(name, side)) ]

/// `keyof T` at an operand the checker could not finish (§4.10). A closed `keyof` arrives
/// already expanded into its union of literal keys and shapes as a StringEnum; this open regime
/// maps to the support package's `keyof<'T>`, erased to a string and phantom-typed by operand.
and internal keyOfRef (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let operand =
        facts.Response.TargetTypeId
        |> ValueOption.toOption
        |> Option.bind (typeVarById model)

    match operand with
    | Some name -> FsApp("keyof", [ FsTypeVar name ]), [ Finding.make owner (TypeReference.KeyOfOpenOperand name) ]
    | None -> FsObj, [ Finding.make owner TypeReference.KeyOfOperandOutOfScope ]

/// An intersection at a reference position. A brand (§4.6, D11) maps to the measure its
/// declaration emitted, applied to the primitive it brands; an undeclared brand falls back to
/// the bare primitive with a finding. Object intersections are a separate mapping (§4.6).
and internal intersectionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    match arrayElement model facts with
    | Some element -> arrayRef ctx model self owner facts element
    | None ->

        match brandedPrimitive model facts with
        | Some primitive ->
            match Map.tryFind facts.Response.TypeId model.DeclNames with
            | Some name -> FsBranded(primitive, name), []
            | None -> primitive, [ Finding.make owner TypeReference.UnnamedBrandToPrimitive ]
        | None ->
            // A flattened intersection is declared under a name (§4.6), exactly as a hoisted
            // anonymous object is, and is applied over the parameters it reads the same way.
            match Map.tryFind facts.Response.TypeId model.DeclNames with
            | Some name when isFlattenable model facts ->
                match freeParamsOf model facts.Response.TypeId with
                | [] ->
                    // A *generic alias* over an intersection binds parameters of its own instead,
                    // and `declTypeParams` writes them on the left side - so a self-reference has
                    // to re-apply them, the way `objectRef` re-applies `ownArguments`. Without
                    // this it comes back out bare and `repair-arity` widens it (`RA003`).
                    match facts.AliasTypeArguments with
                    | [] -> FsNamed name, []
                    | parameters -> appliedRef ctx model self owner name parameters
                | arguments -> appliedRef ctx model self owner name arguments
            | _ ->
                match reducedOperand model facts with
                | Some operand ->
                    let reference, findings = typeRef ctx model self owner operand.Response.TypeId

                    reference, findings @ [ Finding.make owner TypeReference.EmptyIntersectionOperandReduced ]
                | None ->

                    match uninhabitedOperand model facts with
                    | Some(operand, property) ->
                        let reference, findings = typeRef ctx model self owner operand.Response.TypeId

                        reference,
                        findings
                        @ [ Finding.make owner (TypeReference.UninhabitedIntersectionReduced property) ]
                    | None ->

                        // Callable operands carry their signatures over to the intersection, which is
                        // the overload set `typeof round & Chained` spells. A member position reads it
                        // as a delegate, the way any other callback reads (D5); an export position
                        // reaches the same signatures and writes them as overloads.
                        if isPureCallback facts && facts.IndexInfos.IsEmpty then
                            let reference, findings = delegateRef ctx model self owner facts

                            reference,
                            findings
                            @ [
                                Finding.make
                                    owner
                                    (TypeReference.IntersectionCallableFlattened facts.CallSignatures.Length)
                            ]
                        else

                            match openIntersectionRef ctx model self owner facts with
                            | Some carried -> carried
                            | None ->

                                let reason =
                                    if facts.Members.IsEmpty && facts.IndexInfos.IsEmpty then
                                        TypeReference.IntersectionOverNonObject
                                    else
                                        TypeReference.IntersectionNotDeclared

                                FsObj, [ Finding.make owner reason ]

/// An intersection over a type parameter (§4.6). Operands that restate a parameter's bound
/// reduce away, and a single remaining parameter is the type. Otherwise the operands nest to the
/// right in the support package's `JS.Intersect`, the parameter staying a parameter and an
/// anonymous operand its own declaration. `None` where an operand has no reference of its own,
/// which leaves the intersection to widen.
and internal openIntersectionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : (FsTypeRef * Finding list) option =
    match openOperands model facts with
    | None -> None
    | Some kept ->
        let rendered =
            kept
            |> List.map (fun operand -> typeRef ctx model self owner operand.Response.TypeId)

        if rendered |> List.exists (fun (reference, _) -> reference = FsObj) then
            None
        else
            let findings = rendered |> List.collect snd

            match rendered |> List.map fst with
            | [ single ] ->
                Some(single, findings @ [ Finding.make owner TypeReference.EmptyIntersectionOperandReduced ])
            | references ->
                let carried =
                    references
                    |> List.reduceBack (fun left right -> FsApp("JS.Intersect", [ left; right ]))

                Some(
                    carried,
                    findings
                    @ [ Finding.make owner (TypeReference.OpenIntersectionCarried references.Length) ]
                )

/// `T[K]`. Where `K` is a key variable this signature bound as `typekeyof<Operand,'R>`, the
/// access is exactly the `'R` that idiom introduced. Where the operand's own keys confine `K`,
/// the access is the union of the value types those keys select - a widening, and a far tighter
/// one than `obj`. `T[keyof T]` over an in-scope `T` is `proptypelock<'T>`. Any other index is a
/// type-level computation with no F# form, and widens loudly.
and internal indexedAccessRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    let binding =
        facts.Response.IndexTypeId
        |> ValueOption.toOption
        |> Option.bind (fun id -> Map.tryFind id model.KeyVars)

    match binding with
    | Some(TypedKeyOf(_, operandId, result, _)) when facts.Response.ObjectTypeId = ValueSome operandId ->
        FsTypeVar result, []
    | _ ->
        match (facts.AliasIdentity |> Option.map (fun (name, id) -> name / uom<symbolName>, id)) with
        | Some(name, operand) when Naming.SupportBindings.shadows name ->
            // `NoInfer<T>` (§4.11's carve-out): every declaration shipping it writes the same
            // identity, so the checker's flags never distinguish it from an ordinary indexed
            // access - only the alias name does. `GeneratorConfig.ResolveNoInfer` picks which
            // side of the support package's `type NoInfer<'T> = 'T` a reference shows.
            let reference, findings = typeRef ctx model self owner operand

            if ctx.Config.ResolveNoInfer then
                reference, findings
            else
                FsApp(Naming.SupportBindings.qualify name, [ reference ]), findings
        | _ ->
            match indexedAccessValues model facts with
            | Some values -> erasedUnionRef ctx model self owner values
            | None ->
                match valueOfOperand model facts |> Option.bind (typeVarById model) with
                | Some name ->
                    FsApp("proptypelock", [ FsTypeVar name ]),
                    [ Finding.make owner (TypeReference.ValueOfAsPropTypeLock name) ]
                | None -> FsObj, [ Finding.make owner TypeReference.IndexedAccessNoForm ]

/// An array-shaped type as an F# array over its element. The members an intersection's other
/// operands contribute are reported as dropped.
and internal arrayRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    (element: int<Measure.typeId>)
    : FsTypeRef * Finding list =
    let inner, findings = typeRef ctx model self owner element

    let dropped =
        match arrayMembersDropped model facts with
        | 0 -> []
        | count -> [ Finding.make owner (TypeReference.ArrayIntersectionMembersDropped count) ]

    FsArray inner, findings @ dropped

and internal objectRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    match arrayElement model facts with
    | Some element -> arrayRef ctx model self owner facts element
    | None ->

        match Map.tryFind facts.Response.TypeId model.DeclNames with
        | Some name ->
            let arguments =
                if Map.containsKey facts.Response.TypeId model.AliasApplications then
                    freeParamsOf model facts.Response.TypeId
                else
                    (ownArguments facts
                     @ declParamIds facts
                     @ freeParamsOf model facts.Response.TypeId)
                    |> List.distinct

            match arguments with
            | [] -> FsNamed name, []
            | arguments -> appliedRef ctx model self owner name arguments
        | None ->
            if isTuple facts then
                tupleRef ctx model self owner facts
            elif isPureCallback facts && (instantiationOf model facts).IsNone then
                delegateRef ctx model self owner facts
            elif isPureIndexSignature facts then
                recordRef ctx model self owner facts
            else

                match instantiationOf model facts with
                | Some(name, arguments) ->
                    let parameters =
                        facts.Response.TargetTypeId
                        |> ValueOption.toOption
                        |> Option.bind (fun target -> Map.tryFind target model.Types)
                        |> Option.map ownArguments
                        |> Option.defaultValue []

                    appliedRefTo ctx model self owner name parameters arguments
                | None ->
                    match
                        libBinding ctx model self owner facts
                        |> Option.orElseWith (fun () -> mappedBinding ctx model self owner facts)
                        |> Option.orElseWith (fun () -> propertyRecordRef ctx model self owner facts)
                    with
                    | Some result -> result
                    | None ->

                        match
                            GeneratorConfig.disposition ctx.Config facts.Origin,
                            (facts.SymbolName |> Option.map (fun value -> value / uom<symbolName>))
                        with
                        | Reference, Some typeName -> referencedRef ctx model self owner facts typeName
                        | Reference, None -> FsObj, [ Finding.make owner TypeReference.AnonymousInReferencedGroup ]
                        | Map _, None -> FsObj, [ Finding.make owner TypeReference.AnonymousInMappedGroup ]
                        | (Ship | Widen | Map _), Some "globalThis" ->
                            FsObj, [ Finding.make owner TypeReference.GlobalThisToObj ]
                        | (Ship | Widen | Map _), _ when isConstructorObject facts ->
                            // A constructor object this run did not name: the generic message
                            // would say `__type is not among the generated declarations`, which
                            // names the checker's placeholder rather than the construct.
                            let constructs =
                                facts.ConstructSignatures
                                |> List.tryPick (fun signature -> Map.tryFind signature.ReturnTypeId model.DeclNames)
                                |> Option.orElse (
                                    facts.SymbolName
                                    |> Option.filter (isSyntheticName >> not)
                                    |> Option.map (fun value -> value / uom<symbolName>)
                                )
                                |> Option.defaultValue "an anonymous class"

                            FsObj, [ Finding.make owner (TypeReference.ConstructorObjectNotDeclared constructs) ]
                        | (Ship | Widen | Map _), _ when facts.Members.IsEmpty && facts.IndexInfos.IsEmpty ->
                            // A member-less object type maps completely to `obj`, and the
                            // declaration it would have taken would hold the same. An
                            // author-written name read from somewhere else stays a declaration
                            // this run owes the reader.
                            match facts.SymbolName with
                            | Some shown when shown / uom<symbolName> <> owner && not (isSyntheticName shown) ->
                                FsObj,
                                [
                                    Finding.make
                                        owner
                                        (TypeReference.NotAmongGeneratedDeclarations <| shown / uom<symbolName>)
                                ]
                            | _ -> FsObj, [ Finding.make owner TypeReference.ObjectWithoutMembers ]
                        | (Ship | Widen | Map _), _ ->
                            let shown =
                                (facts.SymbolName |> Option.map (fun value -> value / uom<symbolName>))
                                |> Option.defaultValue "an anonymous object type"

                            FsObj, [ Finding.make owner (TypeReference.NotAmongGeneratedDeclarations shown) ]

/// `{ [P in keyof T]: V }`, with `T` in scope and `V` independent of `P`, as the support
/// package's `PropertyRecord<'T, V>`.
and internal propertyRecordRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : (FsTypeRef * Finding list) option =
    facts.MappedRecord
    |> Option.filter (fun _ -> facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
    |> Option.bind (fun (operand, value) -> typeVarById model operand |> Option.map (fun name -> name, value))
    |> Option.map (fun (name, value) ->
        let valueRef, valueFindings = typeRef ctx model self owner value

        FsApp("PropertyRecord", [ FsTypeVar name; valueRef ]),
        valueFindings
        @ [ Finding.make owner (TypeReference.MappedAsPropertyRecord name) ])

/// A compiler-lib type a shipped package binds: `Error` -> `exn` and `Promise` -> `JS.Promise<'T>`
/// through `Naming.LibBindings`, with their explicit arity and loss rules; any DOM declaration, a
/// pinned ECMAScript declaration (`Map`, `Uint8Array`), and under `widen` any other ECMAScript
/// declaration, under the name and arguments of the shipped Core.TS producer
/// (`Fable.Core.TS.Dom`, `Fable.Core.TS.Es`). Under `ship`, only the promise names bind.
and internal libBinding (ctx: Context) (model: ShapeModel) (self: string option) (owner: string) (facts: TypeFacts) =
    match facts.Origin, (facts.SymbolName |> Option.map (fun value -> value / uom<symbolName>)) with
    | CompilerLib, Some name when
        GeneratorConfig.disposition ctx.Config CompilerLib <> Ship
        || (facts.NamespacePath.IsEmpty && Naming.LibBindings.bindsWhenShipped name)
        ->
        let arguments = facts.TypeArguments

        // (F# name, arity it takes, the loss the mapping itself costs). A DOM binding costs
        // nothing beyond the arity rule below, so its loss list is always empty.
        let bound =
            match
                (if facts.NamespacePath.IsEmpty then
                     Naming.LibBindings.tryFind name
                 else
                     None)
            with
            | Some(fsharpName, arity, loss) ->
                if arguments.Length < arity then
                    None
                else
                    Some(fsharpName, arity, Option.toList loss)
            | None ->
                match facts.DeclFile |> Option.map (fun value -> value / uom<declFile>) with
                | Some file when Grouping.libFamily file = "Dom" ->
                    Some($"Fable.Core.TS.Dom.{namespaceQualified facts name}", arguments.Length, [])
                | Some file when
                    Grouping.isLibFile file
                    && name <> "globalThis"
                    && not (isSyntheticName (name * uom<symbolName>))
                    && (GeneratorConfig.disposition ctx.Config CompilerLib = Widen
                        || Naming.LibBindings.isPinnedToCoreTs name)
                    ->
                    Some($"Fable.Core.TS.Es.{namespaceQualified facts name}", arguments.Length, [])
                | _ -> None

        match bound with
        | None -> None
        | Some(fsharpName, arity, loss) ->
            let parameters =
                if fsharpName.StartsWith "Fable.Core.TS." then
                    facts.Response.TargetTypeId
                    |> ValueOption.toOption
                    |> Option.bind (fun target -> Map.tryFind target model.Types)
                    |> Option.map ownArguments
                    |> Option.defaultValue []
                else
                    []

            let reference, findings =
                match arity with
                | 0 -> FsNamed fsharpName, []
                | _ -> appliedRefTo ctx model self owner fsharpName parameters (List.truncate arity arguments)

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

/// A type of a mapped group written as the binding somebody already wrote (O7's `map`): the
/// group's table gives the F# name, and that name's arity decides whether the application is
/// legal. A name the table does not carry widens, exactly as the rest of the group does.
and internal mappedBinding
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : (FsTypeRef * Finding list) option =
    match
        GeneratorConfig.disposition ctx.Config facts.Origin,
        (facts.SymbolName |> Option.map (fun value -> value / uom<symbolName>))
    with
    | Map names, Some name ->
        Map.tryFind name names
        |> Option.map (fun destination ->
            let arguments = facts.TypeArguments

            if arguments.Length <> destination.Arity then
                FsObj,
                [
                    Finding.make owner (TypeReference.MappedNameArityMismatch(name, arguments.Length))
                ]
            elif arguments.IsEmpty then
                FsNamed destination.FSharpName, []
            else
                appliedRef ctx model self owner destination.FSharpName arguments)
    | _ -> None

/// A type of a referenced group written as the name a `ship` run of that group produces (O7),
/// applied to the type arguments the site gives, each shaped at its position.
///
/// The shipped declaration's arity stays outside a run resolving the group by identity alone, so
/// every application raises `ReferencedArityUnconfirmed`.
and internal referencedRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    (typeName: string)
    : FsTypeRef * Finding list =
    let moduleName =
        match facts.Origin, (facts.DeclFile |> Option.map (fun value -> value / uom<declFile>)) with
        | CompilerLib, Some file -> Naming.compilerLibFamilyModule (Grouping.libFamily file)
        | CompilerLib, None -> Naming.CompilerLibEsModule
        | origin, _ -> Naming.groupModule ctx.Config ctx.PackageName origin

    let name = $"{moduleName}.{namespaceQualified facts typeName}"

    match facts.TypeArguments with
    | [] -> FsNamed name, []
    | arguments ->
        let reference, findings = appliedRef ctx model self owner name arguments

        let unconfirmed =
            Finding.make owner (TypeReference.ReferencedArityUnconfirmed(typeName, arguments.Length))

        reference, findings @ [ unconfirmed ]

/// A generic name applied to type arguments, each shaped at this position (§4.9).
and internal appliedRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (name: string)
    (arguments: int<Measure.typeId> list)
    : FsTypeRef * Finding list =
    appliedRefTo ctx model self owner name [] arguments

/// The nominal constraint a type parameter's declaration head writes, with the bound's id, or
/// `None` where the head writes none (`constraintProvenNominal`).
and internal statedConstraintOf
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (typeId: int<Measure.typeId>)
    : (int<Measure.typeId> * FsTypeRef) option =
    Map.tryFind typeId model.Types
    |> Option.bind _.Constraint
    |> Option.filter (fun boundId -> boundId <> typeId)
    |> Option.filter (constraintProvenNominal model typeId)
    |> Option.bind (fun boundId ->
        match Map.tryFind boundId model.Types with
        | Some bound when
            flag TypeFlags.Object bound
            && (ownArguments bound).IsEmpty
            && (arrayElement model bound).IsNone
            && not (isTuple bound)
            && not (isPureCallback bound)
            && not (isPureIndexSignature bound)
            ->
            Map.tryFind boundId model.DeclNames
            |> Option.map FsNamed
            |> Option.orElseWith (fun () ->
                match typeRef ctx model self owner boundId with
                | FsApp _ as reference, _ -> Some reference
                | FsNamed name, [] when name <> Naming.LibBindings.FunctionBinding -> Some(FsNamed name)
                | _ -> None)
            |> Option.map (fun reference -> boundId, reference)
        | _ -> None)

/// An application of a generic declaration whose own parameters are known, so each argument is
/// checked against the constraint the declaration states (§4.9). An argument that widened to
/// `obj`, or whose declared bases exclude the constraint, is written as the constraint itself.
and internal appliedRefTo
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (name: string)
    (parameters: int<Measure.typeId> list)
    (arguments: int<Measure.typeId> list)
    : FsTypeRef * Finding list =
    let mutable findings = []
    let statedConstraint = statedConstraintOf ctx model self owner

    // The constraint a key's result variable carries, where the argument is that key's access.
    let keyResultBound (argument: int<Measure.typeId>) =
        Map.tryFind argument model.Types
        |> Option.filter (flag TypeFlags.IndexedAccess)
        |> Option.bind (fun access -> access.Response.IndexTypeId |> ValueOption.toOption)
        |> Option.bind (fun key -> Map.tryFind key model.KeyVars)
        |> Option.bind (function
            | TypedKeyOf(_, _, _, bound) -> bound
            | KeyOf _ -> None)

    let satisfies = satisfiesNominally model

    // `NodeListOf<'E>` holds under `'E :> Element`, because `Element` inherits `Node` (W7.4).
    let variableSatisfies boundId bound argument =
        match statedConstraint argument with
        | Some(argumentBoundId, argumentBound) -> argumentBound = bound || satisfies boundId argumentBoundId
        | None -> false

    // The forms F# seals against a `:>` constraint. The arms below decide the rest by name.
    let sealedForm =
        function
        | FsObj
        | FsTypeVar _
        | FsNamed _
        | FsApp _ -> false
        | _ -> true

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
                    @ [
                        Finding.make owner (TypeReference.ConstrainedArgumentWidened(name, typeSpelling bound))
                    ]

                bound
            | Some(boundId, bound), FsTypeVar variable when
                not (variableSatisfies boundId bound argument)
                && keyResultBound argument <> Some bound
                ->
                findings <-
                    findings
                    @ [
                        Finding.make
                            owner
                            (TypeReference.ArgumentNotBoundWithConstraint(variable, name, typeSpelling bound))
                    ]

                bound
            | Some(boundId, bound), (FsNamed shown | FsApp(shown, _)) when
                reference <> bound && not (satisfies boundId argument)
                ->
                findings <-
                    findings
                    @ [
                        Finding.make
                            owner
                            (TypeReference.ArgumentNotASubtypeOfConstraint(shown, name, typeSpelling bound))
                    ]

                bound
            // TypeScript admits a primitive against a structural bound - `string` has `length` -
            // where `:>` admits it nowhere, so a sealed form is written as the constraint too.
            | Some(boundId, bound), _ when sealedForm reference && not (satisfies boundId argument) ->
                findings <-
                    findings
                    @ [
                        Finding.make
                            owner
                            (TypeReference.ArgumentNotASubtypeOfConstraint(
                                typeSpelling reference,
                                name,
                                typeSpelling bound
                            ))
                    ]

                bound
            | _ -> reference)

    FsApp(name, mapped), findings

/// A tuple as an F# tuple (D7, §4.12): Fable compiles the two to the same JS array, so a fixed
/// tuple is Exact and element labels drop. `[string, number?]` arrives as `number | undefined`,
/// already an `option`. A rest element widens to the shared element type, or `obj[]`.
and internal tupleRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    let mutable findings = []

    // A `...rest` element is reported as the element type and a `[...Spread]` element as the
    // array being spread, so a spread contributes its own element to the widened array.
    let componentOf (flags: ElementFlags) (element: int<Measure.typeId>) =
        let reference, refFindings = typeRef ctx model self owner element
        findings <- findings @ refFindings

        match reference with
        | FsArray inner when flags.HasFlag ElementFlags.Variadic -> inner
        | reference -> reference

    let components = List.map2 componentOf (tupleElementFlags facts) facts.TypeArguments

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
    | first :: rest ->
        let mutable findings =
            if rest.IsEmpty then
                []
            elif callSignaturesSeparable model facts then
                [
                    Finding.make owner (TypeReference.CallbackOverloadsFromFirst(rest.Length + 1))
                ]
            else
                [
                    Finding.make owner (TypeReference.CallbackOverloadsNotSeparable(rest.Length + 1))
                ]

        let signature = expandTupleRest model first

        let parameters =
            signature.Parameters
            |> List.map (fun p ->
                let reference, refFindings =
                    typeRef ctx model self $"{owner}({p.Symbol.Name})" p.TypeId

                findings <- findings @ refFindings
                optionalRef (isOptionalParam p reference) reference)

        let returnOwner = $"{owner}()"

        let returns, returnFindings =
            match Map.tryFind signature.ReturnTypeId model.Types with
            | Some returnFacts when model.AnyReturnOwner = Some owner && isSourceAny returnFacts ->
                FsUnit, [ Finding.make returnOwner TypeReference.AnyReturnAsUnit ]
            | _ -> typeRef ctx model self returnOwner signature.ReturnTypeId

        let reference, callbackFindings = callbackRef owner parameters returns
        reference, findings @ returnFindings @ callbackFindings

/// A pure index signature with no name of its own, resolved through the support package
/// rather than minted a declaration (§4.10, TR059): `Record<'Key, 'Value>` for a writable
/// index, `ReadonlyRecord<'Key, 'Value>` for a readonly one - the get-only form a readonly
/// index signature already renders as `MB.IndexSignatureAsIndexer`. Qualified names retain
/// the support declaration even when a shipped library or consumer declares `Record`.
and internal recordRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    let info = List.exactlyOne facts.IndexInfos
    let key, keyFindings = typeRef ctx model self owner info.KeyTypeId
    let value, valueFindings = typeRef ctx model self owner info.ValueTypeId

    let name = if info.IsReadonly then "ReadonlyRecord" else "Record"

    FsApp(Naming.SupportBindings.qualify name, [ key; value ]),
    keyFindings
    @ valueFindings
    @ [
        Finding.make owner (TypeReference.IndexSignatureAsRecord(typeSpelling key, typeSpelling value))
    ]

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

            let absence = absenceAcross model hoisted

            wrapped,
            Finding.make
                owner
                (TypeReference.NullableHoistedToOption(absence.FromNull, absence.FromUndefined, absence.FromVoid))
            :: findings

    // A reference to a tagged union applies the parameters its declaration binds (§4.9): an
    // alias application its recovered arguments, any other declaration its own parameters and
    // the free ones it reads.
    let namedUnionRef (typeId: int<Measure.typeId>) (name: string) =
        match
            Map.tryFind typeId model.AliasApplications
            |> Option.bind (fun declared -> Map.tryFind declared model.Types),
            Map.tryFind typeId model.Types
        with
        | Some declared, _ ->
            match freeParamsOf model typeId with
            | [] -> FsNamed name, []
            | arguments -> appliedRefTo ctx model self owner name (declParamIds declared) arguments
        | None, Some named when isObjectUnionDeclaration model named ->
            match declParamIds named @ freeParamsOf model typeId |> List.distinct with
            | [] -> FsNamed name, []
            | arguments -> appliedRef ctx model self owner name arguments
        | _ -> FsNamed name, []

    // A member-set match onto a generic tagged declaration outside its scope - the arms a
    // phantom parameter leaves shared by every application - is written over the arguments of
    // the site's own application of the same alias, and declined where the site carries none.
    let matchedUnionRef (typeId: int<Measure.typeId>) (name: string) =
        let inScope id =
            Map.containsKey id model.TypeVars || Map.containsKey id model.KeyVars

        match Map.tryFind typeId model.AliasApplications, Map.tryFind typeId model.Types with
        | None, Some named when isObjectUnionDeclaration model named ->
            let own = declParamIds named
            let free = freeParamsOf model typeId

            if own @ free |> List.forall inScope then
                Some(namedUnionRef typeId name)
            else
                [
                    Some facts
                    facts.NonNullableApplication
                    |> Option.bind (fun id -> Map.tryFind id model.Types)
                ]
                |> List.choose id
                |> List.tryPick (fun site ->
                    match site.Response.AliasSymbolId, site.Response.AliasArgumentTypeIds with
                    | ValueSome alias, ValueSome arguments when
                        named.Response.AliasSymbolId = ValueSome alias
                        && List.isEmpty free
                        && arguments.Length = own.Length
                        && arguments |> Array.forall (fun argument -> Map.containsKey argument model.Types)
                        ->
                        Some(Array.toList arguments)
                    | _ -> None)
                |> Option.map (appliedRefTo ctx model self owner name own)
        | _ -> Some(namedUnionRef typeId name)

    match facts.NonNullableAlias with
    | Some alias ->
        let reference, findings = typeRef ctx model self owner alias
        wrap reference findings
    | None ->
        match remaining with
        | [] ->
            let absence = absenceAcross model hoisted

            FsUnit,
            [
                Finding.make
                    owner
                    (TypeReference.OnlyNullUndefinedToUnit(absence.FromNull, absence.FromUndefined, absence.FromVoid))
            ]
        | [ single ] ->
            let inner, findings = typeRef ctx model self owner single
            wrap inner findings
        | _ when isBooleanPair model remaining -> wrap FsBool []
        | _ ->
            match Map.tryFind facts.Response.TypeId model.DeclNames with
            | Some name when List.isEmpty hoisted -> namedUnionRef facts.Response.TypeId name
            | Some name ->
                let isLiteralEnum =
                    remaining.Length >= 2
                    && remaining
                       |> List.forall (fun id -> Map.tryFind id model.Types |> Option.bind literalOf |> Option.isSome)

                if not isLiteralEnum then
                    FsNamed name, []
                else
                    wrap (FsNamed name) []
            | None ->
                // An erased-union alias application is written over its arguments. Nullish arms
                // an argument carries stay inside it; optionality added at the site wraps it.
                // An argument rendering as an erased union, directly or through a named
                // abbreviation, keeps the flattened form: a nested `U2<U2<_, _>, _>` admits no
                // single `!^` cast from a constituent.
                let rec erased reference =
                    match reference with
                    | FsOption inner -> erased inner
                    | FsErasedUnion _ -> true
                    | _ -> false

                let nestsErasedUnion (argument: int<Measure.typeId>) =
                    match Map.tryFind argument model.Types with
                    | None -> false
                    | Some argumentFacts ->
                        let reference = fst (typeRef ctx model self owner argument)

                        // An enum or a union declaration casts as its own type; any other
                        // named union is an abbreviation of its arms.
                        let declaredUnion name =
                            model.Decls
                            |> List.exists (function
                                | FsStringEnum decl -> decl.Name = name
                                | FsEnum decl -> decl.Name = name
                                | FsTaggedUnion decl -> decl.Name = name
                                | FsErasedObjectUnion decl -> decl.Name = name
                                | _ -> false)

                        let named () =
                            match reference with
                            | FsNamed name
                            | FsOption(FsNamed name) when
                                flag TypeFlags.Union argumentFacts
                                && not (flag TypeFlags.Boolean argumentFacts)
                                && not (declaredUnion name)
                                ->
                                let wanted = nonNullishMemberSet model argumentFacts

                                let twins =
                                    model.DeclNames
                                    |> Map.filter (fun typeId _ ->
                                        match Map.tryFind typeId model.Types with
                                        | Some candidate when
                                            flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate)
                                            ->
                                            typeId = argument || nonNullishMemberSet model candidate = wanted
                                        | _ -> false)

                                not twins.IsEmpty
                                && (let unnamed =
                                        { model with
                                            DeclNames =
                                                model.DeclNames |> Map.filter (fun id _ -> not (twins.ContainsKey id))
                                        }

                                    erased (fst (typeRef ctx unnamed self owner argument)))
                            | _ -> false

                        erased reference
                        || (erasedAliasApplication model argumentFacts).IsSome
                        || named ()

                let aliasApplication =
                    [
                        Some(facts, false)
                        facts.NonNullableApplication
                        |> Option.bind (fun id -> Map.tryFind id model.Types)
                        |> Option.map (fun site -> site, true)
                    ]
                    |> List.choose id
                    |> List.tryPick (fun (site, wrapped) ->
                        erasedAliasApplication model site
                        |> Option.filter (fun (_, _, arguments) ->
                            arguments
                            |> List.forall (fun argument ->
                                fst (typeRef ctx model self owner argument) <> FsObj
                                && not (nestsErasedUnion argument)))
                        |> Option.map (fun application -> application, wrapped))

                match aliasApplication with
                | Some((name, parameters, arguments), wrapped) ->
                    let reference, findings =
                        appliedRefTo ctx model self owner name parameters arguments

                    if wrapped then
                        wrap reference findings
                    else
                        reference, findings
                | None ->

                    match
                        facts.NonNullableApplication
                        |> Option.filter (fun application -> Map.containsKey application model.DeclNames)
                    with
                    | Some application ->
                        let reference, findings = typeRef ctx model self owner application
                        wrap reference findings
                    | None ->
                        match
                            namedUnionByMembers ctx model remaining
                            |> Option.bind (fun (typeId, name) -> matchedUnionRef typeId name)
                        with
                        | Some(reference, findings) -> wrap reference findings
                        | None ->
                            let reference, findings = erasedUnionRef ctx model self owner remaining in
                            wrap reference findings

/// An unnamed heterogeneous union as Fable's `U2`-`U4` (D4, §4.5(4)). Arms are the members' own
/// F# types, deduplicated after mapping, so an unnamed literal union collapses to `string`.
/// One arm widening to `obj` collapses the whole union to `obj`.
and internal erasedUnionRef
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (memberIds: int<Measure.typeId> list)
    : FsTypeRef * Finding list =
    let mutable findings = []

    let arms =
        memberIds
        |> List.map (fun id ->
            let reference, refFindings = typeRef ctx model self owner id
            findings <- findings @ refFindings
            reference)
        |> List.distinct

    let findings = findings |> List.distinct

    match arms with
    | [] -> FsObj, findings @ [ Finding.make owner TypeReference.EmptyUnionToObj ]
    | [ single ] -> single, findings
    | arms when arms |> List.contains FsObj -> FsObj, findings @ [ Finding.make owner TypeReference.UnionWithObjArm ]
    | arms -> FsErasedUnion arms, findings

/// An optional member or parameter reads as `option`, one level deep however the optionality
/// arrived (a `?` marker, an `undefined` union member, or both).
and optionalRef (optional: bool) (reference: FsTypeRef) =
    match optional, reference with
    | false, reference -> reference
    | true, FsOption _ -> reference
    | true, reference -> FsOption reference

/// A parameter reads as `option` where its declaration marks it `?`, or where its type admits
/// `undefined` and `typeRef` has already hoisted it. F# spells both the same way; `MB001` and
/// `MB006` keep the two apart in the manifest.
and internal isOptionalParam (p: ResolvedMember) (reference: FsTypeRef) =
    p.Optional
    || (match reference with
        | FsOption _ -> true
        | _ -> false)

// ---------------------------------------------------------------------------------------------
// Generics (§4.9).
// ---------------------------------------------------------------------------------------------

/// A declaration's own type parameters, and the scope its members must be shaped under. Both
/// come from one walk so they agree: a member may reference exactly the parameters that earn a
/// name. A constraint survives only where it maps to a named type; `extends keyof T` drops.
let typeParamsOf
    (ctx: Context)
    (model: ShapeModel)
    (owner: string)
    (ids: int<Measure.typeId> list)
    : FsTypeParam list * Map<int<Measure.typeId>, string> * Finding list =
    let mutable findings = []

    let named =
        ids
        |> List.indexed
        |> List.choose (fun (position, id) ->
            match
                Map.tryFind id model.Types
                |> Option.bind (_.SymbolName >> Option.map (fun value -> value / uom<symbolName>))
            with
            | Some name -> Some(id, name)
            | None ->
                findings <- findings @ [ Finding.make owner (TypeParameters.UnnamedTypeParameter position) ]

                None)

    // A sanitised spelling yields to every name already in scope and to each verbatim name in
    // the list, taking the first free numeric suffix: `<$T, _T>` is written `<'_T2, '_T>`.
    let verbatim =
        named
        |> List.map snd
        |> List.filter (fun name -> Naming.typeVariable name = name)
        |> Set.ofList

    let named =
        named
        |> List.fold
            (fun (written, taken) (id, name) ->
                let spelled = Naming.typeVariable name

                let collides =
                    model.TypeVars
                    |> Map.exists (fun otherId allocated -> otherId <> id && allocated = name)
                    || written
                       |> List.exists (fun (otherId, allocated) -> otherId <> id && allocated = name)

                match Map.tryFind id model.TypeVars with
                | Some allocated -> written @ [ id, allocated ], Set.add allocated taken
                | None when spelled = name && not collides -> written @ [ id, name ], Set.add name taken
                | None ->
                    let free =
                        Seq.initInfinite (fun index -> index)
                        |> Seq.map (fun index -> if index = 0 then spelled else $"{spelled}{index + 1}")
                        |> Seq.find (fun candidate -> not (Set.contains candidate taken))

                    findings <-
                        findings
                        @ [
                            Finding.make owner (SynthesizeAnonymous.NameSanitisedForIdentifier(name, free))
                        ]

                    written @ [ id, free ], Set.add free taken)
            ([], model.TypeVars |> Map.toList |> List.map snd |> Set.ofList |> Set.union verbatim)
        |> fst

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
            let boundId = Map.tryFind id model.Types |> Option.bind _.Constraint

            let bound =
                boundId
                |> Option.map (fun boundId ->
                    // Only something that becomes an interface can be an F# base type. A union
                    // renders as a sealed `U_n` or StringEnum, so FS0698 rejects
                    // `'T :> Renderable`; tuples, arrays and delegates are sealed the same way.
                    //
                    // TypeScript reads `'T extends { [key: K]: V }` structurally: any shape
                    // whose properties satisfy `V` proves the bound, named interface or not.
                    // `Record<K, V>` only proves it for a type that
                    // implements that exact interface, which a plain named interface with its
                    // own explicit members never does - so the bound goes unexpressed here the
                    // same way a union's does, rather than rejecting every argument TypeScript
                    // itself accepts.
                    let expressible =
                        match Map.tryFind boundId model.Types with
                        | Some bound ->
                            flag TypeFlags.Object bound
                            && (arrayElement model bound).IsNone
                            && not (isTuple bound)
                            && not (isPureCallback bound)
                            && not (isPureIndexSignature bound)
                        | None -> false

                    if expressible then
                        typeRef ctx scoped None owner boundId
                    else
                        FsObj, [])

            // The bound has an F# form and is still not written: nominally it would reject the
            // declaration's own default argument, so `:>` here is FS0001 waiting to happen at
            // every bare use (§4.9). Distinct from `TP002`, where there is no F# form at all.
            let provable =
                boundId
                |> Option.map (constraintProvenNominal model id)
                |> Option.defaultValue true

            match bound with
            | Some(FsNamed Naming.LibBindings.FunctionBinding, _) ->
                findings <-
                    findings
                    @ [ Finding.make owner (TypeParameters.FunctionConstraintDropped name) ]

                { Name = name; Constraint = None }
            | Some((FsNamed shown | FsApp(shown, _)), _) when not provable ->
                findings <-
                    findings
                    @ [ Finding.make owner (TypeParameters.ConstraintNotProvenNominal(name, shown)) ]

                { Name = name; Constraint = None }
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

/// The parameters a declaration binds on its left side.
let internal declTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    declParamIds facts @ freeParamsOf model facts.Response.TypeId
    |> List.distinct
    |> typeParamsOf ctx model owner

/// The parameters a callback alias binds, which include the signature's own. F# has no rank-2
/// form, so a generic *function type* - `type F = <T>(t: T) => T` - can only be approximated
/// by hoisting the variable onto the alias, which is worth a finding.
///
/// Several call signatures may declare the same name, and F# rejects a head naming one variable
/// twice (FS0037). Declarations sharing a name and a bound are written as one variable, which
/// every signature's uses bind to; a name declared under two bounds keeps a slot per bound, and
/// `repair-arity` prices the head F# refuses.
let internal aliasTypeParams (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) =
    let declared = declParamIds facts @ freeParamsOf model facts.Response.TypeId
    let hoisted = facts.CallSignatures |> List.collect _.TypeParameters |> List.distinct
    let ids = declared @ hoisted |> List.distinct

    // Declarations sharing a name *and* a bound share a variable; a name declared under two
    // bounds keeps a slot per bound, where one variable would retype a signature. An unnamed
    // parameter answers only to itself, so each still reports its own erasure.
    let groups = ids |> List.groupBy (typeParamIdentity model)

    let collapsed =
        groups
        |> List.choose (fun (identity, group) ->
            match identity, group with
            | Ok(name, _), head :: (_ :: _ as tail) -> Some(name, head, tail, group.Length)
            | _ -> None)

    let parameters, scope, findings =
        groups |> List.map (snd >> List.head) |> typeParamsOf ctx model owner

    // Each collapsed id names the variable the head writes, so every signature's uses bind to it.
    let scope =
        collapsed
        |> List.fold
            (fun bound (_, head, tail, _) ->
                let written = Map.find head bound
                tail |> List.fold (fun bound id -> Map.add id written bound) bound)
            scope

    let collapseFindings =
        collapsed
        |> List.map (fun (name, _, _, declared) ->
            Finding.make owner (TypeParameters.DuplicateTypeParameterCollapsed(name, declared)))

    let hoistFindings =
        if hoisted |> List.exists (fun id -> not (List.contains id declared)) then
            [ Finding.make owner TypeParameters.GenericFunctionHoisted ]
        else
            []

    parameters, scope, findings @ collapseFindings @ hoistFindings

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
    | FsFunc(argument, returns) -> Set.union (typeVarsOf argument) (typeVarsOf returns)
    | FsApp(_, arguments) -> union arguments
    | _ -> Set.empty

/// The key variables a signature binds (§4.10): each type parameter whose bound is a `keyof`,
/// paired with the id of the operand that `keyof` was taken over.
let internal keyCandidates
    (model: ShapeModel)
    (ids: int<Measure.typeId> list)
    : (int<Measure.typeId> * int<Measure.typeId>) list =
    ids
    |> List.choose (fun id ->
        let parameter = Map.tryFind id model.Types

        match parameter |> Option.bind _.KeyOfOperand, parameter |> Option.bind _.Constraint with
        | Some operand, _ -> Some(id, operand)
        | None, None -> None
        | None, Some boundId ->
            match Map.tryFind boundId model.Types with
            | Some bound when flag TypeFlags.Index bound ->
                bound.Response.TargetTypeId
                |> ValueOption.toOption
                |> Option.map (fun operand -> id, operand)
            | _ -> None)

/// Whether any of `roots` reaches the indexed access `object[key]` - what tells `key: K` apart
/// from `key: K` *plus* the value it selects. Carriers are followed, members are not: the point
/// is to find `T[K]` where a signature returns it, bare or wrapped, not to walk object graphs.
let internal mentionsAccess
    (model: ShapeModel)
    (objectId: int<Measure.typeId>)
    (keyId: int<Measure.typeId>)
    (roots: int<Measure.typeId> list)
    : bool =
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
                    && facts.Response.ObjectTypeId = ValueSome objectId
                    && facts.Response.IndexTypeId = ValueSome keyId
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

/// The constraint the result variable of `object[key]` carries: the bound of the generic
/// parameter the access is applied at, where every value `object` declares nominally satisfies
/// that bound. Carriers are followed as `mentionsAccess` follows them; the first application of
/// the access decides.
let internal accessBound
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (objectId: int<Measure.typeId>)
    (keyId: int<Measure.typeId>)
    (roots: int<Measure.typeId> list)
    : FsTypeRef option =
    let values =
        match Map.tryFind objectId model.Types with
        | Some operand when not operand.Members.IsEmpty -> operand.Members |> List.map _.TypeId
        | _ ->
            Map.tryFind keyId model.Types
            |> Option.filter (fun key -> key.KeyOfOperand = Some objectId)
            |> Option.map _.KeyOfValues
            |> Option.defaultValue []

    let isAccess (id: int<Measure.typeId>) =
        Map.tryFind id model.Types
        |> Option.exists (fun facts ->
            flag TypeFlags.IndexedAccess facts
            && facts.Response.ObjectTypeId = ValueSome objectId
            && facts.Response.IndexTypeId = ValueSome keyId)

    let boundAt (facts: TypeFacts) =
        match facts.Response.TargetTypeId |> ValueOption.toOption with
        | Some target when not facts.TypeArguments.IsEmpty ->
            let parameters =
                Map.tryFind target model.Types
                |> Option.map ownArguments
                |> Option.defaultValue []

            if parameters.Length = facts.TypeArguments.Length then
                List.zip parameters facts.TypeArguments
                |> List.tryPick (fun (parameter, argument) ->
                    if isAccess argument then
                        statedConstraintOf ctx model self owner parameter
                    else
                        None)
            else
                None
        | _ -> None

    let rec go visited pending =
        match pending with
        | [] -> None
        | id :: rest when Set.contains id visited -> go visited rest
        | id :: rest ->
            match Map.tryFind id model.Types with
            | None -> go (Set.add id visited) rest
            | Some facts ->
                match boundAt facts with
                | Some found -> Some found
                | None ->
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

    match go Set.empty roots with
    | Some(boundId, reference) when not values.IsEmpty && values |> List.forall (satisfiesNominally model boundId) ->
        Some reference
    | _ -> None

/// The name to write the value a key selects under: `stem`, then `stem1`, `stem2`, ... while the
/// name is taken in scope. `R` for a key over an in-scope operand, `E` for a key over a declared
/// map (`typekeyof<HTMLElementTagNameMap, 'E>`).
let internal resultName (stem: string) (taken: Set<string>) =
    let rec pick n =
        let candidate = if n = 0 then stem else $"{stem}{n}"

        if Set.contains candidate taken then
            pick (n + 1)
        else
            candidate

    pick 0

// ---------------------------------------------------------------------------------------------
// Shared shaping of members and signatures.
// ---------------------------------------------------------------------------------------------

/// The member's tags an overload carries: those its own JSDoc writes, or, for an undocumented
/// overload, the shared tags other than `@deprecated`.
let internal signatureTags (tags: JSDocTagInfo list) (signature: ResolvedSignature) =
    match signature.OwnDoc with
    | Some(Documented names) -> tags |> List.filter (fun tag -> Set.contains tag.Name names)
    | Some Undocumented -> tags |> List.filter (fun tag -> tag.Name <> "deprecated")
    | None -> tags

/// A resolved signature as an F# type-parameter list, parameter list and return reference.
/// Rest parameters are marked from the signature flag; their array types read as-is, and a
/// tuple-typed one is written out as the parameters it stands for. A signature's *own*
/// parameters (§4.9) bind here, not at the declaration: `get<T>(source: T)`.
let internal shapeSignature
    (ctx: Context)
    (model: ShapeModel)
    (self: string option)
    (owner: string)
    (declared: ResolvedSignature)
    : FsTypeParam list * FsParam list * FsTypeRef * Finding list =
    let signature = expandTupleRest model declared

    // §4.10, the open keyof regime: a `K extends keyof T` variable is not bound as an F#
    // variable - a bare `'K` would let any type through and drag `T[K]` to obj. The support
    // package's `keyof<'T>` is written at its uses instead.
    let candidates = keyCandidates model signature.TypeParameters

    let plain =
        signature.TypeParameters
        |> List.filter (fun id -> candidates |> List.forall (fun (key, _) -> key <> id))

    let typeParameters, scope, parameterFindings =
        match plain with
        | [] -> [], model.TypeVars, []
        | ids -> typeParamsOf ctx model owner ids

    // A key over an operand that is nowhere in scope has no `'T` to be taken over. Where the
    // operand is a declared map with a written name (`HTMLElementTagNameMap`), the key is taken
    // over that name; otherwise it falls back to an ordinary type parameter and widens like one.
    let bindable, outOfScope =
        candidates |> List.partition (fun (_, operand) -> Map.containsKey operand scope)

    let mapReference (operand: int<Measure.typeId>) =
        if Map.tryFind operand model.Types |> Option.exists (flag TypeFlags.TypeParameter) then
            None
        else
            match typeRef ctx { model with TypeVars = scope } self owner operand with
            | (FsNamed _ | FsApp _) as reference, findings when
                findings |> List.forall (fun f -> f.Tier = Exact || f.Tier = Ergonomic)
                ->
                Some(reference, findings)
            | _ -> None

    let mapped, loose =
        outOfScope
        |> List.map (fun (key, operand) -> key, operand, mapReference operand)
        |> List.partition (fun (_, _, reference) -> reference.IsSome)

    let loose = loose |> List.map (fun (key, operand, _) -> key, operand)

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
            let result = resultName "R" taken
            taken <- Set.add result taken
            resultParameters <- resultParameters @ [ { Name = result; Constraint = None } ]
            keyVars <- Map.add key (TypedKeyOf(FsTypeVar operandName, operand, result, None)) keyVars

            keyFindings <-
                keyFindings
                @ [
                    Finding.make owner (TypeParameters.KeyWithIndexedAccess(operandName, result))
                ]
        else
            keyVars <- Map.add key (KeyOf(FsTypeVar operandName)) keyVars

            keyFindings <- keyFindings @ [ Finding.make owner (TypeParameters.KeyOverOperand operandName) ]

    for key, operand, reference in mapped do
        let map, mapFindings = reference.Value
        let spelling = typeSpelling map

        if mentionsAccess model operand key roots then
            let result = resultName "E" taken
            taken <- Set.add result taken

            let bound =
                accessBound ctx { model with TypeVars = scope } self owner operand key roots

            resultParameters <- resultParameters @ [ { Name = result; Constraint = bound } ]
            keyVars <- Map.add key (TypedKeyOf(map, operand, result, bound)) keyVars

            keyFindings <-
                keyFindings
                @ mapFindings
                @ [
                    Finding.make owner (TypeParameters.KeyOverDeclaredMap(spelling, Some result))
                ]
        else
            keyVars <- Map.add key (KeyOf map) keyVars

            keyFindings <-
                keyFindings
                @ mapFindings
                @ [ Finding.make owner (TypeParameters.KeyOverDeclaredMap(spelling, None)) ]

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

    // F# optional parameters are a tail: `?a: T, b: U` is FS1212, and a rest parameter leaves
    // no tail at all - `setTimeout(callback, ?msDelay, ...args)` the same. `undefined` in a
    // type is admitted anywhere, so an earlier admitting parameter stays required, of `option`.
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
            elif admitsOptional then
                findings <- findings @ [ Finding.make paramOwner Members.OptionalParameterFromUnion ]

            {
                Name = Naming.memberName p.Symbol.Name
                Optional = optional
                Rest = rest
                Type = optionalRef admitsOptional reference
            })

    let returns, returnFindings =
        typeRef ctx model self $"{owner}()" signature.ReturnTypeId

    findings <- findings @ returnFindings

    // Start with the variables value positions still name, then retain the variables their
    // constraints read, transitively: `<T, E extends Box<T>>(env: E)` needs both variables.
    // A constraint belonging to an unused parameter does not keep either one alive.
    let directlyNamed =
        parameters
        |> List.map _.Type
        |> List.fold (fun acc t -> Set.union acc (typeVarsOf t)) (typeVarsOf returns)

    let rec closeConstraints named =
        let expanded =
            typeParameters
            |> List.fold
                (fun found parameter ->
                    if Set.contains parameter.Name named then
                        parameter.Constraint
                        |> Option.map (typeVarsOf >> Set.union found)
                        |> Option.defaultValue found
                    else
                        found)
                named

        if expanded = named then
            named
        else
            closeConstraints expanded

    let named = closeConstraints directlyNamed

    let live, erased =
        typeParameters |> List.partition (fun p -> Set.contains p.Name named)

    for p in erased do
        findings <- findings @ [ Finding.make owner (TypeParameters.TypeParameterErased p.Name) ]

    live, parameters, returns, findings

/// A declared type's content beside the `undefined` an optional declaration carries: the union's
/// non-nullish members, or the type itself where it is not a union. Two declarations agreeing
/// here declare one type, one of them optionally (D1).
let private declaredContent (model: ShapeModel) (typeId: int<Measure.typeId>) =
    match Map.tryFind typeId model.Types with
    | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
        nonNullishMemberSet model facts
    | _ -> [ typeId ]

/// Whether a declared type admits `null` or `undefined` as a union member of its own (D1).
let private carriesNullish (model: ShapeModel) (typeId: int<Measure.typeId>) =
    match Map.tryFind typeId model.Types with
    | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
        facts.UnionMembers.Length <> (nonNullishMemberSet model facts).Length
    | _ -> false

/// The type every operand of an intersection gives a member, where they agree on one (§4.6).
/// The checker types a member several operands declare as the intersection of those declared
/// types, and distributes that intersection over the unions inside it; where the operands
/// declare one type, that type is the member's type and the distribution is a detour.
///
/// The answer is the required declaration's type: the flattened member is required wherever one
/// operand requires it.
let internal agreedMemberType (model: ShapeModel) (facts: TypeFacts) (m: ResolvedMember) =
    if not (flag TypeFlags.Intersection facts) then
        None
    else
        let declaredBy =
            operandsOf model facts
            |> List.choose (fun operand ->
                operand.Members
                |> List.tryFind (fun candidate -> candidate.Symbol.Name = m.Symbol.Name))
            |> List.map _.TypeId

        // A member one operand declares alone carries the checker's substitution into it - a
        // polymorphic `this` resolving to the intersection - and that substitution is the reading
        // the declaration owes.
        if declaredBy.Length < 2 then
            None
        else
            let declared = List.distinct declaredBy

            match declared |> List.map (declaredContent model) |> List.distinct with
            | [ _ ] ->
                declared
                |> List.tryFind (carriesNullish model >> not)
                |> Option.filter (fun agreed -> agreed <> m.TypeId)
            | _ -> None

/// A class an ambient module exports for consumers to derive from: `abstract`, or carrying a
/// base of its own. F# admits no `inherit` of an interface (FS0946), so this is the one shape
/// that reaches a consumer's `type Actor(ctx, env) = inherit DurableObject(ctx, env)`. Every
/// other class keeps the interface form, where the `[<ParamObject>]` Create is the construction
/// a consumer wants.
let internal isEntrypoint
    (ctx: Context)
    (export: HarvestedExport)
    (constructSignatures: ResolvedSignature list)
    (bases: int<Measure.typeId> list)
    =
    match export.Origin with
    | FromAmbientModule specifier ->
        let runtime =
            GeneratorConfig.runtimePackage ctx.Config ctx.PackageName / uom<importSpecifier>

        let publicInput =
            ctx.PublicPaths
            |> List.exists (fun path ->
                let publicSpecifier =
                    if path.Key = "." then
                        runtime
                    else
                        runtime + "/" + path.Key.Substring 2

                publicSpecifier = specifier / uom<importSpecifier>)

        let sourcePackage =
            match Grouping.classify ctx.PackageDir (ValueSome export.Symbol) with
            | EntryPackage -> Some(ctx.PackageName / uom<npmDependency>)
            | Dependency name -> Some(name / uom<npmDependency>)
            | CompilerLib
            | Unclassified -> None

        let packageImport =
            sourcePackage
            |> Option.exists (fun package ->
                let specifier = specifier / uom<importSpecifier>

                specifier = package
                || specifier.StartsWith(package + "/", System.StringComparison.Ordinal))

        not publicInput
        && not packageImport
        && ((constructSignatures |> List.exists _.IsAbstract) || not bases.IsEmpty)
    | FromGlobal
    | FromModule -> false

/// Runtime exports and constructor signatures for public and reached ambient class declarations.
let internal exportedClassSides
    (model: ShapeModel)
    : Map<int<Measure.typeId>, HarvestedExport * ResolvedSignature list> =
    let reached =
        model.Types
        |> Map.toList
        |> List.choose (fun (typeId, facts) -> facts.AmbientClass |> Option.map (fun side -> typeId, side))

    let exported =
        model.Harvest.Exports
        |> List.choose (fun export ->
            if not (hasAny SymbolFlags.Class export.Symbol.Flags) then
                None
            else
                match Map.tryFind export.Symbol.SymbolId model.ExportTypes with
                | Some ids ->
                    match ids.Declared, ids.Value |> Option.bind (fun typeId -> Map.tryFind typeId model.Types) with
                    | Some declared, Some valueFacts -> Some(declared, (export, valueFacts.ConstructSignatures))
                    | _ -> None
                | None -> None)

    reached @ exported |> Map.ofList

/// The call signatures a member declares, read off the non-nullish arms where its type is a
/// union. Under `strictNullChecks` an optional member's type is a union with `undefined`, which
/// carries call signatures of its own only where every arm does.
let internal callSignaturesOf (model: ShapeModel) (typeId: int<Measure.typeId>) : ResolvedSignature list =
    match Map.tryFind typeId model.Types with
    | None -> []
    | Some facts when not facts.CallSignatures.IsEmpty -> facts.CallSignatures
    | Some facts when flag TypeFlags.Union facts ->
        nonNullishMemberSet model facts
        |> List.choose (fun id -> Map.tryFind id model.Types)
        |> List.collect _.CallSignatures
    | Some _ -> []

/// The element type an iterator method yields: the `value` of the first `next` result arm whose
/// `done` is absent or admits `false`; otherwise the returned iterator's first type argument.
let internal iteratedElement (model: ShapeModel) (iteratorTypeId: int<Measure.typeId>) : int<Measure.typeId> option =
    callSignaturesOf model iteratorTypeId
    |> List.tryHead
    |> Option.bind (fun signature -> Map.tryFind signature.ReturnTypeId model.Types)
    |> Option.bind (fun iterator ->
        let memberOf (name: string) (facts: TypeFacts) =
            facts.Members |> List.tryFind (fun m -> m.Symbol.Name = name)

        let finished (arm: TypeFacts) =
            memberOf "done" arm
            |> Option.bind (fun m -> Map.tryFind m.TypeId model.Types)
            |> Option.bind literalOf
            |> (=) (Some(LitBool true))

        let yielded =
            memberOf "next" iterator
            |> Option.bind (fun next -> callSignaturesOf model next.TypeId |> List.tryHead)
            |> Option.bind (fun signature -> Map.tryFind signature.ReturnTypeId model.Types)
            |> Option.bind (fun result ->
                (if flag TypeFlags.Union result then
                     result.UnionMembers |> List.choose (fun id -> Map.tryFind id model.Types)
                 else
                     [ result ])
                |> List.tryFind (fun arm -> not (finished arm) && (memberOf "value" arm).IsSome)
                |> Option.bind (memberOf "value")
                |> Option.map _.TypeId)

        match yielded with
        | Some element -> Some element
        | None -> List.tryHead iterator.TypeArguments)

/// Whether a member is written as a method: a method symbol, or a required `function`
/// declaration of a namespace object, `globalThis` or a class's merged namespace whose type is a
/// callback alone (W7.3). `facts` looks up the member's type.
let internal emitsAsMethodWith (facts: int<Measure.typeId> -> TypeFacts option) (m: ResolvedMember) =
    hasAny SymbolFlags.Method m.Symbol.Flags
    || (m.ResolvesToFunction
        && not m.Optional
        && facts m.TypeId |> Option.exists isPureCallback)

/// `emitsAsMethodWith` over the model's resolved types.
let internal emitsAsMethod (model: ShapeModel) (m: ResolvedMember) =
    emitsAsMethodWith (fun id -> Map.tryFind id model.Types) m

/// A *lifecycle hook*: a method a declaration marks `?`, which the platform calls on an object
/// that provides it. The name is identifier-shaped, because the emission names an interface
/// after it.
let internal isOptionalHook (model: ShapeModel) (m: ResolvedMember) =
    m.Optional
    && hasAny SymbolFlags.Method m.Symbol.Flags
    && Naming.nestable (Naming.memberName m.Symbol.Name)
    && not (callSignaturesOf model m.TypeId).IsEmpty

/// The opt-in interface one lifecycle hook is emitted as: a `fetch?` on `Station` becomes
/// `Station.IFetchHandler`, carrying `fetch` as an abstract member, one per call signature. A
/// subclass implements the interfaces it provides, so `typeof instance.fetch === "function"`
/// holds exactly where it does.
///
/// The interface takes the owner's type parameters where the hook's signature mentions any of
/// them, and none otherwise. One interface per hook: a group is derivable from these by
/// interface inheritance, where a grouped interface admits no decomposition.
let internal shapeHook
    (ctx: Context)
    (model: ShapeModel)
    (self: string)
    (ownerParameters: FsTypeParam list)
    (order: DeclOrder option)
    (m: ResolvedMember)
    : FsDecl * Finding list =
    let key = Naming.memberName m.Symbol.Name
    let owner = $"{self}.{key}"
    let name = nestUnder self $"I{Naming.pascalSegment key}Handler"
    let mutable findings = []

    let overloads =
        callSignaturesOf model m.TypeId
        |> List.collect (restOverloads model)
        |> List.map (fun signature ->
            let typeParameters, parameters, returns, signatureFindings =
                shapeSignature ctx model (Some self) owner signature

            findings <- findings @ signatureFindings

            {
                Name = key
                Docs = ""
                Tags = []
                TypeParameters = typeParameters
                Parameters = parameters
                Return = returns
            })

    let mentioned =
        overloads
        |> List.fold
            (fun found overload ->
                overload.Parameters
                |> List.fold
                    (fun acc p -> Set.union acc (typeVarsOf p.Type))
                    (Set.union found (typeVarsOf overload.Return)))
            Set.empty

    let bound = ownerParameters |> List.map _.Name |> Set.ofList

    let decl =
        FsInterface
            {
                Name = name
                Docs = m.Docs
                Tags = m.Tags
                Order = order
                Callable = false
                TypeParameters =
                    (if Set.isEmpty (Set.intersect mentioned bound) then
                         []
                     else
                         ownerParameters)
                Inherits = []
                Members = overloads |> List.map FsMethod
                Entrypoint = None
                CreateOverloads = []
                Statics = []
            }

    decl, findings @ [ Finding.make owner (Members.OptionalHookAsInterface name) ]

/// The interface members of an object type: methods for method symbols and function declarations
/// (each call signature an overload), properties otherwise, callbacks as delegate-typed
/// properties (D5).
///
/// `hooks` names the members `shape-interfaces` lifts out as opt-in interfaces of their own
/// (`shapeHook`); the declaration carries none of them.
let internal shapeMembers
    (ctx: Context)
    (model: ShapeModel)
    (hooks: Set<string>)
    (self: string)
    (facts: TypeFacts)
    : FsMember list * Finding list =
    let mutable findings = []
    let emit finding = findings <- findings @ [ finding ]

    // W5: names a literal-keyed overload's method takes, by JavaScript key and literals. The
    // member names of the type, inherited ones included, are taken from the start.
    let mutable taken =
        facts.Members
        |> List.map (fun m -> Naming.memberName m.Symbol.Name)
        |> Set.ofList

    let mutable literalNames = Map.empty

    // The method an overload keyed by `literals` reads as: `on("close", listener)` is
    // `onClose(listener)`, or `on_Close` where a member already has that name.
    let literalKeyedMethod (m: ResolvedMember) (signature: ResolvedSignature) (literals: (int * string) list) =
        let target = Naming.memberName m.Symbol.Name
        let texts = literals |> List.map snd

        let name =
            match Map.tryFind (target, texts) literalNames with
            | Some name -> name
            | None ->
                let suffix = texts |> List.map Naming.literalSuffix |> String.concat ""
                let plain = target + suffix
                let separated = target + "_" + suffix

                let name =
                    [ plain; separated ]
                    |> List.tryFind (fun name -> not (Set.contains name taken))
                    |> Option.defaultWith (fun () ->
                        Seq.initInfinite (fun i -> $"{separated}{i + 2}")
                        |> Seq.find (fun name -> not (Set.contains name taken)))

                taken <- Set.add name taken
                literalNames <- Map.add (target, texts) name literalNames

                emit (
                    Finding.make
                        $"{self}.{m.Symbol.Name}"
                        (Members.LiteralOverloadAsMethod(String.concat ", " texts, name))
                )

                name

        let positions = literals |> List.map fst |> Set.ofList

        let reduced =
            { signature with
                Parameters =
                    signature.Parameters
                    |> List.indexed
                    |> List.filter (fun (index, _) -> not (Set.contains index positions))
                    |> List.map snd
            }

        let typeParameters, parameters, returns, signatureFindings =
            shapeSignature ctx model (Some self) $"{self}.{name}" reduced

        findings <- findings @ signatureFindings

        let removed =
            literals
            |> List.map (fun (index, _) -> signature.Parameters[index].Symbol.Name)
            |> Set.ofList

        // The overload's own JSDoc, then its TypeScript signature.
        let docs =
            match signature.Source with
            | Some source ->
                let fence = "```ts\n" + source.Spelling + "\n```"

                if source.Comment = "" then
                    fence
                else
                    source.Comment + "\n\n" + fence
            | None -> m.Docs

        FsLiteralKeyed(
            target,
            literals |> List.mapi (fun before (index, text) -> index - before, text),
            FsMethod
                {
                    Name = name
                    Docs = docs
                    Tags =
                        signatureTags m.Tags signature
                        |> List.filter (fun tag ->
                            match tag.Name, tag.Text with
                            | "param", ValueSome text -> not (Set.contains (text.Trim().Split(' ')[0]) removed)
                            | _ -> true)
                    TypeParameters = typeParameters
                    Parameters = parameters
                    Return = returns
                }
        )

    let members =
        facts.Members
        |> List.filter (fun m ->
            if m.Symbol.Name.Contains '`' then
                emit (Finding.make $"{self}.{m.Symbol.Name}" Members.UnspellableMemberDropped)
                false
            elif (emittedSymbol facts m).IsSome then
                // Shaped below, under the symbol's own F# name.
                false
            elif isSymbolKeyed m.Symbol.Name then
                // The name is cut at the checker id (`__@iterator@1469` -> `__@iterator`):
                // the id is session-specific and would break run-to-run determinism.
                let stable = m.Symbol.Name.Substring(0, m.Symbol.Name.LastIndexOf '@')
                emit (Finding.make $"{self}.{stable}" Members.SymbolKeyedMemberDropped)
                false
            elif isConstructorObject facts && m.Symbol.Name = "prototype" then
                // On a constructor object, `prototype` is the instance side, which is a
                // declaration of its own - `shape-classes` drops it for the same reason.
                false
            elif Set.contains m.Symbol.Name hooks then
                // A lifecycle hook is declared by an interface of its own, so the class carries
                // no member for it and a subclass declining it carries none either.
                false
            else
                true)
        |> List.collect (fun m ->
            let owner = $"{self}.{m.Symbol.Name}"
            let agreed = agreedMemberType model facts m
            let effectiveTypeId = agreed |> Option.defaultValue m.TypeId

            let methodFacts =
                if not (emitsAsMethod model m) then
                    None
                else
                    match Map.tryFind m.TypeId model.Types with
                    | Some memberFacts when not memberFacts.CallSignatures.IsEmpty -> Some memberFacts
                    | _ -> None

            // A property carrying a pure callback of two or more signatures separable by
            // parameter count and type (§4.2, extended to callbacks): F# admits the overload set
            // as one method per signature under the property's own name, so no name is minted for
            // it (Callbacks.fs's TR031/TR062 floor is for the callback's own declaration, not for
            // a member that merely holds one). An optional member is excluded - F#'s method form
            // has no way to carry the option that `m.Optional` demands.
            let recoverableCallbackFacts =
                if methodFacts.IsSome || m.Optional then
                    None
                else
                    match Map.tryFind effectiveTypeId model.Types with
                    | Some memberFacts when
                        isPureCallback memberFacts
                        && memberFacts.CallSignatures.Length > 1
                        && callSignaturesSeparable model memberFacts
                        ->
                        Some memberFacts
                    | _ -> None

            // Writes each call signature as an overload of one method, or as a method of its own
            // where a literal keys it (W5). Only the member's declared type is keyed.
            let overloads (declared: bool) (memberFacts: TypeFacts) =
                let keyed = declared && isLiteralKeyed model m memberFacts

                memberFacts.CallSignatures
                |> List.collect (restOverloads model)
                |> List.map (fun signature ->
                    match if keyed then literalKeys model signature else [] with
                    | [] ->
                        let typeParameters, parameters, returns, signatureFindings =
                            shapeSignature ctx model (Some self) owner signature

                        findings <- findings @ signatureFindings

                        FsMethod
                            {
                                Name = Naming.memberName m.Symbol.Name
                                Docs = m.Docs
                                Tags = signatureTags m.Tags signature
                                TypeParameters = typeParameters
                                Parameters = parameters
                                Return = returns
                            }
                    | literals -> literalKeyedMethod m signature literals)

            match methodFacts, recoverableCallbackFacts with
            | Some memberFacts, _ -> overloads true memberFacts
            | None, Some memberFacts ->
                if agreed.IsSome then
                    emit (Finding.make owner TypeReference.IntersectionOperandsIdentical)

                overloads agreed.IsNone memberFacts
            | None, None ->
                let reference, refFindings =
                    typeRef ctx (selectAnyReturn ctx model m.Symbol.Name owner) (Some self) owner effectiveTypeId

                let literal =
                    literalMemberValue model m effectiveTypeId
                    |> Option.filter (fun _ -> reference = FsFloat || reference = FsString)

                match literal with
                | Some value ->
                    let widening (finding: Finding) =
                        match finding.Kind with
                        | :? TypeReference as kind ->
                            kind = TypeReference.NumericLiteralToFloat
                            || kind = TypeReference.StringLiteralToString
                        | _ -> false

                    findings <- findings @ List.filter (widening >> not) refFindings
                    emit (Finding.make owner (Members.LiteralMemberAsConstant(typeScriptLiteral value)))
                | None -> findings <- findings @ refFindings

                if agreed.IsSome then
                    emit (Finding.make owner TypeReference.IntersectionOperandsIdentical)

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
                            Literal = literal
                        }
                ])

    // A well-known symbol's member is spelled from the symbol: `m.Symbol.Name` carries the
    // session-specific checker id. `settle-symbol-members` raises its findings.
    let symbolMembers =
        facts.Members
        |> List.collect (fun m ->
            match emittedSymbol facts m with
            | None -> []
            | Some symbol ->
                let name = symbolMemberName symbol
                let owner = $"{self}.[Symbol.{symbol}]"

                let signatures =
                    if hasAny SymbolFlags.Method m.Symbol.Flags then
                        match Map.tryFind m.TypeId model.Types with
                        | Some memberFacts -> memberFacts.CallSignatures
                        | None -> []
                    else
                        []

                if signatures.IsEmpty then
                    let reference, refFindings = typeRef ctx model (Some self) owner m.TypeId
                    findings <- findings @ refFindings

                    [
                        FsSymbolKeyed(
                            symbol,
                            FsProperty
                                {
                                    Name = name
                                    Docs = m.Docs
                                    Tags = m.Tags
                                    ReadOnly = m.ReadOnly
                                    Type = reference
                                    Literal = None
                                }
                        )
                    ]
                else
                    signatures
                    |> List.collect (restOverloads model)
                    |> List.map (fun signature ->
                        let typeParameters, parameters, returns, signatureFindings =
                            shapeSignature ctx model (Some self) owner signature

                        findings <- findings @ signatureFindings

                        FsSymbolKeyed(
                            symbol,
                            FsMethod
                                {
                                    Name = name
                                    Docs = m.Docs
                                    Tags = signatureTags m.Tags signature
                                    TypeParameters = typeParameters
                                    Parameters = parameters
                                    Return = returns
                                }
                        )))

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
        |> List.collect (restOverloads model)
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

    members @ symbolMembers @ indexers @ constructors, findings

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

/// The DU case names of a discriminated union's cases, in case order: the tag value, or, for a
/// named arm sharing its tag value with another arm, the arm's own name.
let internal taggedCaseNamesOf (tagged: TaggedArm list) =
    let shared =
        tagged
        |> List.countBy _.Value
        |> List.filter (snd >> (<) 1)
        |> List.map fst
        |> Set.ofList

    tagged
    |> List.map (fun arm ->
        match arm.Facts.SymbolName with
        | Some name when
            Set.contains arm.Value shared
            && not (isAnonymousArm arm.Facts)
            && not (isSyntheticName name)
            ->
            Naming.enumCaseOfString (string name)
        | _ -> Naming.enumCaseOfString arm.Value)
    |> uniqueCaseNames

/// The case each arm of a union reads under once `detect-tagged-unions` declares it, keyed by
/// the arm's type id. Empty for a union `detect-tagged-unions` does not declare.
let internal taggedCaseNames (model: ShapeModel) (facts: TypeFacts) : Map<int<Measure.typeId>, string> =
    if not (isTaggedDeclaration model facts) then
        Map.empty
    else
        match taggedUnionShape model facts with
        | Discriminated(_, tagged, _) ->
            List.zip (tagged |> List.map _.Facts.Response.TypeId) (taggedCaseNamesOf tagged)
            |> Map.ofList
        | TagCollides _
        | Untagged -> Map.empty

/// Type abbreviations declared by a model, as name -> target. Shared by the passes that must
/// compare signatures the way the compiler does: .NET overload resolution sees through an
/// abbreviation, so `TargetsParam` and the `U6<...>` it abbreviates are one type to it.
let abbreviations (decls: FsDecl list) : Map<string, FsTypeRef> =
    decls
    |> List.choose (function
        | FsAbbrev decl -> Some(decl.Name, decl.Target)
        | _ -> None)
    |> Map.ofList

/// A reference with abbreviations expanded. `visited` breaks a cycle a malformed declaration
/// set could otherwise spin on; it is threaded rather than global so nested positions of one
/// reference each get their own budget.
let rec expandAbbreviations (abbrevs: Map<string, FsTypeRef>) (visited: Set<string>) (reference: FsTypeRef) =
    let recur = expandAbbreviations abbrevs visited

    match reference with
    | FsNamed name when Map.containsKey name abbrevs && not (Set.contains name visited) ->
        expandAbbreviations abbrevs (Set.add name visited) abbrevs[name]
    | FsOption inner -> FsOption(recur inner)
    | FsArray element -> FsArray(recur element)
    | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
    | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
    | other -> other

/// Signatures as .NET overload resolution compares them: abbreviations expanded at every depth and
/// a signature's own type variables renamed by position.
module CompiledSignature =
    /// The reference with every abbreviation expanded, at any depth. A cycle stops at its first
    /// repeated name.
    let rec normalize (abbrevs: Map<string, FsTypeRef>) (visited: Set<string>) (reference: FsTypeRef) : FsTypeRef =
        let recur = normalize abbrevs visited

        match reference with
        | FsNamed name when Map.containsKey name abbrevs && not (Set.contains name visited) ->
            normalize abbrevs (Set.add name visited) abbrevs[name]
        | FsOption inner -> FsOption(recur inner)
        | FsArray element -> FsArray(recur element)
        | FsTuple components -> FsTuple(List.map recur components)
        | FsErasedUnion arms -> FsErasedUnion(List.map recur arms)
        | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
        | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
        | FsApp(name, args) -> FsApp(name, List.map recur args)
        | FsBranded(primitive, measure) -> FsBranded(recur primitive, measure)
        | other -> other

    let rec private renameTypeVars (rename: Map<string, string>) (reference: FsTypeRef) : FsTypeRef =
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

    /// The reference as the compiler sees it inside a signature declaring `typeParameters`: the
    /// signature's own type variables are renamed by position, then abbreviations expand.
    let compiled (abbrevs: Map<string, FsTypeRef>) (typeParameters: FsTypeParam list) (reference: FsTypeRef) =
        let rename = typeParameters |> List.mapi (fun i p -> p.Name, $"T{i}") |> Map.ofList

        normalize abbrevs Set.empty (renameTypeVars rename reference)

    /// The compiled parameter signature: generic arity, optionality and type per position.
    /// ParamArray is call-site metadata and contributes nothing to overload identity.
    let parameterKey (abbrevs: Map<string, FsTypeRef>) (typeParameters: FsTypeParam list) (parameters: FsParam list) =
        typeParameters.Length,
        parameters
        |> List.map (fun p -> p.Optional, compiled abbrevs typeParameters p.Type)

    /// The parameter's type as written in its slot: an optional parameter renders `?name: T`
    /// for a `T option` reference.
    let private slotType (abbrevs: Map<string, FsTypeRef>) (typeParameters: FsTypeParam list) (p: FsParam) =
        match p.Type with
        | FsOption inner when p.Optional -> compiled abbrevs typeParameters inner
        | other -> compiled abbrevs typeParameters other

    /// Whether `longer` begins with `shorter`'s parameter types and the rest of `longer` may be
    /// omitted at the call. The two may be the same length, differing in optionality alone.
    let private typedPrefix
        (abbrevs: Map<string, FsTypeRef>)
        (shorter: FsTypeParam list * FsParam list)
        (longer: FsTypeParam list * FsParam list)
        =
        let shorterTypes, shorterParams = shorter
        let longerTypes, longerParams = longer

        shorterParams.Length <= longerParams.Length
        && shorterTypes.Length = longerTypes.Length
        && List.forall2
            (fun (a: FsParam) (b: FsParam) ->
                a.Rest = b.Rest
                && slotType abbrevs shorterTypes a = slotType abbrevs longerTypes b)
            shorterParams
            (List.take shorterParams.Length longerParams)
        && (longerParams
            |> List.skip shorterParams.Length
            |> List.forall (fun p -> p.Optional || p.Rest))

    /// Whether one signature's parameters are a typed prefix of the other's and some call selects
    /// either, which F# reports as FS0041: the call supplies an argument count both accept and
    /// leaves an optional parameter unsupplied in both or in neither, and likewise a `ParamArray`.
    /// `(x)` beside `(x, ?y)` resolves to `(x)` at every call; `(x, ?y)` beside `(x, ?y, ?z)` is
    /// ambiguous at the call supplying `x` alone, and `(x, ?y)` beside `(x, y)` at the call
    /// supplying both. Generic arity separates otherwise equal signatures.
    let ambiguousCall
        (abbrevs: Map<string, FsTypeRef>)
        (a: FsTypeParam list * FsParam list)
        (b: FsTypeParam list * FsParam list)
        =
        let aParams = snd a
        let bParams = snd b

        let accepts (parameters: FsParam list) supplied =
            parameters |> List.skip supplied |> List.forall (fun p -> p.Optional || p.Rest)

        let leaves (kind: FsParam -> bool) (parameters: FsParam list) supplied =
            parameters |> List.skip supplied |> List.exists kind

        (typedPrefix abbrevs a b || typedPrefix abbrevs b a)
        && [ 0 .. min aParams.Length bParams.Length ]
           |> List.exists (fun supplied ->
               accepts aParams supplied
               && accepts bParams supplied
               && leaves _.Optional aParams supplied = leaves _.Optional bParams supplied
               && leaves _.Rest aParams supplied = leaves _.Rest bParams supplied)

    /// Whether a call can select either signature: one compiled parameter signature, or an
    /// `ambiguousCall`.
    let overlaps
        (abbrevs: Map<string, FsTypeRef>)
        (a: FsTypeParam list * FsParam list)
        (b: FsTypeParam list * FsParam list)
        =
        parameterKey abbrevs (fst a) (snd a) = parameterKey abbrevs (fst b) (snd b)
        || ambiguousCall abbrevs a b
