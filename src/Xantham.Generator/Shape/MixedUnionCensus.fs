/// Measurement pass for `docs/.ai/plans/2026-09-22-mixed-literal-unions.md` Step 0: appends one
/// JSON row per union type mixing literal and typed arms, with its eligibility for an erased-DU
/// mapping. Inert unless `XANTHAM_MIXED_UNION_CENSUS` is set to the file to append to.
/// Arms are classified from raw `TypeFacts`, so string literals count as literals rather than as
/// `string`; the plan's "Step 0 result" section records the rationale.
module Xantham.Generator.Shape.MixedUnionCensus

open System
open System.IO
open System.Text.Json
open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// The runtime test Fable would emit for an arm, at the granularity that decides collisions.
/// `Fable2Babel` sends arrays and tuples through one `isArrayLike`, and every function shape
/// through one `typeof x === "function"`, so those fold here too.
type private TestClass =
    | Str
    | Num
    | Bool
    | BigIntLike
    | ArrayLike
    | FuncLike
    /// Class-shaped, so `Render.fs` gives it an `[<Import>]` binding and `instanceof` works.
    | ClassShaped
    /// Reached `warnAndEvalToFalse`: the branch compiles and is silently never taken.
    | NotDiscriminable
    /// `U_n`, `obj`, a type parameter: no test at all.
    | Opaque

let private has (flags: TypeFlags) (flag: TypeFlags) = uint32 (flags &&& flag) <> 0u

/// Arms that only carry the option layer and are stripped before any of this matters.
let private isNullish (facts: TypeFacts) =
    let flags = facts.Response.Flags

    has flags TypeFlags.Undefined
    || has flags TypeFlags.Null
    || has flags TypeFlags.Void

let private classify (model: ShapeModel) (facts: TypeFacts) : TestClass =
    let flags = facts.Response.Flags
    let name = facts.SymbolName |> Option.map string |> Option.defaultValue ""

    if has flags TypeFlags.String then
        Str
    elif has flags TypeFlags.Number || has flags TypeFlags.Enum then
        Num
    elif has flags TypeFlags.Boolean then
        Bool
    elif has flags TypeFlags.BigInt then
        BigIntLike
    elif has flags TypeFlags.Union then
        Opaque
    elif
        has flags TypeFlags.TypeParameter
        || has flags TypeFlags.Any
        || has flags TypeFlags.Unknown
    then
        Opaque
    elif has flags TypeFlags.Object then
        if facts.Response.IsTupleType = ValueSome true then
            ArrayLike
        elif name = "Array" || name = "ReadonlyArray" then
            ArrayLike
        elif not facts.CallSignatures.IsEmpty && facts.Members.IsEmpty then
            FuncLike
        elif facts.AmbientClass.IsSome || not facts.ConstructSignatures.IsEmpty then
            ClassShaped
        else
            NotDiscriminable
    else
        Opaque

/// One union's census row, in the vocabulary the plan's conditions are written in.
let private census (model: ShapeModel) (typeId: int<Measure.typeId>) (facts: TypeFacts) =
    let arms =
        facts.UnionMembers
        |> List.choose (fun id -> Map.tryFind id model.Types)
        |> List.filter (isNullish >> not)

    // TypeScript flattens `boolean` into `true | false`, so a `string | boolean` union arrives
    // carrying two boolean literals that are not literals in this feature's sense at all. Fold
    // the pair back into the one typed arm it stands for before anything is counted.
    let boolLiterals =
        arms |> List.filter (fun arm -> has arm.Response.Flags TypeFlags.BooleanLiteral)

    let foldedBool = boolLiterals.Length = 2

    let arms =
        if foldedBool then
            arms |> List.except boolLiterals
        else
            arms

    let literals =
        arms
        |> List.choose (fun arm -> Spec.literalOf arm |> Option.map (fun l -> arm, l))

    let nonLiterals = arms |> List.filter (fun arm -> (Spec.literalOf arm).IsNone)

    // Mixed means both halves are non-empty; anything else is `classify-literal-unions`' job
    // or an ordinary union and not this feature's business.
    if literals.IsEmpty || (nonLiterals.IsEmpty && not foldedBool) then
        None
    else
        let stringLiterals =
            literals
            |> List.filter (fun (_, literal) ->
                match literal with
                | LitString _ -> true
                | _ -> false)

        let classes =
            (nonLiterals |> List.map (classify model))
            @ (if foldedBool then [ Bool ] else [])

        let shared = classes |> List.countBy id |> List.filter (fun (_, count) -> count > 1)

        let disqualifying =
            classes
            |> List.filter (fun c -> c = Str || c = Opaque || c = NotDiscriminable)
            |> List.distinct

        let rejection =
            if stringLiterals.Length < literals.Length then
                Some "non-string literals in the mix"
            elif disqualifying |> List.contains Str then
                Some "bare string arm beside the literals"
            elif disqualifying |> List.contains Opaque then
                Some "an opaque arm"
            elif disqualifying |> List.contains NotDiscriminable then
                Some "an arm Fable cannot type test"
            elif not shared.IsEmpty then
                Some "two arms share one test class"
            else
                None

        Some(
            {|
                TypeId = int typeId
                Name = facts.SymbolName |> Option.map string |> Option.defaultValue ""
                Literals = literals.Length
                LiteralTexts =
                    literals
                    |> List.map (fun (_, literal) ->
                        match literal with
                        | LitString text -> text
                        | other -> sprintf "%A" other)
                    |> List.sort
                StringLiterals = stringLiterals.Length
                Arms =
                    nonLiterals
                    |> List.map (fun arm -> arm.SymbolName |> Option.map string |> Option.defaultValue "")
                Classes = classes |> List.map (sprintf "%A")
                Rejection = rejection |> Option.defaultValue ""
                Eligible = rejection.IsNone
            |}
        )

let countMixedUnions: Pass<ShapeModel> =
    {
        Name = "count-mixed-unions"
        Run =
            fun ctx model ->
                async {
                    match Environment.GetEnvironmentVariable "XANTHAM_MIXED_UNION_CENSUS" with
                    | null
                    | "" -> return Advanced model
                    | path ->
                        let rows =
                            model.Types
                            |> Map.toList
                            |> List.choose (fun (typeId, facts) ->
                                if has facts.Response.Flags TypeFlags.Union then
                                    census model typeId facts
                                else
                                    None)

                        let lines =
                            rows
                            |> List.map (fun row ->
                                JsonSerializer.Serialize
                                    {|
                                        Package = string ctx.PackageName
                                        Union = row
                                    |})

                        lock typeof<TestClass> (fun () -> File.AppendAllLines(path, lines))
                        return Advanced model
                }
    }
