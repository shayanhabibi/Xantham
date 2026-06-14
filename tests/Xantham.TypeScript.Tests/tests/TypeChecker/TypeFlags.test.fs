module TypeFlags
open System
open Xantham.TypeScript.Types.Symbol
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "TF · Type Flags" <| fun _ ->
    // ----------------------------------------------------------------------------------------------
    //                                  TF - TYPE FLAGS
    // ----------------------------------------------------------------------------------------------
    runner.testCase "TF-1 · Union+Boolean types contain both true and false literals" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Union)
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Boolean)
        |> Array.iter (fun typ ->
            typ :?> Ts.UnionType
            |> _.types.AsArray
            |> Flip.Expect.containsAll [ ctx.Checker.getTrueType(); ctx.Checker.getFalseType() ] ""
            )
    runner.testCase "TF-2 · Union+Boolean types have exactly 2 union members" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Union)
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Boolean)
        |> Array.iter (fun typ ->
            typ :?> Ts.UnionType
            |> _.types.AsArray
            |> Flip.Expect.hasLength 2 "Do not expect any less or more than 2 types if both union and boolean flags are set"
            )
    runner.testCase "TF-3 · Literal flag can occur without the EnumLiteral flag" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Literal)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.EnumLiteral) >> not)
        |> Flip.Expect.isNonEmpty ""
    let flags = [
        Ts.TypeFlags.Boolean, "Boolean"
        Ts.TypeFlags.Enum, "Enum"
        Ts.TypeFlags.BigInt, "BigInt"
        Ts.TypeFlags.StringLiteral, "StringLiteral"
        Ts.TypeFlags.NumberLiteral, "NumberLiteral"
        Ts.TypeFlags.BooleanLiteral, "BooleanLiteral"
        Ts.TypeFlags.EnumLiteral, "EnumLiteral"
        Ts.TypeFlags.BigIntLiteral, "BigIntLiteral"
        Ts.TypeFlags.ESSymbol, "ESSymbol"
        Ts.TypeFlags.UniqueESSymbol, "UniqueESSymbol"
        Ts.TypeFlags.Void, "Void"
        Ts.TypeFlags.Undefined, "Undefined"
        Ts.TypeFlags.Null, "Null"
        Ts.TypeFlags.Never, "Never"
        Ts.TypeFlags.TypeParameter, "TypeParameter"
        Ts.TypeFlags.Object, "Object"
        Ts.TypeFlags.Union, "Union"
        Ts.TypeFlags.Intersection, "Intersection"
        Ts.TypeFlags.Index, "Index"
        Ts.TypeFlags.IndexedAccess, "IndexedAccess"
        Ts.TypeFlags.Conditional, "Conditional"
        Ts.TypeFlags.Substitution, "Substitution"
        Ts.TypeFlags.NonPrimitive, "NonPrimitive"
        Ts.TypeFlags.TemplateLiteral, "TemplateLiteral"
        Ts.TypeFlags.StringMapping, "StringMapping"
        Ts.TypeFlags.Literal, "Literal"
        Ts.TypeFlags.Unit, "Unit"
        Ts.TypeFlags.Freshable, "Freshable"
        Ts.TypeFlags.StringOrNumberLiteral, "StringOrNumberLiteral"
        Ts.TypeFlags.PossiblyFalsy, "PossiblyFalsy"
        Ts.TypeFlags.StringLike, "StringLike"
        Ts.TypeFlags.NumberLike, "NumberLike"
        Ts.TypeFlags.BigIntLike, "BigIntLike"
        Ts.TypeFlags.BooleanLike, "BooleanLike"
        Ts.TypeFlags.EnumLike, "EnumLike"
        Ts.TypeFlags.ESSymbolLike, "ESSymbolLike"
        Ts.TypeFlags.VoidLike, "VoidLike"
        Ts.TypeFlags.UnionOrIntersection, "UnionOrIntersection"
        Ts.TypeFlags.StructuredType, "StructuredType"
        Ts.TypeFlags.TypeVariable, "TypeVariable"
        Ts.TypeFlags.InstantiableNonPrimitive, "InstantiableNonPrimitive"
        Ts.TypeFlags.InstantiablePrimitive, "InstantiablePrimitive"
        Ts.TypeFlags.Instantiable, "Instantiable"
        Ts.TypeFlags.StructuredOrInstantiable, "StructuredOrInstantiable"
        Ts.TypeFlags.Narrowable, "Narrowable"
    ]
    let flagTracker = flags |> List.map (fun (flag, _) -> KeyValuePair(flag, enum<Ts.TypeFlags> 0)) |> Dictionary
    let registerFlags (input: Ts.TypeFlags) =
        for flag, _ in flags do
            if input.HasFlag(flag) then
                flagTracker[flag] <- flagTracker[flag] ||| input
    let getName flag = flags |> List.find (fst >> (=) flag) |> snd
    runner.testCase "TF-4 · TypeFlags exclusive/inclusive map holds over the corpus" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.iter (_.flags >> registerFlags)
        let flagMap = Map [
                for kv in flagTracker do
                    let flagName = getName kv.Key
                    kv.Key,
                    flags
                    |> List.filter (fst >> kv.Value.HasFlag >> not)
                    |> List.map (snd >> sprintf "    Ts.TypeFlags.%s")
                    |> String.concat "\n"
                    |> sprintf "Ts.TypeFlags.%s, [\n%s\n]" flagName
        ]
        ctx.Types.Value
        |> Array.iter (fun typ ->
            let typeFlags = typ.flags
            flags
            |> List.filter (fst >> typeFlags.HasFlag)
            |> List.filter (fst >> Map.find >> funApply Spec.TypeFlags.exclusiveMasks >> (&&&) typeFlags >> (<>) (enum 0))
            |> List.map (fst >> fun key -> flagMap[key])
            |> fun incorrectMaskMaps ->
                let typeFlags =
                    flags
                    |> List.filter (fst >> typeFlags.HasFlag)
                    |> List.map snd
                incorrectMaskMaps
                |> Flip.Expect.isEmpty (String.concat "\n" incorrectMaskMaps |> sprintf "%A got a different exclusive typemap:\n%s" typeFlags)
            )
    runner.testCase "TF-5 · TypeFlags are mutually exclusive" <| fun _ ctx ->
        let exclusiveFlags = List.distinct [
            Ts.TypeFlags.Any, "Any"
            Ts.TypeFlags.Unknown, "Unknown"
            Ts.TypeFlags.Undefined, "Undefined"
            Ts.TypeFlags.UniqueESSymbol, "UniqueESSymbol"
            Ts.TypeFlags.Boolean, "Boolean"
            Ts.TypeFlags.String, "String"
            Ts.TypeFlags.Number, "Number"
            Ts.TypeFlags.Null, "Null"
            Ts.TypeFlags.Never, "Never"
            Ts.TypeFlags.Object, "Object"
            Ts.TypeFlags.NonPrimitive, "NonPrimitive"
            Ts.TypeFlags.ESSymbol, "ESSymbol"
            Ts.TypeFlags.BigInt, "BigInt"
            Ts.TypeFlags.Void, "Void"
            Ts.TypeFlags.BooleanLiteral, "BooleanLiteral"
            Ts.TypeFlags.BigIntLiteral, "BigIntLiteral"
            Ts.TypeFlags.NumberLiteral, "NumberLiteral"
            Ts.TypeFlags.StringLiteral, "StringLiteral"
            Ts.TypeFlags.Enum, "Enum"
            Ts.TypeFlags.TypeParameter, "TypeParameter"
            Ts.TypeFlags.Intersection, "Intersection"
            Ts.TypeFlags.Index, "Index"
            Ts.TypeFlags.IndexedAccess, "IndexedAccess"
            Ts.TypeFlags.Conditional, "Conditional"
            Ts.TypeFlags.Substitution, "Substitution"
            Ts.TypeFlags.TemplateLiteral, "TemplateLiteral"
            Ts.TypeFlags.StringMapping, "StringMapping"
        ]
        ctx.Types.Value
        |> Array.iter (fun typ ->
            exclusiveFlags
            |> List.filter (fst >> typ.flags.HasFlag)
            |> function
                | [] | [ _ ] -> ()
                | l -> failtest $"Expected no conflicting exclusive flags for TypeFlags, but got %A{l |> List.map snd}"
            )
    runner.testCase "TF-6 - StringMapping always resolve to a symbol of one of the four intrinsics" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.StringMapping)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.map (_.unsafeGetCanonicalSymbol() >> _.symbolName)
            >> Flip.Expect.all (function
                | SymbolName.String "Lowercase"
                | SymbolName.String "Uppercase"
                | SymbolName.String "Capitalize"
                | SymbolName.String "Uncapitalize" -> true
                | name -> failtest $"Expected StringMapping to resolve to one of the four intrinsics. Instead got: {name}"
                ) ""
            )
    runner.testCase "TF-7 - StringMapping always resolve to a symbol with one declaration" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.StringMapping)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.map (_.unsafeGetCanonicalSymbol() >> _.declarations >> Option.bind NonEmptyArray.create)
            >> Flip.Expect.all (
                Flip.Expect.wantSome ""
                >> _.Length.Equals(1)
                ) ""
            )
    runner.testCase "TF-8 - TemplateLiteral never resolves a symbol" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.TemplateLiteral)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun typ ->
                typ.getCanonicalSymbol()
                |> Flip.Expect.isNone $"TemplateLiteral should never resolve to a symbol. Flags: {typ.flags.ToStringArray()}. Type: {ctx.Checker.typeToString typ}"
                )
            )
    runner.testCase "TF-9 - TemplateLiteral always has at least one type and text argument" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.TemplateLiteral)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (unbox<Ts.TemplateLiteralType> >> fun typ ->
                typ.types.AsArray
                |> Flip.Expect.isNonEmpty $"TemplateLiteral should always have at least one type argument. Flags: {typ.flags.ToStringArray()}. Type: {ctx.Checker.typeToString typ}"
                typ.texts.AsArray
                |> Flip.Expect.isNonEmpty $"TemplateLiteral should always have at least one text argument. Flags: {typ.flags.ToStringArray()}. Type: {ctx.Checker.typeToString typ}"
                )
            )
    runner.testCase "TF-10 - TemplateLiteral always has one more text than type argument." <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.TemplateLiteral)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (unbox<Ts.TemplateLiteralType> >> fun typ ->
                typ.texts.AsArray
                |> Flip.Expect.hasLength (typ.types.AsArray.Length + 1) $"TemplateLiteral should always have one more text than type argument. Flags: {typ.flags.ToStringArray()}. Type: {ctx.Checker.typeToString typ}"
                )
            )
        
    runner.testCase "TF-11 - TemplateLiteral text arguments are always strings, and never null (but can be empty)" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.TemplateLiteral)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (unbox<Ts.TemplateLiteralType> >> fun typ ->
                typ.texts.AsArray
                |> Array.iter (fun text ->
                    text
                    |> Flip.Expect.isNotNull $"TemplateLiteral text argument should always be a string. Flags: {typ.flags.ToStringArray()}. Type: {ctx.Checker.typeToString typ}"
                    )
                )
            )
    runner.testCase "TF-12 - SubstitutionTypes are true repr of instantiable conditionals, or are NoInfer whereby constraint is unknown" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Substitution)
        |> Array.filter (fun typ ->
            typ :?> Ts.SubstitutionType
            |> _.``constraint``
            |> _.flags.HasFlag(Ts.TypeFlags.Unknown)
            )
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (fun typ ->
            typ :?> Ts.SubstitutionType
            |> ctx.Checker.typeToString
            |> Flip.Expect.stringContains "NoInfer" "SubstitutionTypes are true repr of instantiable conditionals, or are NoInfer whereby constraint is unknown"
            ))
    runner.testCase "TF-12.1 - SubstitutionTypes never resolve to a symbol" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Substitution)
        |> unbox<Ts.SubstitutionType array>
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (fun typ ->
                typ.getCanonicalSymbol()
                |> Flip.Expect.isNone "SubstitutionTypes should never resolve to a symbol"
            ))
    runner.testCase "TF-13 - ConditionalTypes have resolved types lazily computed to some value if possible" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ
            |> _.IsSome
            )
        |> Array.filter _.resolvedTrueType.IsSome
        |> Flip.Expect.skipIfEmpty
    runner.testCase "TF-13.1 - ConditionalTypes never resolve a symbol" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (fun typ ->
            typ.getCanonicalSymbol()
            |> Flip.Expect.isNone "ConditionalTypes should never resolve to a symbol"))
        
    runner.testCase "TF-14 - ConditionalTypes that return a value on constraint check will have their trueType value resolved" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ
            |> _.IsSome
            )
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (_.resolvedTrueType >> Flip.Expect.isSome ""))
    runner.testCase "TF-15 - ConditionalType roots always have outerTypeParameters" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (_.root.outerTypeParameters >> Chain.Expect.wantSome "" >> Flip.Expect.isNonEmpty ""))
    runner.testCase "TF-16 - ConditionalTypes that return none on constraint check might still have resolved true field filled" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ
            |> _.IsNone
            && typ.resolvedTrueType.IsSome
            )
        |> Flip.Expect.skipIfEmpty
    runner.testCase "TF-17 - ConditionalTypes always have lazy true/false type filled after constraint check" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.iter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ |> ignore
            typ.resolvedTrueType |> Flip.Expect.isSome ""
            typ.resolvedFalseType |> Flip.Expect.isSome ""
            )
    runner.testCase "TF-18 - ConditionalTypes can have SubstitutionTypes in their true field" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ |> ignore
            typ.resolvedTrueType
            |> Chain.Expect.wantSome ""
            |> _.flags.HasFlag(Ts.TypeFlags.Substitution)
            )
        |> Flip.Expect.skipIfEmpty
    runner.testCase "TF-19 - ConditionalTypes always don't always have SubstitutionTypes in their true field" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ |> ignore
            typ.resolvedTrueType
            |> Chain.Expect.wantSome ""
            |> _.flags.HasFlag(Ts.TypeFlags.Substitution)
            |> not
            )
        |> Flip.Expect.skipIfEmpty
    runner.testCase "TF-20 - ConditionalTypes can have SubstitutionTypes in their false field (rare)" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Conditional)
        |> unbox<Ts.ConditionalType array>
        |> Array.filter (fun typ ->
            ctx.Checker.getBaseConstraintOfType typ |> ignore
            typ.resolvedFalseType
            |> Chain.Expect.wantSome ""
            |> _.flags.HasFlag(Ts.TypeFlags.Substitution)
            )
        // |> Array.map (ctx.Checker.typeToString >> printfn "Type: %s")
        |> Flip.Expect.skipIfEmpty

    
    
        
        
        
