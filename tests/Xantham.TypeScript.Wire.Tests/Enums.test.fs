/// Checks the generated enums against themselves.
///
/// `Enums.generated.fs` states each composite twice: once as the value the generator's own
/// evaluator computed, in the doc comment, and once as an F# expression over the cases. Those two
/// are computed by different languages with different precedence rules - F# puts `|||` and `&&&`
/// at the same level and associates left, where TypeScript binds `&` tighter - so agreeing is a
/// real result and not a tautology. The generator parenthesises everything for exactly this
/// reason; this is the assertion that it worked.
module Xantham.TypeScript.Wire.Tests.Enums

open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Expecto
open Xantham.TypeScript.Wire

let private generated =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Xantham.TypeScript.Wire", "Enums.generated.fs")

/// `module SymbolFlags =` compiles to a `SymbolFlagsModule` type when it shares its name with the
/// enum, and to a plain one when it does not - `InternalSymbolName` is a module of strings.
let private companion (assembly: Assembly) name =
    [ $"Xantham.TypeScript.Wire.{name}Module"; $"Xantham.TypeScript.Wire.{name}" ]
    |> List.tryPick (assembly.GetType >> Option.ofObj)

/// Every literal in the file, as (enum, member, the value the generator recorded for it).
let private recorded () =
    let mutable enumName = ""
    let mutable expected = 0u

    [ for line in File.ReadLines generated do
        let declaration = Regex.Match(line, @"^module (\w+) =$")
        if declaration.Success then enumName <- declaration.Groups[1].Value

        // The `<returns>` of the doc line above the literal carries the value the generator
        // evaluated; the `let` below it carries the expression F# will evaluate for itself.
        let doc = Regex.Match(line, @"^    /// <summary>.*</summary><returns><c>(\d+)u</c></returns>$")
        if doc.Success then expected <- uint32 doc.Groups[1].Value

        let literal = Regex.Match(line, @"^    let (\w+) = ")
        if literal.Success then yield enumName, literal.Groups[1].Value, expected ]

[<Tests>]
let enumTests =
    testList "generated enums" [
        testCase "every composite literal evaluates to the value the generator computed" <| fun _ ->
            let assembly = typeof<SymbolFlags>.Assembly

            let disagreements =
                [ for enumName, name, expected in recorded () do
                    match companion assembly enumName with
                    | None -> failtestf "no companion module emitted for %s" enumName
                    | Some ty ->
                        match ty.GetField(name, BindingFlags.Public ||| BindingFlags.Static) with
                        | null -> failtestf "%s.%s is in the source but not in the assembly" enumName name
                        | field ->
                            // `InternalSymbolName` is a module of strings, which have no arithmetic
                            // to disagree about.
                            match field.GetValue null with
                            | :? string -> ()
                            | value ->
                                let actual = System.Convert.ToUInt32 value
                                if actual <> expected then
                                    yield $"{enumName}.{name}: F# says {actual}, the generator said {expected}" ]

            disagreements |> Flip.Expect.isEmpty "literals whose two spellings disagree"

        // A guard on the guard: if the parse above stopped matching the emitted shape it would
        // pass vacuously, and the count is the cheapest thing that would notice.
        testCase "the file is still shaped the way the check reads it" <| fun _ ->
            recorded ()
            |> List.length
            |> fun found -> Flip.Expect.isTrue $"only {found} literals found in Enums.generated.fs" (found > 100)

        // The split is invisible to a caller: a case and a literal answer to the same prefix, and
        // a literal is still usable where a constant is required.
        testCase "cases and composites share one prefix" <| fun _ ->
            SymbolFlags.Value
            |> Flip.Expect.equal "Value is Variable and the rest of it"
                (SymbolFlags.Variable ||| SymbolFlags.Property ||| SymbolFlags.EnumMember
                 ||| SymbolFlags.ObjectLiteral ||| SymbolFlags.Function ||| SymbolFlags.Class
                 ||| SymbolFlags.Enum ||| SymbolFlags.ValueModule ||| SymbolFlags.Method
                 ||| SymbolFlags.GetAccessor ||| SymbolFlags.SetAccessor)

            match SymbolFlags.Accessor with
            | SymbolFlags.Accessor -> ()
            | other -> failtestf "a literal did not match itself as a pattern: %A" other
    ]
