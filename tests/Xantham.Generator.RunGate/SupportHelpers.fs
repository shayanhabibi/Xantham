module Xantham.Generator.RunGate.SupportHelpers

open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop

type private Settings =
    {
        Count: int
        Name: string
        Payload: obj
    }

[<Measure>]
type private Identifier

let run check =
    let expect claim expected evaluate =
        try
            check claim (evaluate () = expected)
        with error ->
            check $"{claim}: {error.Message}" false

    let settings =
        {
            Count = 42
            Name = "name"
            Payload = null
        }

    let typed = unbox<typekeyof<Settings, int>> "Count"
    let key = unbox<keyof<Settings>> "Count"

    expect "an erased key's Value is its string" "Count" (fun () -> typed.Value)

    expect "TypeKeyOf creates a key from the lambda" "Count" (fun () ->
        TypeKeyOf.create (fun (value: Settings) -> value.Count) |> TypeKeyOf.value)

    expect "TypeKeyOf.item reads its property" 42 (fun () -> TypeKeyOf.item typed settings)
    expect "TypeKeyOf.access reads its property" 42 (fun () -> TypeKeyOf.access typed settings)
    expect "TypeKeyOf.box erases the return type" "Count" (fun () -> TypeKeyOf.box typed |> KeyOf.value)
    expect "KeyOf.fromPropertyKey preserves the key" "Count" (fun () -> KeyOf.fromPropertyKey typed |> KeyOf.value)
    expect "KeyOf.item reads its property" (Some 42) (fun () -> KeyOf.item key settings |> Option.map unbox<int>)
    expect "KeyOf.access reads its property" (Some 42) (fun () -> KeyOf.access key settings |> Option.map unbox<int>)
    expect "KeyOf.item maps null to None" None (fun () -> KeyOf.item (unbox<keyof<Settings>> "Payload") settings)

    expect "KeyOf.item maps an absent property to None" None (fun () ->
        KeyOf.item (unbox<keyof<Settings>> "Absent") settings)

    expect "KeyOf.unsafeUnbox preserves the key" "Count" (fun () ->
        KeyOf.unsafeUnbox<Settings, int> key |> TypeKeyOf.value)

    expect "KeyOf.tryUnbox accepts the selected property" (Some "Count") (fun () ->
        KeyOf.tryUnbox (fun (value: Settings) -> value.Count) key
        |> Option.map TypeKeyOf.value)

    expect "KeyOf.tryUnbox rejects another property" None (fun () ->
        KeyOf.tryUnbox (fun (value: Settings) -> value.Name) key
        |> Option.map TypeKeyOf.value)

    expect "the keyof helper returns the property name" "Name" (fun () ->
        keyof<Settings, _>(fun value -> value.Name) |> KeyOf.value)

    expect "keyof infers both type parameters" "Count" (fun () ->
        let inferred: keyof<Settings> = keyof _.Count
        KeyOf.value inferred)

    expect "the typekeyof helper retains the return type" 42 (fun () ->
        typekeyof<Settings, int>(fun value -> value.Count)
        |> fun field -> TypeKeyOf.item field settings)

    expect "KeyIs matches the key" true (fun () ->
        match key with
        | KeyIs "Count" -> true
        | _ -> false)

    expect "KeyIs rejects another key" false (fun () ->
        match key with
        | KeyIs "Name" -> true
        | _ -> false)

    expect "TypeKeyIs matches the typed key" true (fun () ->
        match typed with
        | TypeKeyIs "Count" -> true
        | _ -> false)

    expect "TypeKeyIs rejects another typed key" false (fun () ->
        match typed with
        | TypeKeyIs "Name" -> true
        | _ -> false)

    expect "Brand string helpers preserve the string" "identifier" (fun () ->
        Brand.tagString<Identifier> "identifier" |> Brand.untagString)

    expect "Brand bool helpers preserve false" false (fun () -> Brand.tagBool<Identifier> false |> Brand.untagBool)
    expect "Brand char helpers preserve the char" 'x' (fun () -> Brand.tagChar<Identifier> 'x' |> Brand.untagChar)

    expect "PropTypeBuilder preserves its property lambda" "name" (fun () ->
        let witness = PropTypeBuilder.proptypekey (fun (value: Settings) -> value.Name)
        emitJsExpr (witness, settings) "$0($1)")

    expect "KeyOf.item preserves an option stored as an object" true (fun () ->
        let payload = box (Some(null: obj))
        let value = { settings with Payload = payload }

        KeyOf.item (unbox<keyof<Settings>> "Payload") value
        |> Option.exists (fun actual -> obj.ReferenceEquals(payload, actual)))

    expect "KeyOf.item evaluates a property getter once" 1 (fun () ->
        let value: obj =
            emitJsExpr () "({ reads: 0, get value() { this.reads++; return 42; } })"

        KeyOf.item (unbox<keyof<obj>> "value") value |> ignore
        emitJsExpr value "$0.reads")
