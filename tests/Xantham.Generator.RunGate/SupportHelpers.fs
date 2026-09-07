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

// Disambiguate type members from the same-spelled auto-open helper functions.
type private TypedKey = Fable.Core.JS.JS.typekeyof<Settings, int>
type private Key = Fable.Core.JS.JS.keyof<Settings>
type private ObjectKey = Fable.Core.JS.JS.keyof<obj>
type private ObjectTypedKey = Fable.Core.JS.JS.typekeyof<obj, obj>

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

    expect "typekeyof.UnsafeCastFrom preserves the key" "Count" (fun () ->
        TypedKey.UnsafeCastFrom<Settings>(unbox<typekeyof<obj, int>> "Count").Value)

    expect "typekeyof.Access reads its property" 42 (fun () -> TypedKey.Access(settings, typed))
    expect "typekeyof.UnsafeAccess reads its property" 42 (fun () -> TypedKey.UnsafeAccess(box settings, typed))
    expect "typekeyof.Invoke reads its property" 42 (fun () -> typed.Invoke(settings))

    expect "keyof.UnsafeCastFrom preserves the key" "Count" (fun () ->
        Key.UnsafeCastFrom<Settings>(unbox<keyof<obj>> "Count").Value)

    expect "keyof.UnsafeCastReturnType preserves the key" "Count" (fun () -> Key.UnsafeCastReturnType<int>(key).Value)

    expect "keyof.op_Implicit preserves the key" "Count" (fun () -> (Key.op_Implicit typed).Value)

    let optionalMembers: (string * (keyof<obj> -> obj -> obj option)) list =
        [
            "keyof.Access", fun field value -> ObjectKey.Access(value, field)
            "keyof.UnsafeAccess", fun field value -> ObjectKey.UnsafeAccess(value, field)
            "keyof.Invoke", fun field value -> field.Invoke(value)
        ]

    let typedMembers: (string * (typekeyof<obj, obj> -> obj -> obj)) list =
        [
            "typekeyof.Access", fun field value -> ObjectTypedKey.Access(value, field)
            "typekeyof.UnsafeAccess", fun field value -> ObjectTypedKey.UnsafeAccess(value, field)
            "typekeyof.Invoke", fun field value -> field.Invoke(value)
        ]

    let undefined: obj = emitJsExpr () "undefined"

    let payloads =
        [
            "zero", box 0
            "false", box false
            "empty string", box ""
            "Some null", box (Some(null: obj))
            "Some undefined", box (Some undefined)
            "nested option", box (Some(Some(null: obj)))
        ]

    let objectWith payload : obj = emitJsExpr payload "({ value: $0 })"

    let sameValue left right : bool =
        emitJsExpr (left, right) "Object.is($0, $1)"

    let untypedField = unbox<keyof<obj>> "value"
    let typedField = unbox<typekeyof<obj, obj>> "value"

    for name, read in optionalMembers do
        for label, payload in [ "null", null; "undefined", undefined ] do
            expect $"{name} maps {label} to None" None (fun () -> read untypedField (objectWith payload))

        expect $"{name} maps an absent property to None" None (fun () -> read untypedField (emitJsExpr () "({})"))

        for label, payload in payloads do
            expect $"{name} preserves {label} inside Some" true (fun () ->
                read untypedField (objectWith payload)
                |> Option.exists (fun actual -> sameValue actual payload))

        expect $"{name} evaluates a property getter once" 1 (fun () ->
            let value: obj =
                emitJsExpr () "({ reads: 0, get value() { this.reads++; return 42; } })"

            read untypedField value |> ignore
            emitJsExpr value "$0.reads")

    for name, read in typedMembers do
        for label, payload in [ "null", null; "undefined", undefined ] @ payloads do
            expect $"{name} preserves the raw {label} value" true (fun () ->
                sameValue (read typedField (objectWith payload)) payload)

        expect $"{name} leaves an absent property undefined" true (fun () ->
            sameValue (read typedField (emitJsExpr () "({})")) undefined)

        expect $"{name} evaluates a property getter once" 1 (fun () ->
            let value: obj =
                emitJsExpr () "({ reads: 0, get value() { this.reads++; return 42; } })"

            read typedField value |> ignore
            emitJsExpr value "$0.reads")

    let witness = PropTypeBuilder.proptypekey (fun (value: Settings) -> value.Count)
    let locked = unbox<proptypelock<Settings>> 42
    expect "proptypekey.lock retains the value" 42 (fun () -> witness.lock (42) |> unbox<int>)
    expect "proptypekey.unlock retains the value" 42 (fun () -> witness.unlock (locked))
    expect "proptypelock.Item retains the value" 42 (fun () -> locked.Item(witness))

    expect "property locks preserve a nested option payload" true (fun () ->
        let objectWitness =
            PropTypeBuilder.proptypekey (fun (value: Settings) -> value.Payload)

        let payload = box (Some(Some undefined))
        let wrapped = objectWitness.lock (payload)

        sameValue (objectWitness.unlock (wrapped)) payload
        && sameValue (wrapped.Item(objectWitness)) payload)

    expect "a multiple-property marker can lock and unlock its union value" 42 (fun () ->
        let marker =
            PropTypeBuilder.proptypekey ((fun (value: Settings) -> value.Name), (fun value -> value.Count))

        match marker.unlock (marker.lock (U2.Case2 42)) with
        | U2.Case2 value -> value
        | _ -> -1)

    expect "typekeyof.Invoke evaluates receiver before argument" "key,object" (fun () ->
        let calls = ResizeArray<string>()

        let getKey () =
            calls.Add "key"
            typed

        let getObject () =
            calls.Add "object"
            settings

        (getKey ()).Invoke(getObject ()) |> ignore
        String.concat "," calls)

    expect "keyof.Invoke evaluates receiver before argument" "key,object" (fun () ->
        let calls = ResizeArray<string>()

        let getKey () =
            calls.Add "key"
            key

        let getObject () =
            calls.Add "object"
            settings

        (getKey ()).Invoke(getObject ()) |> ignore
        String.concat "," calls)

    for name, read in optionalMembers do
        expect $"{name} evaluates the object and key once" 2 (fun () ->
            let mutable calls = 0

            let getKey () =
                calls <- calls + 1
                untypedField

            let getObject () =
                calls <- calls + 1
                objectWith (box 42)

            read (getKey ()) (getObject ()) |> ignore
            calls)

    expect "proptypekey.lock evaluates receiver before argument" "key,value" (fun () ->
        let calls = ResizeArray<string>()

        let getKey () =
            calls.Add "key"
            witness

        let getValue () =
            calls.Add "value"
            42

        (getKey ()).lock(getValue ()) |> ignore
        String.concat "," calls)

    expect "proptypekey.unlock evaluates receiver before argument" "key,value" (fun () ->
        let calls = ResizeArray<string>()

        let getKey () =
            calls.Add "key"
            witness

        let getValue () =
            calls.Add "value"
            locked

        (getKey ()).unlock(getValue ()) |> ignore
        String.concat "," calls)

    expect "proptypelock.Item evaluates its witness argument" "value,key" (fun () ->
        let calls = ResizeArray<string>()

        let getValue () =
            calls.Add "value"
            locked

        let getKey () =
            calls.Add "key"
            witness

        (getValue ()).Item(getKey ()) |> ignore
        String.concat "," calls)
