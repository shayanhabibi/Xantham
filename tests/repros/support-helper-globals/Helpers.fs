module SupportHelperGlobals

open Fable.Core.JS

type Settings = { Count: int }

[<Measure>]
type Identifier

let erasedPropertyName () =
    let key = unbox<typekeyof<Settings, int>> "Count"
    key.Value

let propertyName () =
    TypeKeyOf.create (fun (settings: Settings) -> settings.Count) |> TypeKeyOf.value

let propertyValue () =
    let key = unbox<typekeyof<Settings, int>> "Count"
    TypeKeyOf.item key { Count = 42 }

let optionalValue () =
    let key = unbox<keyof<Settings>> "Count"
    KeyOf.item key { Count = 42 } |> Option.map unbox<int>

let brandRoundTrip (value: string) =
    value |> Brand.tagString<Identifier> |> Brand.untagString
