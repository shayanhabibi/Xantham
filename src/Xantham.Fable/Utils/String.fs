[<RequireQualifiedAccess>]
module String

let replace (oldValue: string) (newValue: string) (input: string) =
    input.Replace(oldValue, newValue)

let inline remove (character: char) (text: string) = text.Trim(character)
module remove =
    let singleQuote = remove '''
    let doubleQuote = remove '"'

module Casing =
    let capitalize (text: string) =
        (string text[0]).ToUpper() + text[1..]
    let lowerAll (text: string) = text.ToLower()
    let lowerFirst (text: string) = (string text[0]).ToLower() + text[1..]

let splitLines (text: string) =
    Fable.Core.JS.Constructors
        .RegExp.Create(
            "\r\n|\r|\n"
            ).Split(text)

let normalizePath = replace "\\" "/"
