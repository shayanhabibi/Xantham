module Xantham.Generator.Tests.Main

open Expecto

[<EntryPoint>]
let main argv =
    use _ = CatalogLibraryLab.lifetime ()
    Xantham.Tests.TestInventory.run argv
