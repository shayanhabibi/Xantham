module internal Xantham.Generator.CatalogMember

open System
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Computed symbols carry a session-local escaped-name ID. Their declaration handles retain
/// ownership without confusing distinct unique symbols which happen to share a spelling.
let key normalize (symbol: SymbolResponse) =
    if not (symbol.Name.StartsWith("__@", StringComparison.Ordinal)) then
        Some(JsonSerializer.Serialize {| ordinaryMember = symbol.Name |})
    else
        let handles =
            symbol.DeclarationHandles
            |> ValueOption.defaultValue [||]
            |> Array.map normalize
            |> Array.distinct
            |> Array.sort

        if Array.isEmpty handles then
            None
        else
            Some(JsonSerializer.Serialize {| computedMember = handles |})
