namespace Xantham.TypeScript

open TypeScript
open Xantham.Fable

module SymbolTypeKey =
    let private symbolNameSigil = SymbolTypeKey.create<SymbolName> "SymbolName"
    let private typeCheckerSigil = SymbolTypeKey.create<Ts.TypeChecker> "TypeChecker"
    let private programSigil = SymbolTypeKey.create<Ts.Program> "Program"
    /// <summary>
    /// A cache for storing expensive computations while running xantham.
    /// Decision to cache value should be based on test results/benchmarks.
    /// </summary>
    let private cacheSigil = SymbolTypeKey.create<System.Collections.Generic.Dictionary<string, obj>> "Cache"

    type SymbolTypeKeyInterface<'Value> =
        abstract get: 'T -> 'Value voption
        abstract unsafeGet: 'T -> 'Value
        abstract getOrSet: (unit -> 'Value) -> 'T -> 'Value
        abstract getOrMap: ('T -> 'Value) -> 'T -> 'Value
        abstract setIfAbsent: 'Value -> 'T -> unit
        abstract addIfAbsent: 'Value -> 'T -> 'T
        abstract addIfAbsentWith: symbolValue: 'Value -> groupedOperations: ('T -> 'T) -> this: 'T -> 'T

    let inline private symbolInterface<'Value> (sigil: SymbolTypeKey<'Value>) = {
        new SymbolTypeKeyInterface<'Value> with
            member this.get(input) = SymbolTypeKey.access sigil input
            member this.unsafeGet(input) = SymbolTypeKey.unsafeAccess sigil input
            member this.getOrSet initFn input = SymbolTypeKey.accessOrInit sigil initFn input
            member this.getOrMap initFn input =
                if not <| SymbolTypeKey.has sigil input then
                    SymbolTypeKey.set sigil (initFn input) input
                SymbolTypeKey.unsafeAccess sigil input
            member this.setIfAbsent value input = SymbolTypeKey.setIfAbsent sigil value input
            member this.addIfAbsent value input = SymbolTypeKey.addIfAbsent sigil value input
            member this.addIfAbsentWith symbolValue groupedOperations input =
                if SymbolTypeKey.has sigil input then input else
                SymbolTypeKey.set sigil symbolValue input
                groupedOperations input
    }
    
    let SymbolName: SymbolTypeKeyInterface<SymbolName> = symbolInterface symbolNameSigil
    let TypeChecker: SymbolTypeKeyInterface<Ts.TypeChecker> = symbolInterface typeCheckerSigil
    let Program: SymbolTypeKeyInterface<Ts.Program> = symbolInterface programSigil
    let Cache: SymbolTypeKeyInterface<System.Collections.Generic.Dictionary<string, obj>> = symbolInterface cacheSigil

[<AutoOpen>]
module SymbolTypeKeyExtensions =
    type Ts.Symbol with
        member inline this.SymbolName = SymbolTypeKey.SymbolName.getOrSet (fun _ -> SymbolName.Create this.escapedName) this
    type Ts.Program with
        member inline this.TypeChecker = SymbolTypeKey.TypeChecker.getOrSet (fun _ -> this.getTypeChecker()) this