module Xantham.Generator.CompilerLibCompileGate.NamespaceAliases

open TypeScript.Lib.Es

let arrayToCanonical (value: GlobalThis.Array<'T>) : Array<'T> = value
let arrayFromCanonical (value: Array<'T>) : GlobalThis.Array<'T> = value
let readonlyToCanonical (value: GlobalThis.ReadonlyArray<'T>) : ReadonlyArray<'T> = value
let partialToCanonical (value: GlobalThis.Partial<'T>) : Partial<'T> = value
let partialFromCanonical (value: Partial<'T>) : GlobalThis.Partial<'T> = value
