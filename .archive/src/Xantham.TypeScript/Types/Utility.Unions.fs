namespace Xantham.TypeScript

open Fable.Core
open TypeScript

[<Erase>]
type InlinedProgram<'T> =
    private | InlinedProgram of obj
    static member inline op_Implicit(other: InlinedProgram<'T>): 'T = unbox other
    interface IUnwrappable<'T>
    interface IInlinedProgram
type inlinedProgram<'T> = InlinedProgram<'T>


[<Erase>]
type ParentInlinedProgram<'T> =
    private | ParentInlinedProgram of obj
    static member inline op_Implicit(other: ParentInlinedProgram<'T>): 'T = unbox other
    interface IUnwrappable<'T>
type parentInlinedProgram<'T> = ParentInlinedProgram<'T>

module InlinedProgram =
    let inline create<'T> (program: Ts.Program) (value: 'T) =
        SymbolTypeKey.Program.addIfAbsent program value
        |> SymbolTypeKey.TypeChecker.addIfAbsent (program.getTypeChecker())
        |> unbox<'T inlinedProgram>
        
    let inline inject<'T> (program: Ts.Program) (value: 'T) = create program value |> unbox<'T>
    
    let inline tryUnbox<'T> (value: 'T) =
        if SymbolTypeKey.Program.get value |> ValueOption.isSome then
            unbox<'T inlinedProgram> value |> ValueSome
        else ValueNone
    let inline unbox<'T> (value: 'T) = unbox<'T inlinedProgram> value
    let inline program (value: 'T inlinedProgram) = SymbolTypeKey.Program.unsafeGet value
    let inline checker (value: 'T inlinedProgram) = SymbolTypeKey.TypeChecker.unsafeGet value

module ParentInlinedProgram =
    let inline wrap<'T, 'U> program ([<InlineIfLambda>] fn: 'T parentInlinedProgram -> 'U) (value: 'T) =
        fn (unbox<'T parentInlinedProgram> value)
        |> InlinedProgram.inject program
    let inline value (value: 'T parentInlinedProgram) = unbox<'T> value
