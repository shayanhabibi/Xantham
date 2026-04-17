#r "nuget: SignalsDotnet"

open SignalsDotnet


let sig1 = Signal(5)
let sig2 = Signal.Computed(fun () ->
    let value = sig1.Value * 2
    System.Console.WriteLine(value)
    value
    )
let sig3 = Signal(0)
let x =
    use _ = new Effect(fun () ->
        if sig3.Value > 0 then
            sig1.Value <- sig3.Value
        )
    ()


sig3.Value <- 10
sig2.Value