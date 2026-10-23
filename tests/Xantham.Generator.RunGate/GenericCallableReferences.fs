module Xantham.Generator.RunGate.GenericCallableReferences

open GenericCallableReferenceLab

let run check =
    let callback: PairCallback<string> = Exports.callback ()
    check "named generic callback can be returned and passed back" (Exports.accept callback = "leftright")
    let pair: Pair<string> = Exports.pair "seed"
    let seed, fromPair = pair
    check "generic tuple retains the callable contract" (seed = "seed" && Exports.accept fromPair = "leftright")
    let supplied = PairCallback<string>(fun left right -> right + left)
    check "consumer creates the same named callback type" (Exports.accept supplied = "rightleft")
