[<RequireQualifiedAccess>]
module Array

let apply voidFun (arr: _ array) =
    for x in arr do voidFun x
    arr

let revApply voidFun (arr: _ array) =
    if arr.Length > 0
    then for x in [ arr.Length - 1 .. 0 ] do voidFun arr[x]
    arr