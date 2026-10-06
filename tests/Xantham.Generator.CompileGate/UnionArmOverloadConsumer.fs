module UnionArmOverloadConsumer

open UnionArmOverloadLab

// An arm beside a declared overload whose tail is optional: each call resolves without FS0041.
let prefixString () : string = Exports.prefix "a"
let prefixFloat () : string = Exports.prefix 1.0
let prefixWithTail () : string = Exports.prefix ("a", 2.0)
