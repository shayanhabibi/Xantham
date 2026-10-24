module Xantham.Generator.CompileGate.NamespaceAliasConsumer

open NestedNameLab

let toSource (value: PublicAlias.Item<'T>) : Original.Item<'T> = value
let fromSource (value: Original.Item<'T>) : PublicAlias.Item<'T> = value
let betweenAliases (value: AliasHolder.First.Inner.Leaf<'T>) : AliasHolder.Second.Inner.Leaf<'T> = value
let fromFunction () : PublicAlias.Inner.Leaf<string> = Exports.readAlias ()
