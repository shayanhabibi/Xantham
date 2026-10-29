module Xantham.Generator.RunGate.GenericCallableInterfaces

open GenericCallableInterfaceLab
open Fable.Core.JsInterop

let run check =
    let identity =
        { new Identity with
            member _.Invoke<'T>(value: 'T) : 'T = value
        }

    let supplied = Exports.accept identity
    let imported: Identity = Exports.identity
    let roundtrip = Exports.accept imported
    check "imported callable identity" (emitJsExpr (imported, roundtrip) "$0 === $1")
    check "imported number" (imported.Invoke 73 = 73)
    check "imported string" (imported.Invoke "native" = "native")
    let importedAnonymous = Exports.anonymousIdentity
    check "anonymous export number" (importedAnonymous.Invoke 74 = 74)
    check "anonymous export string" (importedAnonymous.Invoke "anon-export" = "anon-export")
    check "anonymous export identity" (emitJsExpr (importedAnonymous, Exports.anonymousIdentity) "$0 === $1")
    let alias = Exports.identityAlias
    check "typeof alias number" (alias.Invoke 75 = 75)
    check "typeof alias string" (alias.Invoke "alias" = "alias")
    check "typeof alias native identity" (emitJsExpr (alias, Exports.declaredIdentity) "$0 === $1")
    check "identity preserved" (emitJsExpr (identity, supplied) "$0 === $1")
    check "number" (supplied.Invoke 42 = 42)
    check "string" (supplied.Invoke "hello" = "hello")

    let inherited =
        { new InheritedIdentity with
            member _.Invoke<'T>(value: 'T) : 'T = value
        }

    let parent: Identity = inherited
    check "base identity" (emitJsExpr (inherited, parent) "$0 === $1")
    check "derived callable" ((Exports.accept inherited).Invoke true)

    let anonymous =
        { new Anonymous.Callback with
            member _.Invoke<'T>(value: 'T) : 'T = value
        }

    let result = Exports.anonymous anonymous
    check "anonymous identity" (emitJsExpr (anonymous, result) "$0 === $1")
    check "anonymous number" (result.Invoke 7 = 7)
    check "anonymous string" (result.Invoke "anon" = "anon")
    let pair = Exports.pairWith "outer"
    check "outer method binders" (pair.Invoke 9 = ("outer", 9))
    check "outer second invocation" (pair.Invoke false = ("outer", false))
    check "imported derived" ((Exports.inherited ()).Invoke 11 = 11)

    let redundant =
        { new RedundantIdentity with
            member _.Invoke<'T>(value: 'T) : 'T = value
        }

    let redundantBase: Identity = redundant
    let redundantIntermediate: InheritedIdentity = redundant
    check "redundant base identity" (emitJsExpr (redundant, redundantBase) "$0 === $1")
    check "redundant callable" (redundantIntermediate.Invoke 13 = 13)

    let closed =
        { new StringPair with
            member _.Invoke<'U>(value: 'U) : string * 'U = "closed", value
        }

    check "closed base" (closed.Invoke 14 = ("closed", 14))

    let generic =
        { new RedundantGeneric<string> with
            member _.Invoke<'U>(value: 'U) : string * 'U = "generic", value
        }

    let genericBase: PairWith<string> = generic
    let genericMid: RenamedPair<string> = generic
    check "generic base identity" (emitJsExpr (generic, genericBase) "$0 === $1")
    check "generic base method" (genericBase.Invoke 15 = ("generic", 15))
    check "generic middle method" (genericMid.Invoke false = ("generic", false))

    let collision =
        { new Collision<string> with
            member _.Invoke<'U>(value: 'U) : string * 'U = "outer", value
        }

    check "same-spelled binders stay distinct" (collision.Invoke 18 = ("outer", 18))
    check "same-spelled second invocation" (collision.Invoke false = ("outer", false))
