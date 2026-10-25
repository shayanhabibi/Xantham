module Xantham.Generator.RunGate.BacktickMembers

open CensusBacktickMembersLab

let run check =
    let equal claim expected actual = check claim (actual = expected)
    equal "legacy dollar-backtick read" "legacy" Exports.legacy.Item
    equal "legacy dollar-backtick construction" "created" (Legacy.Create "created").Item

    let value = Exports.collision

    equal
        "leading double underscore literal method keeps source key and receiver"
        "underscore"
        (value.__promisify__Close "underscore")

    equal "leading double underscore overload family keeps source key" 9.0 (value.__promisify__End 9.0)
    let escaped = EscapedKeys.Create("required", Optional = "optional")

    equal
        "escaped compiler key construction preserves required and optional source keys"
        "[\"required\",true,\"optional\"]"
        (Exports.inspectEscapedKeys escaped)

    equal "escaped compiler key getter preserves source key" "required" escaped.Required
    escaped.Required <- "written"

    equal
        "escaped compiler key setter preserves source key"
        "[\"written\",true,\"optional\"]"
        (Exports.inspectEscapedKeys escaped)

    equal "escaped compiler static getter preserves source key" "escaped-static" KeyedClass.Property
    KeyedClass.Property <- "escaped-static-write"
    equal "escaped compiler static setter preserves source key" "escaped-static-write" KeyedClass.Property
    equal "ordinary inherited Item remains distinct" "original" value.Item
    equal "first normalized key remains distinct" "dollar" value.Item2
    equal "second normalized key remains distinct" "tick" value.Item3
    value.Item2 <- "written"
    equal "backtick setter writes the original key" "written" value.Item2
    equal "backtick setter preserves the ordinary key" "original" value.Item
    equal "inherited Value reserves its spelling" "original-value" value.Value
    equal "first Value collision" "first" value.Value2
    equal "second Value collision" "second" value.Value3
    equal "renamed method keeps its receiver" "called:receiver" (value.Invoke2 "called")
    equal "ordinary method remains callable" 4.0 (value.Invoke 4.0)
    equal "Create name collision retains original property" "key-create" value.Create2
    equal "ordinary Create property remains distinct" "create" value.Create
    equal "symbol-name collision keeps original string method" "text:key" (value.SymbolToPrimitive2 "text")
    equal "symbol member remains callable" "hint" (value.SymbolToPrimitive "hint")

    equal
        "literal suffix collision preserves string method"
        "literal-symbol"
        (value.SymbolTo_Primitive "literal-symbol")

    equal "literal suffix collision preserves overload family" 3.0 (value.SymbolToOther 3.0)
    equal "literal overload keeps receiver and original key" "event" (value.On_End "event")
    equal "literal overload family retains both signatures" 8.0 (value.OnClose 8.0)
    equal "ordinary literal-overload collision remains distinct" "ordinary" value.OnEnd
    equal "quotes backslashes and macro holes are literal key text" "escaped" value.QuoteSlash1
    value.QuoteSlash1 <- "escaped-write"
    equal "escaped key setter writes exact text" "escaped-write" value.QuoteSlash1

    let absent = Options.Create("required", 7.0)

    equal
        "Create writes required original keys and omits optional key"
        "[\"required\",7,false,null,false,false]"
        (Exports.inspectOptions absent)

    let present = Options.Create("required", 7.0, Optional = "present")

    equal
        "Create writes explicit optional original key"
        "[\"required\",7,true,\"present\",false,false]"
        (Exports.inspectOptions present)

    let inherited =
        DerivedOptions.Create("extra", "inherited", 9.0, Optional = "inherited-optional")

    equal
        "derived Create retains inherited original key mappings"
        "[\"inherited\",9,true,\"inherited-optional\",false,false]"
        (Exports.inspectOptions inherited)

    equal "derived Create retains its own field" "extra" inherited.extra
    let inheritedAbsent = DerivedOptions.Create("extra", "inherited", 9.0)

    equal
        "derived Create omits inherited optional key"
        "[\"inherited\",9,false,null,false,false]"
        (Exports.inspectOptions inheritedAbsent)

    let renamed = Exports.renamedDerived
    equal "derived ordinary name remains directly accessible" 12.0 renamed.Value
    equal "inherited backtick key remains accessible beside ordinary name" "inherited-key" renamed.Value2
    equal "base contract keeps original backtick key" "inherited-key" (renamed :> RenamedBase).Value

    let choiceA = Choice.Create("tag-a", "value-a")

    equal
        "folded arm Create writes original common key and only its own keys"
        "[\"tag-a\",true,\"value-a\",false,null,false]"
        (Exports.inspectChoice choiceA)

    equal "folded arm property reads original key" "tag-a" choiceA.Tag
    let choiceB = Choice.Create("tag-b", 5.0)

    equal
        "folded arm overload writes original key and other exclusive field"
        "[\"tag-b\",false,null,true,5,false]"
        (Exports.inspectChoice choiceB)

    let instance = Exports.KeyedClass()
    equal "class instance key read" "instance" instance.Item
    instance.Item <- "instance-write"
    equal "class instance call preserves receiver" "instance-write:call" (instance.Call "call")
    equal "class static key read" "static" KeyedClass.Item2
    equal "ordinary static read remains intact" "ordinary-static" KeyedClass.ordinary
    KeyedClass.ordinary <- "ordinary-static-write"
    equal "ordinary static setter remains intact" "ordinary-static-write" KeyedClass.ordinary
    KeyedClass.Item2 <- "static-write"
    equal "class static call preserves constructor receiver" "static-write:call" (KeyedClass.Call2 "call")
