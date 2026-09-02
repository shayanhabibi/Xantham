// Hand-authored fixture for wave four lane O: docs/fable5-workarounds.md §3. An interface
// declaring a method still gains a `[<ParamObject; Emit("$0")>]` Create, with the method bound
// to a delegate-typed parameter - the same type a function-valued property of that signature
// already carries (D5). The delegate receives no `this`; that is a later question.
//
// The negatives are the four shapes that keep getting no Create at all, one declaration each.

/** A method beside a required and an optional property. The method's parameter is required, so
 *  it sorts ahead of the optional property; a method returning `void` is an `Action`. */
export interface Listener {
    name: string;
    tag?: string;
    notify(count: number): string;
    reset(): void;
}

/** A negative: an index signature has no name to bind a Create parameter to, whatever named
 *  members sit beside it. */
export interface Bag {
    label: string;
    lookup(key: string): number;
    [key: string]: unknown;
}

/** The instance side of the constructor-object negative. */
export interface Handle {
    readonly id: string;
}

/** A negative: the construct signature already made `Create` members of its own (§4.4), so the
 *  constructor object gets no ParamObject overload beside them. */
export declare const Handle: {
    new (id: string): Handle;
    reset(): void;
};

/** A negative: two same-named methods would bind two Create parameters of one name. */
export interface Formatter {
    locale: string;
    format(value: number): string;
    format(value: number, digits: number): string;
}

/** A negative: twenty-five members, one over the Create parameter budget. */
export interface Wide {
    a1: number; a2: number; a3: number; a4: number; a5: number; a6: number;
    a7: number; a8: number; a9: number; a10: number; a11: number; a12: number;
    a13: number; a14: number; a15: number; a16: number; a17: number; a18: number;
    a19: number; a20: number; a21: number; a22: number; a23: number; a24: number;
    go(): void;
}

export declare const listener: Listener;
export declare const bag: Bag;
export declare const formatter: Formatter;
export declare const wide: Wide;
