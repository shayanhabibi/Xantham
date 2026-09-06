// Hand-authored fixture for callable-and-properties hybrids: an object, interface or class that
// is directly invocable while also carrying properties. F# has no value that is both callable
// and carries members, so the hybrid is declared as an interface whose call signatures become
// `[<Emit("$0($1...)")>]` `Invoke` members - `$0` is the object the member is read off, so
// `x.Invoke(a)` reaches JavaScript as `x(a)` (§4.4's counterpart for the call side, mirroring
// `ConstructorObjects.fs`'s `Create` for the construct side).
//
// The negative is here too: a hybrid whose own member is already named `Invoke` would collide
// with the emission name, so that one case keeps the call signature loss instead.

/** A hybrid reached at a member position: the property's type is an anonymous object that is
 *  both callable and carries a property. */
export interface Widget {
    handler: {
        (event: string): string;
        enabled: boolean;
    };
}

/** The same shape, written as a named declaration directly. */
export interface Trigger {
    (value: number): number;
    label: string;
}

/** Two call signatures on one hybrid, distinguished by arity: both become `Invoke` overloads. */
export interface Multi {
    (x: number): number;
    (x: number, y: number): number;
    tag: string;
}

/** Two call signatures that do not separate under F#'s overload rules - same parameter types,
 *  different return types only. One `Invoke` overload survives; `dedupe-overloads` drops the
 *  other the same way it drops any other colliding overload. */
export interface Ambiguous {
    (x: number): number;
    (x: number): string;
    note: string;
}

/** A generic call signature, where the interface itself owns no type parameters. */
export interface Identity {
    <T>(value: T): T;
    calls: number;
}

/** A generic interface whose call signature reads the interface's own type parameter. */
export interface Boxed<T> {
    (): T;
    value: T;
}

export declare const widget: Widget;
export declare const trigger: Trigger;
export declare const multi: Multi;
export declare const ambiguous: Ambiguous;
export declare const identity: Identity;
export declare const boxedNumber: Boxed<number>;

// ---------------------------------------------------------------------------
// The negative: a member already named Invoke collides with the emission name.
// ---------------------------------------------------------------------------

/** A hybrid whose own member is already called `Invoke`: emitting a second one would collide, so
 *  the call signature stays a loss (SI001) rather than reaching Invoke (SI008). */
export interface Collides {
    (x: number): number;
    Invoke: string;
}

export declare const collides: Collides;
