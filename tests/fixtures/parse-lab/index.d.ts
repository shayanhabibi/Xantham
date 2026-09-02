// A method-level type parameter whose constraint is itself a generic application renders a head
// ending `>>`. F# lexes `>>` as one token, so the member's colon is swallowed and the whole file
// fails to parse with FS0010 - which stops the compile gate before it can report anything else.
// Reduced from `three`'s `Raycaster.intersectObject`.
export interface EventMap {
    readonly click: { at: number };
}
export interface Object3D<TEventMap> {
    readonly id: number;
}
export interface Intersection<TIntersected> {
    readonly object: TIntersected;
}

export class Caster {
    // The head this renders ends `>>`, immediately before the colon.
    intersectObject<TIntersected extends Object3D<EventMap>>(object: Object3D<EventMap>): Intersection<TIntersected>[];
    // Two constraints, so the `when` clause is joined with `and` and still ends `>>`.
    // Both parameters are read by the signature, so both survive to the head and the `when`
    // clause joins them with `and` - which still ends in `>>`.
    intersectPair<TFirst extends Object3D<EventMap>, TSecond extends Object3D<EventMap>>(
        first: TFirst,
        second: TSecond,
    ): Intersection<TFirst>[];
}

// Negatives: heads that end in a single `>` lex correctly and must keep the tight colon.
export interface Plain {
    // No type parameters at all.
    ping(at: number): void;
    // A parameter, but no constraint: head ends `<'T>`.
    echo<T>(value: T): T;
    // A constraint that is not a generic application: head ends `Ev>`.
    on<T extends EventMap>(handler: T): void;
}
