// A parameter states its presence three ways, and F# has two spellings for the three. `?` and
// `| undefined` both reach `?p: T`; a required parameter of a non-nullish type reaches `p: T`.
// The pair of findings a position carries is what tells the first two apart afterwards.

/** `b?: T` - the marker alone. */
export declare function marked(a: string, b?: string): string;

/** `b: T | undefined` - the declared type alone. Callers must still pass something. */
export declare function unioned(a: string, b: string | undefined): string;

/** Neither. The negative both of the above are read against. */
export declare function required(a: string, b: string): string;

/** `?` over a type that already absorbs `undefined`: the marker is the only fact there is. */
export declare function markedAny(a: string, b?: any): string;

/** The same three at method positions, where the owner is an interface rather than a module. */
export interface Station {
    marked(a: string, b?: string): string;
    unioned(a: string, b: string | undefined): string;
    required(a: string, b: string): string;
}
