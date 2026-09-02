// The `map` disposition of docs/plans/generator-architecture.md O7: a group redirected to a
// binding somebody already wrote by hand. Every type this fixture references from outside its
// own declarations comes from the compiler's `lib.*.d.ts`, so the compiler-lib group is the
// mapped one and `xantham.json` beside this file carries the table.
//
// What the fixture pins, in order below: a mapped name at a destination that takes no type
// arguments; a mapped generic at the arity its destination takes; a name the table does not
// carry, which widens with the rest of the group; a name applied at an arity the destination
// does not take, which widens rather than emitting an application that does not compile; and
// the two pinned Fable tables, which answer ahead of the configured one and are unchanged by
// the group being mapped.

/** A declaration of our own, so a mapped name can be seen passing through a generic. */
export interface Box<T> {
    value: T;
}

/** Another, to be the argument of a mapped generic. */
export interface Handle {
    id: string;
}

// ---------------------------------------------------------------------------
// Mapped. Neither `Fable.Core.JS` nor the `Fable.Browser.*` family binds either name; the BCL
// does, and Fable compiles both to the JavaScript object the declaration means.
// ---------------------------------------------------------------------------

/** A destination taking no type arguments: a name to write, nothing to apply. */
export declare function compile(pattern: RegExp): RegExp;

/** A generic destination, applied at the arity the table states. */
export declare function hold(handle: Handle): WeakRef<Handle>;

/** The destination reads at every position an ordinary reference does. */
export interface Registry {
    /** Under an array. */
    patterns: RegExp[];
    /** Optional, so it arrives wrapped in an option. */
    held?: WeakRef<Handle>;
    /** As the argument of a generic this package declares. */
    boxed: Box<RegExp>;
}

// ---------------------------------------------------------------------------
// The negatives.
// ---------------------------------------------------------------------------

/**
 * A name the table does not carry. `Response` and the rest of `fetch` live in `Fable.Fetch`,
 * which this run has no binding for, so the reference widens exactly as it does under the
 * group's default disposition: mapping is per name, and names outside the table keep what
 * they had.
 */
export declare function respond(): Response;

/**
 * A mapped name applied at an arity its destination does not take. A modern lib gives
 * `Iterator` a return type and a next type beside its element, and `IEnumerator<'T>` takes the
 * element alone, so writing the application would emit F# that does not compile. The reference
 * widens and the finding names the arity the site applied.
 */
export declare function walk(over: Iterator<number>): void;

// ---------------------------------------------------------------------------
// The two pinned tables, which the group's disposition does not move: a mapped compiler-lib
// group extends them by name rather than replacing them.
// ---------------------------------------------------------------------------

/** The ECMAScript half, from `Naming.LibBindings`. */
export declare function fetchOne(url: string): Promise<string>;

/** The DOM half, from the generated `Naming.BrowserBindings`. */
export declare function handle(target: EventTarget): void;
