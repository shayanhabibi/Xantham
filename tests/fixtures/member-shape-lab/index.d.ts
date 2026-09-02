// Hand-authored fixture for the O7 shortcut's member exception. A type written in the compiler
// lib is referenced by name rather than read. That holds for a declaration and fails for a
// member: the type of `Promise.then` carries the symbol `then`, a member name rather than a
// declaration head.
//
// The positives reach a lib type structurally and require its members to survive as call
// signatures. The negatives name a lib declaration at a reference position and require the
// shortcut to keep firing.

/** An intersection over a compiler-lib type: `then` / `catch` / `finally` arrive as method
 *  symbols, and each is a callback rather than a declaration. */
export type Deferred = Promise<string> & { readonly tag: string };

/** The same reach by heritage rather than intersection. */
export interface Recorder extends EventTarget {
    readonly channel: string;
}

/** A lib declaration at a reference position: a shipped package binds it, and the binding stands
 *  for the whole type. */
export interface Stamped {
    readonly at: Date;
}

/** A lib declaration at a reference position outside every shipped package: it widens under its
 *  own name, with its members left unread. */
export interface Located {
    readonly matrix: DOMMatrix;
}

export declare const deferred: Deferred;
export declare const recorder: Recorder;
export declare const stamped: Stamped;
export declare const located: Located;
