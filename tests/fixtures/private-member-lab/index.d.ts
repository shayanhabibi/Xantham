// Hand-authored fixture for TypeScript-private class members, written the way a `.d.ts` emit
// spells them: a `private` member keeps its name and loses its type, and every `#name` member
// collapses into one `#private;` field.

/** A class mixing every accessibility a declaration file carries. */
export declare class Account {
    #private;
    constructor(owner: string);
    /** Public state. */
    readonly owner: string;
    /** A public member whose name merely starts with an underscore: public API. */
    _legacy: number;
    /** Protected state stays: a subclass binding can reach it. */
    protected balance: number;
    protected audit(note: string): void;
    private secret;
    private rotate;
    deposit(amount: number): Account;
    /** Public getter over a private setter. */
    get limit(): number;
    private set limit(value);

    static readonly DEFAULT_OWNER: string;
    private static registry;
    static open(owner: string): Account;
}

/** A class built only through its static factory. */
export declare class Token {
    private constructor();
    readonly value: string;
    static issue(): Token;
}

/** A class whose only instance member is private. */
export declare class Opaque {
    private readonly handle;
}

/** An interface inherits a class's private member along with its public ones. */
export interface AccountView extends Account {
    label: string;
}

export declare function audit(account: Account): Opaque;
