// Hand-authored fixture for the runtime import specifier (wave two lane D; recon blocker 5,
// docs/plans/generator-three-rung.md §8).
//
// The package is named `@types/types-only-lab`, which is what a DefinitelyTyped package looks
// like: it ships declarations and no JavaScript whatsoever. `[<Import(_, "@types/types-only-lab")>]`
// would resolve, at Fable output time, to a package with no runtime in it - the code lives in
// `types-only-lab`. So every attribute in this fixture's golden has to name `types-only-lab`,
// and none of them may name `@types/types-only-lab`.
//
// Three declarations, because two different call sites in the renderer write the specifier and
// both have to move: the `Exports` members (a function and a value) and a class static, which
// is written onto the type rather than onto `Exports`.
//
// The negative is the rest of the corpus rather than a declaration here, because a package has
// exactly one name: `lab` is published as `phase-b-lab`, is not types-only, and its committed
// golden still binds `"phase-b-lab"`. A package that is its own runtime keeps its own name, and
// the golden gate is what says so.

/** A function export: `[<Import("greet", ...)>]` on an `Exports` member. */
export declare function greet(name: string): string;

/** A value export: the same attribute, read as a get-only property. */
export declare const version: string;

/** A class, whose static is bound at the renderer's other specifier-writing call site. */
export declare class Counter {
    constructor(start: number);
    readonly value: number;
    static readonly MAX: number;
}
