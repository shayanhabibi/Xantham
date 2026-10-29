export interface Schema<Out = unknown> {
    readonly out: Out;
}

/** An alias of the defaulted instantiation, used as a bound. */
export type AnySchema = Schema;
/** An alias of a different instantiation, used as a bound. */
export type NumberSchema = Schema<number>;

export interface Fn<Returns extends AnySchema = AnySchema> {
    /** Bound written as the alias's target. */
    output<NewReturns extends Schema>(output: NewReturns): Fn<NewReturns>;
    /** Bound written as the alias itself. */
    aliased<NewReturns extends AnySchema>(output: NewReturns): Fn<NewReturns>;
    /** Structural bound F# cannot relate to `Schema`. */
    shaped<NewReturns extends { readonly out: unknown }>(output: NewReturns): Fn<NewReturns>;
}

export interface NumberFn<Returns extends NumberSchema = NumberSchema> {
    /** Bound written as the alias's target instantiation. */
    output<NewReturns extends Schema<number>>(output: NewReturns): NumberFn<NewReturns>;
}
