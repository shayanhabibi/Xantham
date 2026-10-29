export type FunctionMaybe<T = unknown> = { (): T } | T;
export type Live = "off" | "polite" | "assertive";
export interface Attrs {
  "aria-live"?: FunctionMaybe<"off" | "polite" | "assertive" | undefined>;
  mode: FunctionMaybe<Live>;
  count?: FunctionMaybe<number | undefined>;
  /** Nullability added by `?` rather than written in the argument. */
  size?: FunctionMaybe<number>;
  label: FunctionMaybe<string>;
  /** The `"a" | string` hint idiom stays string. */
  hint: FunctionMaybe<"a" | "b" | string>;
  /** A boolean mixed with literals keeps its current form. */
  flag: FunctionMaybe<boolean | "true" | "false">;
  /** A union argument keeps the flattened form, so each constituent casts with one `!^`. */
  text: FunctionMaybe<string | number>;
  /** A nullable union argument keeps the flattened form. */
  value?: FunctionMaybe<string | number | string[] | undefined>;
  /** A named erased-union argument keeps the flattened form. */
  pair: FunctionMaybe<Pair>;
}
export type Pair = string | boolean;
/** A transformed union keeps the alias symbol over a different arm set and is not an application. */
export type Plain = NonNullable<FunctionMaybe<"x" | "y" | null>>;
