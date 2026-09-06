/**
 * A named callback declared with more than one call signature. Its own declaration is the
 * floor `TR031` reports on: F# writes one delegate signature, and there is no member here for
 * the rest to move onto.
 */
export type Formatter = {
  (value: number): string;
  (value: number, radix: number): string;
};

export interface Holder {
  /** A required member, written with property syntax around an object type rather than TS's
   * method syntax, whose two signatures differ in arity: F# admits both as an overload pair
   * under this member's own name, so nothing is dropped and no name is minted. */
  round: {
    (value: number): number;
    (value: number, precision: number): number;
  };

  /** A required member whose two signatures share arity and parameter types, differing only in
   * return type - .NET overload resolution never consults a return type, so the set is not
   * separable and `TR062` reports it rather than `TR031`. */
  parse: {
    (value: string): number;
    (value: string): boolean;
  };

  /** An optional member with two otherwise-separable signatures: F#'s method form has no way to
   * carry the option `?` demands, so this stays on the floor `TR031` describes. */
  measure?: {
    (value: number): number;
    (value: number, unit: string): number;
  };

  /** Reference position for `Formatter`, exercising the named declaration's own floor from a
   * member. */
  formatter: Formatter;
}
