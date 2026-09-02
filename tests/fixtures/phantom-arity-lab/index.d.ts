/**
 * An alias whose head declares a type parameter its resolved target never reaches.
 * `Reserved` is written on the left and nowhere on the right.
 */
export type SurplusParameter<T, Reserved> = (value: T) => void;

/** The negative: every declared parameter reaches the target. */
export type EveryParameter<T, Reserved> = (value: T) => Reserved;

/** Both aliases at a reference position. */
export interface Holder {
	surplus: SurplusParameter<string, number>;
	every: EveryParameter<string, number>;
}
