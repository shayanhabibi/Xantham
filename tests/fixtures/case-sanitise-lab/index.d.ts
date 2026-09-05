/**
 * A real TS enum whose member names carry characters illegal in a .NET union case name: `@`
 * and `/` from a model-key style name, a space, and a leading digit. The member's own name
 * becomes the case; its value stays the compiled text a JavaScript consumer reads.
 */
export enum Kind {
  "@cf/meta" = "meta",
  "beta channel" = "beta",
  "2fa" = "twofactor",
}

/** Negative: two member names that sanitise to the same identifier must still keep separate cases. */
export enum Collide {
  "a-b" = 1,
  "a_b" = 2,
}

/** Negative: an ordinary dash/underscore/dot-separated member name is untouched noise-wise. */
export enum Plain {
  "one-two" = "onetwo",
}
