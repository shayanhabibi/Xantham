// Lab: `$` in declaration and type-parameter names. JavaScript admits `$` in an identifier; F#
// rejects it in a type name even under backticks (FS0883) and after a type variable's tick
// (FS0010). Each declaration name reduces to its identifier shape and each type variable writes
// `$` as `_`, reported as SY005; the JavaScript name stays on the import.

/** An interface: declared `Shape`, a plain name, zod v4's `$ZodType` shape. */
export interface $Shape {
    a: string;
}

/** An alias: declared `Alias`. */
export type $Alias = { b: number };

/** A class: declared `Cls`, imported as `$Cls`. */
export declare class $Cls {
    constructor(value: string);
    value: string;
}

/** A type parameter: written `'_T`. */
export interface Box<$T> {
    v: $T;
}

/** A trailing `$` on a method's own type parameter: written `'K_`. */
export declare function pick<K$>(key: K$): K$;

/** Negative: the verbatim `Taken` keeps its name, and `$Taken` yields to it. */
export interface Taken {
    c: string;
}
export interface $Taken {
    d: string;
}

/** References each sanitised name, so the reference positions are gated too. */
export declare function use(shape: $Shape, alias: $Alias, cls: $Cls, box: Box<string>, taken: $Taken): Taken;
