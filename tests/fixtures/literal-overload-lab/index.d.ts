/** Options an overload selects with a literal type argument rather than a literal parameter. */
export interface ReadOptions<Kind> {
  kind: Kind;
  cacheTtl?: number;
}

export interface Chunk {
  size: number;
}

/**
 * The construct. Six overloads over two colliding groups: three tell themselves apart by a
 * literal-typed `kind` parameter, two more by the literal a `ReadOptions` argument carries, and
 * the sixth stands alone because `ReadOptions<undefined>` carries no literal at all.
 */
export interface Store {
  read(key: string, options?: ReadOptions<undefined>): Promise<string | null>;
  read(key: string, kind: "text"): Promise<string | null>;
  read(key: string, kind: "json"): Promise<unknown>;
  read(key: string, kind: "bytes"): Promise<Chunk | null>;
  read(key: string, options?: ReadOptions<"text">): Promise<string | null>;
  read(key: string, options?: ReadOptions<"bytes">): Promise<Chunk | null>;
}

/** Negative: one signature, so the literal separates nothing and widens to `string`. */
export interface Solo {
  tag(name: "only"): void;
}

/**
 * Negative: a property typed by a literal the overload set above also carries. The checker
 * interns a literal type, so this pins retention as a property of the position rather than of
 * the type.
 */
export interface Label {
  kind: "text";
}

/** Negative: the parameter lists differ before the literal is read, so nothing collides. */
export interface Mixed {
  send(body: string, mode: "now"): void;
  send(body: number, mode: "later"): void;
}

/** Negative: a union of literals per position, which one literal type cannot stand for. */
export interface Choice {
  pick(kind: "a" | "b"): void;
  pick(kind: "c" | "d"): void;
}

/** Negative: a collision no literal is party to, which drops an overload as it always did. */
export interface Widen {
  scan(input: unknown): void;
  scan(input: any): void;
}
