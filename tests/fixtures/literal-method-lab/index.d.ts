/** A base emitter: one literal overload beside the generic one. */
export interface Emitter {
  on(event: "end", listener: () => void): this;
  on(event: string, listener: (...args: any[]) => void): this;
}

/**
 * The `net.Socket` shape. `connect`, `ready` and `end` take listeners of one shape, so they
 * collide once the literal widens.
 */
export interface Socket extends Emitter {
  /** Fires once the socket is fully closed. */
  on(event: "close", listener: (hadError: boolean) => void): this;
  on(event: "connect", listener: () => void): this;
  on(event: "ready", listener: () => void): this;
  on(event: "end", listener: () => void): this;
  on(
    event: "connectionAttempt",
    listener: (ip: string, port: number, family: number) => void,
  ): this;
  on(event: string, listener: (...args: any[]) => void): this;
  once(event: "close", listener: (hadError: boolean) => void): this;
  once(event: string, listener: (...args: any[]) => void): this;
  emit(event: "close", hadError: boolean): boolean;
  emit(event: "connect"): boolean;
  emit(event: string, ...args: any[]): boolean;
}

/** A name collision: the `on("close")` method meets the `onClose` property. */
export interface Dialog {
  onClose: (() => void) | null;
  on(event: "close", listener: () => void): this;
  on(event: "key-down", listener: (key: string) => void): this;
  on(event: string, listener: (...args: any[]) => void): this;
}

/** A literal after another parameter. */
export interface Store {
  read(key: string, kind: "text"): string;
  read(key: string, kind: "bytes"): number[];
  read(key: string): string;
}

/** Optional and rest tails after the literal. */
export interface Logger {
  log(level: "warn", message?: string): void;
  log(level: "info", ...parts: string[]): void;
  log(level: string, message?: string): void;
}

/** An element whose literal overload is added by an intersection operand. */
export interface Element {
  setAttribute(name: string, value: string): void;
}

export type Drawable = Element & {
  setAttribute(name: "draw", value: `${number} ${number}`): void;
};

/** Negative: one signature, so the literal widens to `string`. */
export interface Solo {
  tag(name: "only"): void;
}

/** Negative: a union of literals per position, which no single method name stands for. */
export interface Choice {
  pick(kind: "a" | "b"): void;
  pick(kind: "c" | "d"): void;
}

export declare const socket: Socket;
export declare const dialog: Dialog;
export declare const store: Store;
export declare const logger: Logger;
