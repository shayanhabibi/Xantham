/// <reference lib="es2020" />
// W3 (#134). The program @types/node builds: es2020 through a lib reference, with the compiler
// lib under the default widen disposition. Each name below is generated in Core.TS.

/** Declared once in lib.es5.d.ts. */
export declare function fill(target: ArrayLike<number>): void;
/** An indexed-access alias over ArrayBuffer and SharedArrayBuffer. */
export declare function wrap(buffer: ArrayBufferLike): void;
/** Declared in lib.es2017.sharedmemory.d.ts; Core.TS adds the es2024 members. */
export declare function share(buffer: SharedArrayBuffer): void;
/** The synchronous iteration protocol, as Core.TS's JS interface. */
export declare function words(values: Iterable<string>): void;
export declare function entries(): ArrayIterator<number>;
export declare function test(pattern: RegExp): boolean;
export declare const generatorFunction: AsyncGeneratorFunction;
/** A namespace member: Core.TS flattens it to its bare name. */
export declare function format(options: Intl.NumberFormatOptions): string;
/** A name Fable.Core binds keeps its Fable.Core.JS spelling. */
export declare function later(): Promise<string>;
