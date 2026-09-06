namespace Fidelity.CloudEdge

open Fable.Core

/// <summary>
/// Erased union type to represent 1 of 10 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U10<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

/// <summary>
/// Erased union type to represent 1 of 11 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U11<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

/// <summary>
/// Erased union type to represent 1 of 14 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U14<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K, 'L, 'M, 'N> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K
    | Case12 of 'L
    | Case13 of 'M
    | Case14 of 'N

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'L) = Case12 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'M) = Case13 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'N) = Case14 x

/// <summary>
/// Erased union type to represent 1 of 15 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U15<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K, 'L, 'M, 'N, 'O> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K
    | Case12 of 'L
    | Case13 of 'M
    | Case14 of 'N
    | Case15 of 'O

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'L) = Case12 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'M) = Case13 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'N) = Case14 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'O) = Case15 x

/// <summary>
/// Erased union type to represent 1 of 18 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U18<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K, 'L, 'M, 'N, 'O, 'P, 'Q, 'R> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K
    | Case12 of 'L
    | Case13 of 'M
    | Case14 of 'N
    | Case15 of 'O
    | Case16 of 'P
    | Case17 of 'Q
    | Case18 of 'R

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'L) = Case12 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'M) = Case13 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'N) = Case14 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'O) = Case15 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'P) = Case16 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'Q) = Case17 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'R) = Case18 x

/// <summary>
/// Erased union type to represent 1 of 22 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U22<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K, 'L, 'M, 'N, 'O, 'P, 'Q, 'R, 'S, 'T, 'U, 'V> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K
    | Case12 of 'L
    | Case13 of 'M
    | Case14 of 'N
    | Case15 of 'O
    | Case16 of 'P
    | Case17 of 'Q
    | Case18 of 'R
    | Case19 of 'S
    | Case20 of 'T
    | Case21 of 'U
    | Case22 of 'V

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'L) = Case12 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'M) = Case13 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'N) = Case14 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'O) = Case15 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'P) = Case16 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'Q) = Case17 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'R) = Case18 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'S) = Case19 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'T) = Case20 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'U) = Case21 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'V) = Case22 x

/// <summary>
/// Erased union type to represent 1 of 33 possible values.
/// <a href="https://fable.io/docs/javascript/features.html#erased-unions">Read more</a>
/// </summary>
[<Erase>]
type U33<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J, 'K, 'L, 'M, 'N, 'O, 'P, 'Q, 'R, 'S, 'T, 'U, 'V, 'W, 'X, 'Y, 'Z, 'AA, 'BB, 'CC, 'DD, 'EE, 'FF, 'GG> =
    | Case1 of 'A
    | Case2 of 'B
    | Case3 of 'C
    | Case4 of 'D
    | Case5 of 'E
    | Case6 of 'F
    | Case7 of 'G
    | Case8 of 'H
    | Case9 of 'I
    | Case10 of 'J
    | Case11 of 'K
    | Case12 of 'L
    | Case13 of 'M
    | Case14 of 'N
    | Case15 of 'O
    | Case16 of 'P
    | Case17 of 'Q
    | Case18 of 'R
    | Case19 of 'S
    | Case20 of 'T
    | Case21 of 'U
    | Case22 of 'V
    | Case23 of 'W
    | Case24 of 'X
    | Case25 of 'Y
    | Case26 of 'Z
    | Case27 of 'AA
    | Case28 of 'BB
    | Case29 of 'CC
    | Case30 of 'DD
    | Case31 of 'EE
    | Case32 of 'FF
    | Case33 of 'GG

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'A) = Case1 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'B) = Case2 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'C) = Case3 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'D) = Case4 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'E) = Case5 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'F) = Case6 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'G) = Case7 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'H) = Case8 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'I) = Case9 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'J) = Case10 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'K) = Case11 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'L) = Case12 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'M) = Case13 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'N) = Case14 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'O) = Case15 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'P) = Case16 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'Q) = Case17 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'R) = Case18 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'S) = Case19 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'T) = Case20 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'U) = Case21 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'V) = Case22 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'W) = Case23 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'X) = Case24 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'Y) = Case25 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'Z) = Case26 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'AA) = Case27 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'BB) = Case28 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'CC) = Case29 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'DD) = Case30 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'EE) = Case31 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'FF) = Case32 x

    [<Emit("$0")>]
    static member op_ErasedCast (x: 'GG) = Case33 x

module Erased =
    /// Advisory alias: content under `Empty` is erased by recipe policy.
    type Empty = obj
    [<AllowNullLiteral>] type Empty<'A> = interface end
    [<AllowNullLiteral>] type Empty<'A, 'B> = interface end
    [<AllowNullLiteral>] type Empty<'A, 'B, 'C> = interface end
    /// Advisory alias: content under `Eventsource` is erased by recipe policy.
    type Eventsource = obj
    [<AllowNullLiteral>] type Eventsource<'A> = interface end
    [<AllowNullLiteral>] type Eventsource<'A, 'B> = interface end
    [<AllowNullLiteral>] type Eventsource<'A, 'B, 'C> = interface end
    /// Advisory alias: content under `Intl` is erased by recipe policy.
    type Intl = obj
    [<AllowNullLiteral>] type Intl<'A> = interface end
    [<AllowNullLiteral>] type Intl<'A, 'B> = interface end
    [<AllowNullLiteral>] type Intl<'A, 'B, 'C> = interface end
    /// Advisory alias: content under `JsonSchema` is erased by recipe policy.
    type JsonSchema = obj
    [<AllowNullLiteral>] type JsonSchema<'A> = interface end
    [<AllowNullLiteral>] type JsonSchema<'A, 'B> = interface end
    [<AllowNullLiteral>] type JsonSchema<'A, 'B, 'C> = interface end
    /// Advisory alias: content under `JsonSchemaTyped` is erased by recipe policy.
    type JsonSchemaTyped = obj
    [<AllowNullLiteral>] type JsonSchemaTyped<'A> = interface end
    [<AllowNullLiteral>] type JsonSchemaTyped<'A, 'B> = interface end
    [<AllowNullLiteral>] type JsonSchemaTyped<'A, 'B, 'C> = interface end
    /// Advisory alias: content under `TypesJsonSchema` is erased by recipe policy.
    type TypesJsonSchema = obj
    [<AllowNullLiteral>] type TypesJsonSchema<'A> = interface end
    [<AllowNullLiteral>] type TypesJsonSchema<'A, 'B> = interface end
    [<AllowNullLiteral>] type TypesJsonSchema<'A, 'B, 'C> = interface end
