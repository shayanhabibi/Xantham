namespace Xantham.Fable.Core

open Fable.Core

/// <summary>
/// A TypeScript index signature (<c>{ [key: K]: V }</c>): a value read and written through
/// <c>Item</c> compiles to the same property access an index signature's own type would.
/// </summary>
type Record<'Key, 'Value> =
    [<EmitIndexer>]
    abstract Item: 'Key -> 'Value with get, set

/// <summary>
/// A readonly TypeScript index signature (<c>{ readonly [key: K]: V }</c>): the same shape
/// as <see cref="Record{Key,Value}"/>, without the setter a readonly index signature never
/// gave it.
/// </summary>
type ReadonlyRecord<'Key, 'Value> =
    [<EmitIndexer>]
    abstract Item: 'Key -> 'Value
