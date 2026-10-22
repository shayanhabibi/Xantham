// Temporary source copy from Fickl 0.5.1, src/Fickl.Core/Fickl.Core.Types.fs:
// https://github.com/shayanhabibi/fickl/blob/56687cf139247d3ef0e2552a1451a3acd8ca6a5d/src/Fickl.Core/Fickl.Core.Types.fs
// Replace this file with a Fickl.Core package reference when that package is published.
(*
The MIT License (MIT)

Copyright (c) Alfonso García-Caro and contributors.
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
namespace Fickl.Core

open System

/// <summary>
/// Compiles a case of a <c>TypeScriptTaggedUnion</c> as its interface field with the tag added,
/// instead of as an object holding the field.
/// On the union type, it applies to every case that has fields.
/// </summary>
/// <remarks>
/// The case must have exactly one field, whose type is an interface:
/// <code>
/// [&lt;TypeScriptTaggedUnion("type")>]
/// type Command =
///     | [&lt;InlineInterface>] Take of Take
///     | Save
/// </code>
/// The TypeScript type of the <c>Take</c> case is <c>Take &amp; { type: "take" }</c>.
/// Constructing the case copies the field value with its prototype and sets the tag on the copy.
/// Reading the field returns the union value.
/// </remarks>
type InlineInterfaceAttribute() =
    inherit Attribute()
