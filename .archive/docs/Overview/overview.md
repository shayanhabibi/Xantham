---
title: Repository Structure
category: Overview
categoryindex: 0
---

# Xantham

The *Xantham* repository is a collection of `.NET` and `Fable` F# libraries
and projects that provide a pipeline for encoding TypeScript type information
into a JSON schema, and decoding that schema into F# types.

    |- src/
        |- Xantham.Common/
        |- Xantham.Fable/
        |- Xantham.Fable.Core/
        |- Xantham.Decoder/
        |- Xantham.Generator/

## Xantham Pipeline

The Xantham pipeline is a two phase process of encoding `ts` information
into a `json` internal representation (IR) via `Fable`, and then decoding the IR
into `.NET` for use in generators.

    // == Encoding Phase ==          == Decoding Phase ==
    TypeScript -> Fable -> | IR Json | -> Decoder -> .NET

### Common

> This project is not built/packaged; the `Encoder` and `Decoder` projects
> include the `src/Xantham.Common/Common.Types.fs` file directly into their
> builds instead.
  
`src/Xantham.Common` is a mono-file project that provides the communication
scheme between the two phases.

## Fable

`src/Xantham.Fable` is the encoding library and/or CLI tool which produces
the json IR for consumption by the decoder.

The core functionality is quite generic usage of the `TypeScript` compiler API,
but provides high level interfaces and abstractions for a stack-based iterative
approach to traversing the type hierarchies using signals to propagate changes,
with the ability to trace type nodes through the pipeline for debugging if required.

The type hierarchy is then encoded using `Thoth.Json` manual encoders.

The section for `Xantham.Fable` documents the main API, usage, and development
of the library/tool.

## Decoder

`src/Xantham.Decoder` is a .NET library responsible for decoding the JSON schema
emitted by `Xantham.Fable` into strongly-typed F# structures.

It provides the utility layer for convenient generator consumption and handles the conversion
from JSON data structures into F# types that can be used by generators.

## Generator

> The Xantham.Generator project is an example implementation of a consumer of the
> Xantham.Decoder library. It is suitable for use, and can be extended or changed
> to fit the needs of your own projects.

`src/Xantham.Generator` contains the core rendering logic that consumes the decoded
schema and generates F# bindings. It provides a foundation for implementing custom generators
using the decoded type information from `Xantham.Decoder`.

## Fable.Core

`src/Xantham.Fable.Core` is a small Fable utility library that provides F# representations
of TypeScript type-system idioms that have no direct equivalent in standard F#.
All types are erased at runtime — they exist only for the compiler and carry zero overhead
in the emitted JavaScript.

It provides utilities for working with `keyof type`, indexed access types, and heterogeneous
property unions that are necessary for complete TypeScript type system support in F#.

The key components in this library are:

- `keyof<'T>` - TypeScript `keyof T`
- `typekeyof<'T, 'ReturnType>` - TypeScript `keyof T` with value-type retention  
- `proptypekey<'T, 'ReturnType>` and `proptypelock<'T>` - TypeScript `T[keyof T]` (union of all property value types)
- `PropertyRecord<'T, 'K>` - index-signature objects

This library enables more accurate TypeScript to F# translation by handling complex type constructs.