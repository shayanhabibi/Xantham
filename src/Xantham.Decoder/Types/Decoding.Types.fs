namespace Xantham.Decoder

open Xantham

/// <summary>
/// Map from a <c>TypeKey</c> to the structural <c>TsType</c> it identifies.
/// Produced by the decoder as the canonical store of all referenced types.
/// </summary>
/// <category index="1">Decoded Result</category>
type TypeMap = Map<TypeKey, TsType>

/// <summary>
/// Map from a <c>TypeKey</c> to the <c>TsExportDeclaration</c> it identifies.
/// Holds top-level exported declarations (variables, interfaces, classes, etc.)
/// keyed by the same <c>TypeKey</c> address space as <see cref="T:TypeMap"/>.
/// </summary>
/// <category index="1">Decoded Result</category>
type ExportTypeMap = Map<TypeKey, TsExportDeclaration>

/// <summary>
/// Map from a source module path (string) to the set of <c>TypeKey</c>s
/// declared as exports by that source. Used to look up all exports originating
/// from a given <c>.d.ts</c> file or module.
/// </summary>
/// <category index="1">Decoded Result</category>
type ExportMap = Map<string, Set<TypeKey>>

/// <summary>
/// Set of <c>TypeKey</c>s that originate from the TypeScript standard
/// (<c>lib.es*.d.ts</c>) library files. Used to differentiate runtime/builtin
/// declarations from user-defined declarations.
/// </summary>
/// <category index="1">Decoded Result</category>
type LibSet = TypeKey Set

/// <summary>
/// Intermediate result produced by the compression pass, holding the compressed
/// type and export maps along with the protected top-level export set.
/// </summary>
/// <category index="1">Decoded Result</category>
type CompressionResult = {
    /// Compressed map of structural types, keyed by canonical <c>TypeKey</c>.
    Types: TypeMap
    /// Compressed map of export declarations, keyed by <c>TypeKey</c>.
    Exports: ExportTypeMap
    /// Map of top-level exports preserved through compression.
    TopLevelExports: ExportTypeMap
}

/// <summary>
/// Final shape returned by <c>Decoder.read</c>/<c>Decoder.readWithSettings</c>.
/// Contains all decoded type, export, and module information from a Xantham
/// JSON file in a form suitable for downstream generators.
/// </summary>
/// <category index="1">Decoded Result</category>
type DecodedResult = {
    /// All structural types referenced anywhere in the source, keyed by <c>TypeKey</c>.
    TypeMap: TypeMap
    /// All exported declarations, keyed by <c>TypeKey</c>.
    ExportTypeMap: ExportTypeMap
    /// Mapping from source module path to the set of <c>TypeKey</c>s exported by it.
    ExportMap: ExportMap
    /// The <c>TypeKey</c>s of declarations exported at the top level of the input.
    TopLevelExports: TypeKey list
    /// The <c>TypeKey</c>s belonging to the TypeScript <c>lib.es*</c> standard library.
    LibEsExports: TypeKey list
}
