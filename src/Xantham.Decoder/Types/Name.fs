namespace Xantham.Decoder

open System.Text.RegularExpressions
open Fantomas.FCS.Syntax.PrettyNaming

/// <summary>
/// Utility type for working with names or manipulating the
/// names of types and members while preserving the original source.
/// </summary>
/// <category index="3">Names and Casing</category>
[<Struct>]
type Name =
    | Modified of original: string * modified: string
    | Source of original: string
    /// <summary>The original source string regardless of whether the name has been modified.</summary>
    member this.ValueOrSource =
        match this with
        | Modified(original = source) | Source(source) -> source
    /// <summary>The modified string if the name has been modified; otherwise the original source string.</summary>
    member this.ValueOrModified =
        match this with
        | Modified(modified = modified) | Source(modified) -> modified
    /// <summary>Create an unmodified <c>Source</c> name from the given string. No normalization is applied.</summary>
    static member Create(value: string) = Source value
    /// <summary>Create a <c>Modified</c> name from explicit original and modified strings.</summary>
    static member Create(original: string, modified: string) = Modified(original, modified)
    /// <summary>Create a <c>Modified</c> name from explicit original and modified strings.</summary>
    static member CreateModified(original: string, modified: string) = Modified(original, modified)


/// <summary>Provide static typing over the casing of a name</summary>
/// <category index="3">Names and Casing</category>
[<MeasureAnnotatedAbbreviation>] type Name<[<Measure>] 'u> = Name

/// <summary>
/// This provides unsafe means of removing/adding measures to a <c>Name</c>.
/// Use with caution, as they are not associated with any transformations of the underlying strings.
/// </summary>
/// <category index="3">Names and Casing</category>
module Case =
    /// <summary>Measure to signify Pascal casing.</summary>
    /// <category index="3">Names and Casing</category>
    type [<Measure>] pascal
    /// <summary>Measure to signify camel casing.</summary>
    /// <category index="3">Names and Casing</category>
    type [<Measure>] camel
    /// <summary>
    /// Measure to signify the name is modified to represent the module interface.
    /// This is used to distinguish between the module interface and the module itself.
    /// </summary>
    /// <category index="3">Names and Casing</category>
    type [<Measure>] modulename
    /// <summary>Measure to signify a type parameter (prefixed with a single quote).</summary>
    /// <category index="3">Names and Casing</category>
    type [<Measure>] typar

    /// <summary>Tag a <c>Name</c> with the given casing measure. Performs no string transformation.</summary>
    let inline addMeasure<[<Measure>] 'u> (name: Name): Name<'u> = unbox name
    /// <summary>Strip the casing measure from a <c>Name&lt;_&gt;</c>.</summary>
    let inline withoutMeasure (name: Name<'u>): Name = unbox name
    /// <summary>Re-tag a <c>Name&lt;_&gt;</c> with a different casing measure. Performs no string transformation.</summary>
    let inline unboxMeasure (name: Name<'u>): Name<'t> = unbox name
    /// <summary>Tag a <c>Name</c> with the <c>pascal</c> casing measure.</summary>
    let inline addPascalMeasure (name: Name) = addMeasure<pascal> name
    /// <summary>Tag a <c>Name</c> with the <c>camel</c> casing measure.</summary>
    let inline addCamelMeasure (name: Name) = addMeasure<camel> name
    /// <summary>Tag a <c>Name</c> with the <c>modulename</c> casing measure.</summary>
    let inline addModuleMeasure (name: Name) = addMeasure<modulename> name
    /// <summary>Tag a <c>Name</c> with the <c>typar</c> casing measure.</summary>
    let inline addTyparMeasure (name: Name) = addMeasure<typar> name
    
/// <summary>
/// A <c>Name</c> tagged with one of the supported casing measures (Pascal, camel,
/// module, or typar). Useful as an envelope when the casing flavour is decided
/// dynamically rather than known statically.
/// </summary>
/// <category index="3">Names and Casing</category>
[<Struct; RequireQualifiedAccess>]
type CasedName =
    /// A name carrying the Pascal-case measure (e.g. type, module, or class names).
    | Pascal of Name<Case.pascal>
    /// A name carrying the camelCase measure (e.g. member or value names).
    | Camel of Name<Case.camel>
    /// A name carrying the module-name measure.
    | Module of Name<Case.modulename>
    /// A name carrying the type-parameter (typar) measure.
    | Typar of Name<Case.typar>
    /// <summary>The underlying untyped <c>Name</c>, dropping the casing measure.</summary>
    member inline this.Value =
        match this with
        | Pascal name -> unbox<Name> name
        | Camel name -> unbox name
        | Module name -> unbox name
        | Typar name -> unbox name

/// <summary></summary>
/// <category index="3">Names and Casing</category>
module Name =
    module Normalization =
        /// <summary></summary>
        /// <category index="3">Names and Casing</category>
        type Setting =
            | Backticks
            | SafeCustom of (string -> string)
            | Custom of (string -> string)
            static member inline Default = Setting.Backticks
        let mutable private setting = Setting.Default
        /// Characters that cannot appear in an F# identifier even when wrapped in
        /// double-backticks (newlines, braces, backtick). These leak in from e.g.
        /// destructured object-binding parameter names like `{ type, payload }`,
        /// whose source text Fantomas' NormalizeIdentifierBackticks cannot make valid.
        let private illegalInBackticks = Regex(@"[`\r\n{}]", RegexOptions.Compiled)
        /// Collapse a destructuring-pattern / multiline source name to a single safe
        /// identifier fragment before backtick-normalization. Drops the offending
        /// characters and whitespace runs; falls back to "arg" if nothing remains.
        let private sanitizeIdentifierSource (text: string) =
            // An empty/whitespace-only source (e.g. the TS `""` string-literal
            // union member) would be wrapped by NormalizeIdentifierBackticks into
            // an invalid empty backtick identifier (FS3563). Substitute a valid
            // placeholder; any CompiledName/EmitProperty attribute still carries
            // the real (empty) value.
            if System.String.IsNullOrWhiteSpace text then "Empty"
            elif illegalInBackticks.IsMatch text then
                let stripped =
                    illegalInBackticks.Replace(text, " ")
                    |> fun s -> Regex.Replace(s, @"\s+", " ")
                    |> fun s -> s.Trim().Replace(",", "")
                    |> fun s -> Regex.Replace(s, @"\s", "")
                if System.String.IsNullOrWhiteSpace stripped then "arg" else stripped
            else text
        let mutable private normalize_ = sanitizeIdentifierSource >> NormalizeIdentifierBackticks
        let normalize text = normalize_ text
        let setNormalizeSetting (newSetting: Setting) =
            setting <- newSetting
            match newSetting with
            | Backticks ->
                normalize_ <- sanitizeIdentifierSource >> NormalizeIdentifierBackticks
            | SafeCustom stringFunc ->
                normalize_ <- sanitizeIdentifierSource >> stringFunc >> NormalizeIdentifierBackticks
            | Custom stringFunc ->
                normalize_ <- stringFunc
    /// Creates a Name from a string. Will automatically normalize the name with backticks if required..
    /// Use the static member Create if you don't want automatic normalization.
    let create value =
        // Source value
        let name = Normalization.normalize value
        if value <> name then
            Modified(value, name)
        else Source value
    /// Creates a modified Name DU.
    let inline createModified original modified = Modified(original, modified)
    /// Retrieves the original source value for a name.
    let valueOrSource: Name -> string = _.ValueOrSource
    /// Retrieves the modified value for a name if it exists; otherwise returns the original source value.
    let valueOrModified: Name -> string = _.ValueOrModified
    /// <summary>
    /// Map a function over the current (modified or original if unmodified) name.
    /// </summary>
    /// <remarks>
    /// If the output matches the original, then you will receive a <c>Name.Source</c> case
    /// instead of a <c>Name.Modified</c> case, even if it was Modified before.
    /// </remarks>
    /// <param name="f"></param>
    /// <param name="name"></param>
    let map (f: string -> string) (name: Name) =
        match name with
        | Modified(original, modified) -> 
            let next = f modified
            if next = original then Source original
            else Modified(original, next)
        | Source original ->
            let next = f original
            if next = original then Source original
            else Modified(original, next)
    /// <summary>
    /// Map a function over the original source name.
    /// </summary>
    /// <remarks>
    /// If the output differs from the original, you will receive a <c>Name.Modified</c> case.
    /// </remarks>
    /// <param name="f"></param>
    /// <param name="name"></param>
    let mapSource (f: string -> string) (name: Name) =
        match name with
        | Source original
        | Modified(original,_) when f original <> original ->
            Modified(original, f original)
        | Modified(original,_) | Source(original) ->
            Source(original)
    /// <summary>
    /// Map a function over the modified name. Does not apply if the name is not modified.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="name"></param>
    let mapModified (f: string -> string) (name: Name) =
        match name with
        | Modified(original, modified) when f modified <> original ->
            Modified(original, f modified)
        | Source original 
        | Modified(original, _) ->
            Source(original)
    module private Internal =
        let isUpperSnakeCase (s: string) =
            s.Length > 0 &&
            s |> Seq.forall (fun c -> System.Char.IsUpper(c) || c = '_' || System.Char.IsDigit(c)) &&
            s |> Seq.exists System.Char.IsUpper

        let pascalCaseRegex = Regex(@"(?:^|[-_])(.)", RegexOptions.Compiled)
        let toPascalCase (s: string) =
            if isUpperSnakeCase s then s
            else
                pascalCaseRegex.Replace(s, fun m -> m.Groups.[1].Value.ToUpperInvariant())

        /// `$`, `/`, `.`, and `@` are valid in a backtick-quoted MEMBER name (`` ``a/b`` ``,
        /// `` ``c.d`` ``) but INVALID in a TYPE / module / union / namespace name even when
        /// backticked (FS0883 — and a backtick-escaped segment mid dotted-path, e.g.
        /// `` ``@scope``.Sub.T ``, is unparseable). They leak in from literal VALUES used as
        /// type-name fragments: MongoDB-style query operators (`$eq`/`$in`/...), MIME-type literals
        /// (`image/avif`), FQN-derived nested-literal names joined with `.`
        /// (`Message.inputImage.imageUrl`), and scoped-npm-package module segments (`@scope`, from a
        /// cross-package `node_modules/@scope/pkg` reference). Treat each as a WORD boundary so the
        /// name PascalCases across it (`$eq$gt` -> `EqGt`, `image/avif` -> `ImageAvif`,
        /// `Message.inputImage` -> `MessageInputImage`, `@modelcontextprotocol` ->
        /// `Modelcontextprotocol`) — readable, not just stripped — then drop any residual occurrence.
        /// Applied ONLY on the Pascal (type-level) path, so valid backtick-escaped member names are
        /// untouched.
        let private typeNameSeparators = Regex(@"[$/.@+\[\]]+(.)?", RegexOptions.Compiled)
        let sanitizeTypeName (s: string) =
            let cased =
                typeNameSeparators.Replace(s, fun m ->
                    if m.Groups.[1].Success then m.Groups.[1].Value.ToUpperInvariant() else "")
            // Drop any leading separator that produced no following char, and any stragglers.
            cased.Replace("$", "").Replace("/", "").Replace(".", "").Replace("@", "").Replace("+", "").Replace("[", "").Replace("]", "")

        let toCamelCase (s: string) =
            if isUpperSnakeCase s then s
            else
                let p = toPascalCase s
                if p.Length > 0 then
                    // Find first letter and lowercase it, keeping leading non-letters
                    let mutable i = 0
                    while i < p.Length && not (System.Char.IsLetter(p.[i])) do
                        i <- i + 1
                    if i < p.Length then
                        p.Substring(0, i) + p.Substring(i, 1).ToLowerInvariant() + p.Substring(i + 1)
                    else
                        p.ToLowerInvariant()
                else p

        let stripBackticks (s: string) =
            if s.StartsWith("``") && s.EndsWith("``") then s.Substring(2, s.Length - 4) else s
        let normalizeString = Normalization.normalize
    /// Normalizes the name by adding backticks if needed
    let normalize (name: Name) = name |> map Normalization.normalize
    /// Removes backticks, applies casing, and then reapplies backticks if necessary.
    let private _pascalCase (fn: (string -> string) -> Name -> Name) name =
        // `sanitizeTypeName` drops `$`/`/`/`.` (valid in member names, INVALID in type names —
        // FS0883) — Pascal path ONLY, so backtick-escaped member names keep them.
        name |> fn (Internal.stripBackticks >> Internal.toPascalCase >> Internal.sanitizeTypeName >> Internal.normalizeString)
    /// Removes backticks, applies casing, and then reapplies backticks if necessary.
    let private _capitalize (fn: (string -> string) -> Name -> Name) name =
        name |> fn (
            Internal.stripBackticks
            >> fun s -> s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)
            >> Internal.normalizeString
            )
    /// Removes backticks, applies casing, and then reapplies backticks if necessary.
    let private _camelCase (fn: (string -> string) -> Name -> Name) name =
        name |> fn (
            Internal.stripBackticks
            >> Internal.toCamelCase
            >> Internal.normalizeString
            )
        
    /// <summary>
    /// Applies Pascal Casing to the name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>If the name was modified, then the function is applied against the <i>Modified</i> value.</para>
    /// </remarks>
    let pascalCase (name: Name) = _pascalCase map name
    /// <summary>
    /// Applies Pascal Casing to the source name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourcePascalCase (name: Name) = _pascalCase mapSource name
    /// <summary>
    /// Capitalizes the name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// </remarks>
    let capitalize (name: Name) = _capitalize map name
    /// <summary>
    /// Capitalizes the source name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourceCapitalize (name: Name) = _capitalize mapSource name
    /// <summary>
    /// Applies camel casing to the name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// </remarks>
    let camelCase (name: Name) = _camelCase map name
    /// <summary>
    /// Applies camel casing to the source name.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourceCamelCase (name: Name) = _camelCase mapSource name
    /// Normalizes name for types. Uses PascalCase.
    let normalizeForType = pascalCase
    /// Normalizes name for types. Uses PascalCase.
    let sourceNormalizeForType = sourcePascalCase
    /// Normalizes name for parameters. Uses camelCase.
    let normalizeForParameter = camelCase
    /// Normalizes name for parameters. Uses camelCase.
    let sourceNormalizeForParameter = sourceCamelCase
    /// Normalizes name for properties. Uses camelCase.
    let normalizeForProperty = camelCase
    /// Normalizes name for properties. Uses camelCase.
    let sourceNormalizeForProperty = sourceCamelCase
    /// Normalizes name for methods. Uses camelCase.
    let normalizeForMethod = camelCase
    /// Normalizes name for methods. Uses camelCase.
    let sourceNormalizeForMethod = sourceCamelCase
    /// Normalizes name for enum cases. Uses PascalCase.
    let normalizeForEnumCase = pascalCase
    /// Normalizes name for enum cases. Uses PascalCase.
    let sourceNormalizeForEnumCase = sourcePascalCase
    /// <summary>
    /// Pascal cases the name, and prefixes with a single quote.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// </remarks>
    // A type parameter CANNOT be backtick-escaped (the leading quote forbids it), so the body
    // must be a bare identifier. Collapse separators like `-`/`_` (e.g. "key-of" -> "keyOf")
    // that would otherwise emit the un-parseable `'key-of` (FS0010), while leaving already-valid
    // bodies ("T", "key") untouched. Lowercase only the leading char of a multi-segment result
    // so a separatored source camelCases; a single-segment body keeps its case.
    let private sanitizeTyparBody (s: string) =
        let body = Internal.stripBackticks s
        let isIdentChar c = System.Char.IsLetterOrDigit c || c = '_'
        if body |> Seq.forall isIdentChar && body.Length > 0 && not (System.Char.IsDigit body.[0]) then body
        else
            // Collapse `-`/`_` separators via PascalCase, then STRIP any remaining char that is
            // illegal in a bare identifier (dot, space, slash, quote, ...) — a typar body must
            // be a bare ident (it cannot be backtick-escaped). Lowercase the lead letter so a
            // separatored source camelCases; drop a leading digit.
            let stripped =
                Internal.toPascalCase body
                |> Seq.filter isIdentChar
                |> Seq.toArray
                |> System.String
            let trimmed = stripped.TrimStart([| '0'..'9' |])
            let p = if trimmed.Length = 0 then "T" else trimmed
            if System.Char.IsLetter p.[0]
            then string (System.Char.ToLowerInvariant p.[0]) + p.Substring(1)
            else "T" + p
    let normalizeForTypeParameter = map (sanitizeTyparBody >> sprintf "'%s")
    /// <summary>
    /// Pascal cases the source name, and prefixes with a single quote.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourceNormalizeForTypeParameter = mapSource (sanitizeTyparBody >> sprintf "'%s")
    /// <summary>
    /// Pascal cases the name, and prefixes with <c>I</c>.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// </remarks>
    // Prepend "I" to the cased body. Must operate on the BARE (un-backticked) body and
    // re-normalize once — otherwise prepending to an already-backticked result (e.g.
    // ``With space``) glues the "I" OUTSIDE the fence => two tokens => invalid F#.
    let private prefixIModule (name: Name) =
        name
        |> map (fun s -> s.TrimStart('_'))
        |> capitalize
        |> map (Internal.stripBackticks >> sprintf "I%s" >> Internal.normalizeString)
    let mapToModuleName (name: Name) =
        let p = pascalCase name
        match name with
        | Modified(original = s) | Source s when s.StartsWith "``" -> p
        | _ -> prefixIModule p
    /// <summary>
    /// Pascal cases the source name, and then prefixes with <c>I</c>.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourceMapToModuleName (name: Name) =
        let p = sourcePascalCase name
        match name with
        | Modified(original = s) | Source s when s.StartsWith "``" -> p
        | _ -> prefixIModule p
 
    /// <summary>
    /// Provides some equivalency functions for working with Names that have measures.
    /// </summary>
    module Case =
        let inline valueOrModified (name: Name<'u>) = Case.withoutMeasure name |> valueOrModified
        let inline valueOrSource (name: Name<'u>) = Case.withoutMeasure name |>valueOrSource
        let inline map fn (name: Name<'u>) = Case.withoutMeasure name |> map fn
        let inline mapSource fn (name: Name<'u>) = Case.withoutMeasure name |> mapSource fn
        let inline isModified (name: Name<'u>) = Case.withoutMeasure name |> _.IsModified
        let inline isSource (name: Name<'u>) = Case.withoutMeasure name |> _.IsSource
    /// Provides means for working with Pascal case measure annotated names directly.
    module Pascal =
        /// <summary>Convert a <c>Name</c> to PascalCase and tag it with the <c>pascal</c> measure.</summary>
        let fromName = sourcePascalCase >> Case.addPascalMeasure
        /// <summary>Normalize a string into a <c>Name&lt;pascal&gt;</c>.</summary>
        let create = create >> fromName
        /// <summary>Re-cast any cased name to a <c>Name&lt;pascal&gt;</c>, applying PascalCase normalization.</summary>
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Camel case measure annotated names directly.
    module Camel =
        /// <summary>Convert a <c>Name</c> to camelCase and tag it with the <c>camel</c> measure.</summary>
        let fromName = sourceCamelCase >> Case.addCamelMeasure
        /// <summary>Normalize a string into a <c>Name&lt;camel&gt;</c>.</summary>
        let create = create >> fromName
        /// <summary>Re-cast any cased name to a <c>Name&lt;camel&gt;</c>, applying camelCase normalization.</summary>
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Module measure annotated names directly.
    module Module =
        /// <summary>Map a <c>Name</c> into a module-name shape and tag it with the <c>modulename</c> measure.</summary>
        let fromName = sourceMapToModuleName >> Case.addModuleMeasure
        /// <summary>Normalize a string into a <c>Name&lt;modulename&gt;</c>.</summary>
        let create = create >> fromName
        /// <summary>Re-cast any cased name to a <c>Name&lt;modulename&gt;</c>, applying module-name normalization.</summary>
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Typar measure annotated names directly.
    module Typar =
        /// <summary>Normalize a <c>Name</c> for use as a TypeScript-style type parameter and tag it with the <c>typar</c> measure.</summary>
        let fromName = sourceNormalizeForTypeParameter >> Case.addTyparMeasure
        /// <summary>Normalize a string into a <c>Name&lt;typar&gt;</c>.</summary>
        let create = create >> fromName
        /// <summary>Re-cast any cased name to a <c>Name&lt;typar&gt;</c>, applying typar normalization.</summary>
        let inline fromCase name = Case.withoutMeasure name |> fromName
