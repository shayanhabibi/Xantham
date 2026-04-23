namespace Xantham.Decoder

open System.Text.RegularExpressions
open Fantomas.FCS.Syntax.PrettyNaming

/// <summary>
/// Utility type for working with names or manipulating the
/// names of types and members while preserving the original source.
/// </summary>
[<Struct>]
type Name =
    | Modified of original: string * modified: string
    | Source of original: string
    member this.ValueOrSource =
        match this with
        | Modified(original = source) | Source(source) -> source
    member this.ValueOrModified =
        match this with
        | Modified(modified = modified) | Source(modified) -> modified
    static member Create(value: string) = Source value
    static member Create(original: string, modified: string) = Modified(original, modified)
    static member CreateModified(original: string, modified: string) = Modified(original, modified)


/// Provide static typing over the casing of a name
[<MeasureAnnotatedAbbreviation>] type Name<[<Measure>] 'u> = Name

/// <summary>
/// This provides unsafe means of removing/adding measures to a <c>Name</c>.
/// Use with caution, as they are not associated with any transformations of the underlying strings.
/// </summary>
module Case =
    /// Measure to signify Pascal casing.
    type [<Measure>] pascal
    /// Measure to signify camel casing.
    type [<Measure>] camel
    /// Measure to signify the name is modified to represent the module interface.
    /// This is used to distinguish between the module interface and the module itself.
    type [<Measure>] modulename
    /// Measure to signify a type parameter (prefixed with a single quote).
    type [<Measure>] typar

    let inline addMeasure<[<Measure>] 'u> (name: Name): Name<'u> = unbox name
    let inline withoutMeasure (name: Name<'u>): Name = unbox name
    let inline unboxMeasure (name: Name<'u>): Name<'t> = unbox name
    let inline addPascalMeasure (name: Name) = addMeasure<pascal> name
    let inline addCamelMeasure (name: Name) = addMeasure<camel> name
    let inline addModuleMeasure (name: Name) = addMeasure<modulename> name
    let inline addTyparMeasure (name: Name) = addMeasure<typar> name
    
/// <summary>
/// Struct union for runtime safe access to typed name casings.
/// </summary>
[<Struct; RequireQualifiedAccess>]
type CasedName =
    | Pascal of Name<Case.pascal>
    | Camel of Name<Case.camel>
    | Module of Name<Case.modulename>
    | Typar of Name<Case.typar>
    member inline this.Value =
        match this with
        | Pascal name -> unbox<Name> name
        | Camel name -> unbox name
        | Module name -> unbox name
        | Typar name -> unbox name

module Name =
    module Normalization =
        type Setting =
            | Backticks
            | SafeCustom of (string -> string)
            | Custom of (string -> string)
            static member inline Default = Setting.Backticks
        let mutable private setting = Setting.Default
        let mutable private normalize_ = NormalizeIdentifierBackticks
        let normalize text = normalize_ text
        let setNormalizeSetting (newSetting: Setting) =
            setting <- newSetting
            match newSetting with
            | Backticks ->
                normalize_ <- NormalizeIdentifierBackticks
            | SafeCustom stringFunc ->
                normalize_ <- stringFunc >> NormalizeIdentifierBackticks
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
        name |> fn (Internal.stripBackticks >> Internal.toPascalCase >> Internal.normalizeString)
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
    let normalizeForTypeParameter = map (Internal.stripBackticks >> sprintf "'%s")
    /// <summary>
    /// Pascal cases the source name, and prefixes with a single quote.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// <para>The function is applied to the source string, even if the name is modified</para>
    /// </remarks>
    let sourceNormalizeForTypeParameter = mapSource (Internal.stripBackticks >> sprintf "'%s")
    /// <summary>
    /// Pascal cases the name, and prefixes with <c>I</c>.
    /// </summary>
    /// <remarks>
    /// <para>Backticks are removed prior to applying casing. Normalization is reapplied
    /// after casing is applied.</para>
    /// </remarks>
    let mapToModuleName (name: Name) =
        let p = pascalCase name
        match name with
        | Modified(original = s) | Source s when s.StartsWith "``" -> p
        | _ -> 
            p 
            |> map (fun s -> s.TrimStart('_')) 
            |> capitalize
            |> map (sprintf "I%s")
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
        | _ -> 
            p 
            |> map (fun s -> s.TrimStart('_')) 
            |> capitalize
            |> map (sprintf "I%s")
 
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
        let fromName = sourcePascalCase >> Case.addPascalMeasure
        let create = create >> fromName
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Camel case measure annotated names directly.
    module Camel =
        let fromName = sourceCamelCase >> Case.addCamelMeasure
        let create = create >> fromName
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Module measure annotated names directly.
    module Module =
        let fromName = sourceMapToModuleName >> Case.addModuleMeasure
        let create = create >> fromName
        let inline fromCase name = Case.withoutMeasure name |> fromName
    /// Provides means for working with Typar measure annotated names directly.
    module Typar =
        let fromName = sourceNormalizeForTypeParameter >> Case.addTyparMeasure
        let create = create >> fromName
        let inline fromCase name = Case.withoutMeasure name |> fromName
