[<AutoOpen>]
module Measures

[<MeasureAnnotatedAbbreviation>]
type string<[<Measure>] 'U> = string

module Measures =
    module String =
        let inline add<[<Measure>] 'U> (value: string) = unbox<string<'U>> value
        let inline replace (value: string<_>): string<'U> = unbox<string<'U>> value
        let inline remove (value: string<_>): string = unbox value
        /// <summary>
        /// String is relative to the last node_modules in the path.
        /// </summary>
        [<Measure>] type nodeModulesRelative
        /// <summary>
        /// String is has had its extension removed.
        /// </summary>
        [<Measure>] type extensionless
        /// <summary>
        /// String represents a directory
        /// </summary>
        [<Measure>] type directory
        /// <summary>
        /// String represents a package directory - the last part of the path is the package name.
        /// </summary>
        [<Measure>] type packageDirectory
