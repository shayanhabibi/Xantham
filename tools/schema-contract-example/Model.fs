namespace SchemaContractExample

open System
open System.ComponentModel
open System.ComponentModel.DataAnnotations

// This input model describes JSON before defaults are applied. The generator's
// internal model can retain resolved names, concrete Booleans, and empty maps.
// Convention for this example: the library's naming policy, optional fields omit
// rather than accept null, and each nonempty union case carries one JSON value.
module Model =
    type MappedNameObject =
        {
            [<Description("The qualified F# name of the destination type.")>]
            Name: string
            [<Description("Type argument count; omitted means zero.")>]
            [<Range(0, Int32.MaxValue)>]
            Arity: int option
        }

    type MappedNameInput =
        | ShortName of string
        | DetailedName of MappedNameObject

    type MappedGroup =
        {
            [<Description("TypeScript names redirected to existing F# types.")>]
            Map: Map<string, MappedNameInput>
        }

    type GroupAction =
        | Ship
        | Reference
        | Widen

    type GroupInput =
        | Action of GroupAction
        | Mapping of MappedGroup

    [<Description("A four-field example of Xantham's JSON input contract.")>]
    type ConfigInput =
        {
            [<Description("Override the generated F# module name; omission uses the package name.")>]
            Module: string option
            [<Description("Ambient type packages. Omission enables discovery; an empty array disables it.")>]
            Types: string list option
            [<Description("Resolve NoInfer<T> to T. Omission uses false.")>]
            ResolveNoInfer: bool option
            [<Description("Treatment of referenced packages, keyed by npm package name.")>]
            Groups: Map<string, GroupInput> option
        }
