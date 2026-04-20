module Xantham.Generator.Types.Customisation

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath

module Interceptors =
    type IgnoreRendersForPaths = {
        Source: ArenaInterner.QualifiedNamePart -> bool
        QualifiedName: QualifiedName -> bool
    } with
        static member Default = {
            Source = fun _ -> false
            QualifiedName = fun _ -> false
        }

type Interceptors = {
    IgnoreRendersForPaths: Interceptors.IgnoreRendersForPaths
} with
    static member Default = {
        IgnoreRendersForPaths = Interceptors.IgnoreRendersForPaths.Default
    }
    

type Customisation = {
    Interceptors: Interceptors
} with
    static member Default = {
        Interceptors = Interceptors.Default
    }
    static member Create(fn: Customisation -> Customisation) = fn Customisation.Default