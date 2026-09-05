/// The terse model-construction vocabulary the per-pass tests are written in: wire responses
/// have many fields and the tests care about two or three, so each shape is written out once
/// here and copy-updated at use sites.
module Xantham.Generator.Tests.Build

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator

/// A context for pure passes. The session is deliberately absent - a pure pass touching the
/// wire is a bug, and the null deref is how this harness reports it.
let context =
    {
        Session = Unchecked.defaultof<Session<TscMailbox>>
        Config = GeneratorConfig.Default
        PackageDir = "."
        PackageName = "test-pkg"
        EntryFile = "index.d.ts"
    }

let symbol (id: int) (name: string) (flags: SymbolFlags) : SymbolResponse =
    {
        Id = id
        Project = "test"
        Name = name
        Flags = flags
        CheckFlags = Unchecked.defaultof<CheckFlags>
        Declarations = ValueNone
        ValueDeclaration = ValueNone
        Parent = ValueNone
        ExportSymbol = ValueNone
    }

let typeResponse (id: int) (flags: TypeFlags) : TypeResponse =
    {
        Id = id
        Flags = flags
        ObjectFlags = ValueNone
        IsTupleType = ValueNone
        Value = null
        Target = ValueNone
        TypeParameters = ValueNone
        OuterTypeParameters = ValueNone
        LocalTypeParameters = ValueNone
        ElementFlags = ValueNone
        FixedLength = ValueNone
        Readonly = ValueNone
        LabeledElementDeclarations = ValueNone
        ObjectType = ValueNone
        IndexType = ValueNone
        CheckType = ValueNone
        ExtendsType = ValueNone
        BaseType = ValueNone
        SubstConstraint = ValueNone
        Texts = ValueNone
        FreshType = ValueNone
        RegularType = ValueNone
        IsThisType = ValueNone
        IntrinsicName = ValueNone
        AliasTypeArguments = ValueNone
        AliasSymbol = ValueNone
        Symbol = ValueNone
    }

let facts (response: TypeResponse) = TypeFacts.shallow response

let resolvedMember (sym: SymbolResponse) (typeId: int) : ResolvedMember =
    {
        Symbol = sym
        Docs = ""
        Tags = []
        Optional = false
        ReadOnly = false
        TypeId = typeId
    }

let export (name: string) (sym: SymbolResponse) : HarvestedExport =
    {
        ExportName = name
        Symbol = sym
        Docs = ""
        Tags = []
        Origin = FromModule
        Order = None
    }

/// A shape model over the given type table, everything else empty.
let shapeModel (table: TypeFacts list) : ShapeModel =
    {
        Harvest = HarvestModel.Empty
        ExportTypes = Map.empty
        Types = table |> List.map (fun facts -> facts.Response.Id, facts) |> Map.ofList
        NotFollowed = Map.empty
        DeclNames = Map.empty
        DeclOrders = Map.empty
        DeclParams = Map.empty
        ExportMembers = []
        TypeVars = Map.empty
        KeyVars = Map.empty
        Decls = []
    }

/// A call or construct signature over the given parameters, no rest tail.
let signature (parameters: ResolvedMember list) (returnTypeId: int) : ResolvedSignature =
    {
        Parameters = parameters
        HasRest = false
        TypeParameters = []
        IsAbstract = false
        ReturnTypeId = returnTypeId
    }

/// Runs one pass to completion under the wire-less context and splits the outcome.
let runPass (pass: Pass<'Model>) (model: 'Model) : 'Model * Finding list =
    match Async.RunSynchronously(pass.Run context model) with
    | Advanced advanced -> advanced, []
    | Degraded(degraded, findings) -> degraded, findings

// The primitive corner of a type table, under the ids the tests refer to them by.
let stringType = facts (typeResponse 1 TypeFlags.String)
let numberType = facts (typeResponse 2 TypeFlags.Number)
let booleanType = facts (typeResponse 3 (TypeFlags.Boolean ||| TypeFlags.Union))
let voidType = facts (typeResponse 4 TypeFlags.Void)
let undefinedType = facts (typeResponse 5 TypeFlags.Undefined)
let nullType = facts (typeResponse 6 TypeFlags.Null)

let primitives =
    [ stringType; numberType; booleanType; voidType; undefinedType; nullType ]
