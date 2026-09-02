/// Tier 3 - Shape: the mapping document executed. Phase B of
/// `docs/plans/generator-architecture.md` covers interfaces with methods and overloads,
/// literal unions (D12), callbacks as delegates (D5), classes (instance interface plus
/// constructor members on `Exports`), ParamObject synthesis (D3), arrays, and value exports;
/// what remains richer than that widens to `obj` with a finding, so the fidelity manifest -
/// not silence - says what is not done yet. Every pass here is pure.
///
/// One file per pass, in `Shape/`, over the shared reading machinery in `Shape/Spec.fs`. The
/// list below is the tier's only entry point and the one place the order lives; the file order
/// in the project matches it, so a pass may refer to any pass ahead of it.
module Xantham.Generator.Shape.Passes

open Xantham.Generator

/// The tier's pass list, in execution order.
let passes: Pass<ShapeModel> list =
    [
        ExportNames.nameExports
        Anonymous.synthesizeAnonymous
        ConstructorObjects.nameConstructorObjects
        FreeTypeParams.bindFreeTypeParams
        LiteralUnions.classifyLiteralUnions
        TaggedUnions.detectTaggedUnions
        Callbacks.shapeCallbacks
        Interfaces.shapeInterfaces
        Aliases.shapeAliases
        Classes.shapeClasses
        Exports.shapeExports
        ParamObjects.synthesizeParamObjects
        Overloads.dedupeOverloads
        Ordering.orderDeclarations
        Arity.repairArity
        Coverage.auditCoverage
    ]
