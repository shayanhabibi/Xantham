/// Tier 3 - Shape: the mapping document executed. What it does not cover widens to `obj` with
/// a finding, so the fidelity manifest says so. Every pass here is pure. One file per pass over
/// the shared reading machinery in `Shape/Spec.fs`; the project's file order matches this list.
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
