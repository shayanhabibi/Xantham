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
        SourceStringAliases.nameAliases
        ConstructorObjects.nameConstructorObjects
        FreeTypeParams.bindFreeTypeParams
        MixedUnionCensus.countMixedUnions
        LiteralUnions.classifyLiteralUnions
        TaggedUnions.detectTaggedUnions
        ErasedObjectUnions.detectErasedObjectUnions
        Callbacks.shapeCallbacks
        Interfaces.shapeInterfaces
        Aliases.shapeAliases
        SourceStringAliases.reifyAliases
        Classes.shapeClasses
        Exports.shapeExports
        ParamObjects.synthesizeParamObjects
        Inherited.dropInherited
        SymbolMembers.settleSymbolMembers
        Overloads.dedupeOverloads
        Ordering.orderDeclarations
        UnionArms.expandUnionArms
        Arity.repairConstraints
        Arity.repairArity
        Arity.normalizeObjUnions
        ExportCollisions.resolveExportCollisions
        Orphans.dropOrphanDelegates
        Coverage.auditCoverage
    ]
