/// The finding catalogue's one invariant that the compiler cannot hold: keys are positional, so
/// they are stable only while unions stay append-only. This snapshot is the guard - a renumbered
/// key fails here, not silently in every golden manifest.
module Xantham.Generator.Tests.FindingsTests

open Expecto
open Xantham.Generator

let private tierLabel =
    function
    | Exact -> "exact"
    | Ergonomic -> "ergonomic"
    | Widened -> "widened"
    | Escape -> "escape"

[<Tests>]
let findingsTests =
    testList
        "findings"
        [ testCase "every finding key is unique" <| fun _ ->
              let keys = FindingCatalogue.table () |> List.map (fun (key, _, _) -> key)
              Expect.equal (List.distinct keys) keys "no two cases share a key"

              let prefixes = FindingCatalogue.unions |> List.map (fun t -> (Coder.forType t).Prefix)
              Expect.equal (List.distinct prefixes) prefixes "no two unions share a prefix"

          testCase "the key table is append-only" <| fun _ ->
              let table =
                  FindingCatalogue.table ()
                  |> List.map (fun (key, case, tier) -> $"{key} {case} {tierLabel tier}")

              let expected =
                  [ "TR001 TypeReference.SelfReferenceThroughUnnamed widened"
                    "TR002 TypeReference.TypeNotResolved widened"
                    "TR003 TypeReference.MissingFromTypeTable escape"
                    "TR004 TypeReference.LoneEnumMemberToFloat widened"
                    "TR005 TypeReference.LoneEnumMemberToString widened"
                    "TR006 TypeReference.StringLiteralToString widened"
                    "TR007 TypeReference.NumericLiteralToFloat widened"
                    "TR008 TypeReference.AnyToObj escape"
                    "TR009 TypeReference.UnknownToObj widened"
                    "TR010 TypeReference.PolymorphicThisAsDeclaringType ergonomic"
                    "TR011 TypeReference.ThisOutsideDeclaration widened"
                    "TR012 TypeReference.TypeParameterOutOfScopeToConstraint widened"
                    "TR013 TypeReference.TypeParameterOutOfScope widened"
                    "TR014 TypeReference.TypeFlagsNotMapped widened"
                    "TR015 TypeReference.KeyOfOpenOperand ergonomic"
                    "TR016 TypeReference.KeyOfOperandOutOfScope widened"
                    "TR017 TypeReference.UnnamedBrandToPrimitive ergonomic"
                    "TR018 TypeReference.IntersectionOverNonObject widened"
                    "TR019 TypeReference.IntersectionNotDeclared widened"
                    "TR020 TypeReference.IndexedAccessNoForm widened"
                    "TR021 TypeReference.AnonymousInReferencedGroup widened"
                    "TR022 TypeReference.GlobalThisToObj widened"
                    "TR023 TypeReference.NotAmongGeneratedDeclarations widened"
                    "TR024 TypeReference.LibExtraTypeArgumentsDropped ergonomic"
                    "TR025 TypeReference.LibBindingLoss ergonomic"
                    "TR026 TypeReference.ConstrainedArgumentWidened widened"
                    "TR027 TypeReference.ArgumentNotBoundWithConstraint widened"
                    "TR028 TypeReference.TupleRestToArray widened"
                    "TR029 TypeReference.TupleArityNoForm widened"
                    "TR030 TypeReference.CallableWithoutSignatures widened"
                    "TR031 TypeReference.CallbackOverloadsFromFirst widened"
                    "TR032 TypeReference.NullableHoistedToOption ergonomic"
                    "TR033 TypeReference.OnlyNullUndefinedToUnit widened"
                    "TR034 TypeReference.EmptyUnionToObj widened"
                    "TR035 TypeReference.UnionWithObjArm widened"
                    "TR036 TypeReference.UnionTooWide widened"
                    "TR037 TypeReference.TemplateLiteralToString widened"
                    "TR038 TypeReference.StringMappingToString widened"
                    "TR039 TypeReference.BigIntLiteralToBigInt widened"
                    "TR040 TypeReference.ObjectTypeToObj widened"
                    "TR041 TypeReference.SymbolNoBinding widened"
                    "TR042 TypeReference.UniqueSymbolNoBinding widened"
                    "TR043 TypeReference.ConstructorObjectNotDeclared widened"
                    "TR044 TypeReference.ArgumentNotASubtypeOfConstraint widened"
                    "TR045 TypeReference.ConditionalTypeDeferred widened"
                    "TR046 TypeReference.ConditionalResolvedToBranch ergonomic"
                    "TP001 TypeParameters.UnnamedTypeParameter widened"
                    "TP002 TypeParameters.ConstraintDropped ergonomic"
                    "TP003 TypeParameters.GenericFunctionHoisted ergonomic"
                    "TP004 TypeParameters.KeyWithIndexedAccess ergonomic"
                    "TP005 TypeParameters.KeyOverOperand ergonomic"
                    "TP006 TypeParameters.TypeParameterErased widened"
                    "TP007 TypeParameters.UnnamedTypeParametersCounted widened"
                    "TP008 TypeParameters.ConstraintNotProvenNominal ergonomic"
                    "MB001 Members.OptionalParameterAsOption ergonomic"
                    "MB002 Members.SymbolKeyedMemberDropped widened"
                    "MB003 Members.OptionalMemberAsOption ergonomic"
                    "MB004 Members.IndexSignatureAsIndexer ergonomic"
                    "HG001 HarvestGlobals.AmbientModuleDropped escape"
                    "HG002 HarvestGlobals.UnwritableGlobalDropped escape"
                    "HG003 HarvestGlobals.NothingHarvested escape"
                    "RE001 ResolveExportTypes.FacetNotResolved escape"
                    "RT001 ResolveTypeTable.FrontierNotResolved widened"
                    "RT002 ResolveTypeTable.TypeNotResolved widened"
                    "LU001 ClassifyLiteralUnions.NonStringLiteralCase exact"
                    "DT001 DetectTaggedUnions.ArmNotPlainData ergonomic"
                    "DT002 DetectTaggedUnions.TaggedUnion exact"
                    "SY001 SynthesizeAnonymous.InstantiationNamedOnce exact"
                    "SY002 SynthesizeAnonymous.HoistArgumentsNotRecovered widened"
                    "SI001 ShapeInterfaces.HybridLosesCallSignatures widened"
                    "SI002 ShapeInterfaces.BaseMembersFlattened ergonomic"
                    "SI003 ShapeInterfaces.IntersectionFlattened ergonomic"
                    "SI004 ShapeInterfaces.ConstructorObjectDeclared ergonomic"
                    "SI005 ShapeInterfaces.BaseInherited exact"
                    "SI006 ShapeInterfaces.BaseNotDeclaredHere ergonomic"
                    "SI007 ShapeInterfaces.BaseWouldCycle ergonomic"
                    "SA001 ShapeAliases.BrandAsMeasure ergonomic"
                    "SA002 ShapeAliases.PhantomComputation widened"
                    "SC001 ShapeClasses.ClassWithoutValueType escape"
                    "SC002 ShapeClasses.StaticMemberDropped widened"
                    "SC003 ShapeClasses.StaticReadOnly widened"
                    "SC004 ShapeClasses.StaticWithoutDeclaration widened"
                    "SC005 ShapeClasses.StaticMethodWithoutSignatures widened"
                    "SE001 ShapeExports.NoValueType escape"
                    "SE002 ShapeExports.RuntimeSpecifierDerived ergonomic"
                    "SP001 SynthesizeParamObjects.ParamObjectSynthesized ergonomic"
                    "DO001 DedupeOverloads.OverloadDropped widened"
                    "RA001 RepairArity.GenericAliasDropped widened"
                    "RA002 RepairArity.ReferenceToDroppedAlias widened"
                    "RA003 RepairArity.GenericWithoutArguments widened"
                    "RA004 RepairArity.ArityMismatch widened"
                    "RA005 RepairArity.ReadWithoutWrite ergonomic"
                    "AC001 AuditCoverage.ExportNotRepresented escape" ]

              Expect.equal table expected "keys are positional: append new cases, never insert or reorder"

          testCase "a pass is labelled with the prefix of the union it owns" <| fun _ ->
              Expect.equal (FindingCatalogue.passLabel "shape-interfaces") "SI - shape-interfaces" "a pass with a union"
              Expect.equal (FindingCatalogue.passLabel "name-exports") "name-exports" "a pass without one stays bare"

              let passes = FindingCatalogue.passPrefixes |> Map.toList |> List.map fst
              Expect.equal passes.Length 14 "every per-pass union names its pass"

          testCase "a finding derives key, tier and message from its kind" <| fun _ ->
              let finding = Finding.make "Options.legacy" (TypeReference.UnionTooWide(5, 4))
              Expect.equal finding.Key "TR036" "key"
              Expect.equal finding.Tier Widened "tier"

              Expect.equal
                  finding.Message
                  "union of 5 distinct types widened to obj (D4 caps the erased union at 4)"
                  "message interpolates the payload" ]
