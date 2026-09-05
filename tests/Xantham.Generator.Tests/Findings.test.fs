/// The finding catalogue's two invariants the compiler cannot hold: every case carries a row in
/// `FindingCodes.table`, and the pairing of name to code stays where it is. This snapshot is the
/// guard - a renamed case or a moved code fails here, not silently in every golden manifest.
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
        [ testCase "every finding name and code is unique" <| fun _ ->
              let names = FindingCatalogue.table () |> List.map (fun (name, _, _, _) -> name)
              Expect.equal (List.distinct names) names "no two cases share a name"

              let codes = FindingCatalogue.table () |> List.map (fun (_, code, _, _) -> code)
              Expect.equal (List.distinct codes) codes "no two cases share a code"

              let prefixes = FindingCatalogue.unions |> List.map (fun t -> (Coder.forType t).Prefix)
              Expect.equal (List.distinct prefixes) prefixes "no two unions share a prefix"

          testCase "the committed table codes each name once, under its own prefix" <| fun _ ->
              let named = FindingCodes.table |> List.map fst
              Expect.equal (List.distinct named) named "no name is coded twice"

              let codes = FindingCodes.table |> List.map snd
              Expect.equal (List.distinct codes) codes "a retired case keeps its row, so no code is handed out twice"

              for name, code in FindingCodes.table do
                  let prefix = name.Substring(0, name.IndexOf '.')
                  Expect.stringStarts code prefix $"{code} sits under {name}'s own prefix"

          testCase "every finding's name and code are pinned" <| fun _ ->
              let table =
                  FindingCatalogue.table ()
                  |> List.map (fun (name, code, _, tier) -> $"{name} {code} {tierLabel tier}")

              let expected =
                  [
                    "TR.SelfReferenceThroughUnnamed TR001 widened"
                    "TR.TypeNotResolved TR002 widened"
                    "TR.MissingFromTypeTable TR003 escape"
                    "TR.LoneEnumMemberToFloat TR004 widened"
                    "TR.LoneEnumMemberToString TR005 widened"
                    "TR.StringLiteralToString TR006 widened"
                    "TR.NumericLiteralToFloat TR007 widened"
                    "TR.AnyToObj TR008 escape"
                    "TR.UnknownToObj TR009 widened"
                    "TR.PolymorphicThisAsDeclaringType TR010 ergonomic"
                    "TR.ThisOutsideDeclaration TR011 widened"
                    "TR.TypeParameterOutOfScopeToConstraint TR012 widened"
                    "TR.TypeParameterOutOfScope TR013 widened"
                    "TR.TypeFlagsNotMapped TR014 widened"
                    "TR.KeyOfOpenOperand TR015 ergonomic"
                    "TR.KeyOfOperandOutOfScope TR016 widened"
                    "TR.UnnamedBrandToPrimitive TR017 ergonomic"
                    "TR.IntersectionOverNonObject TR018 widened"
                    "TR.IntersectionNotDeclared TR019 widened"
                    "TR.IndexedAccessNoForm TR020 widened"
                    "TR.AnonymousInReferencedGroup TR021 widened"
                    "TR.GlobalThisToObj TR022 widened"
                    "TR.NotAmongGeneratedDeclarations TR023 widened"
                    "TR.LibExtraTypeArgumentsDropped TR024 ergonomic"
                    "TR.LibBindingLoss TR025 ergonomic"
                    "TR.ConstrainedArgumentWidened TR026 widened"
                    "TR.ArgumentNotBoundWithConstraint TR027 widened"
                    "TR.TupleRestToArray TR028 widened"
                    "TR.TupleArityNoForm TR029 widened"
                    "TR.CallableWithoutSignatures TR030 widened"
                    "TR.CallbackOverloadsFromFirst TR031 widened"
                    "TR.NullableHoistedToOption TR032 ergonomic"
                    "TR.OnlyNullUndefinedToUnit TR033 widened"
                    "TR.EmptyUnionToObj TR034 widened"
                    "TR.UnionWithObjArm TR035 widened"
                    "TR.UnionTooWide TR036 widened"
                    "TR.TemplateLiteralToString TR037 widened"
                    "TR.StringMappingToString TR038 widened"
                    "TR.BigIntLiteralToBigInt TR039 widened"
                    "TR.ObjectTypeToObj TR040 widened"
                    "TR.SymbolNoBinding TR041 widened"
                    "TR.UniqueSymbolNoBinding TR042 widened"
                    "TR.ConstructorObjectNotDeclared TR043 widened"
                    "TR.ArgumentNotASubtypeOfConstraint TR044 widened"
                    "TR.ConditionalTypeDeferred TR045 widened"
                    "TR.ConditionalResolvedToBranch TR046 ergonomic"
                    "TR.ObjectWithoutMembers TR047 widened"
                    "TR.ArrayIntersectionMembersDropped TR048 widened"
                    "TR.EmptyIntersectionOperandReduced TR049 exact"
                    "TR.IntersectionCallableFlattened TR050 ergonomic"
                    "TR.IntersectionOperandsIdentical TR051 exact"
                    "TR.AnonymousInMappedGroup TR052 widened"
                    "TR.MappedNameArityMismatch TR053 widened"
                    "TR.ReferencedArityUnconfirmed TR054 escape"
                    "TR.CallbackKeptAsDelegate TR055 ergonomic"
                    "TR.StringLiteralKeptForOverload TR056 exact"
                    "TP.UnnamedTypeParameter TP001 widened"
                    "TP.ConstraintDropped TP002 ergonomic"
                    "TP.GenericFunctionHoisted TP003 ergonomic"
                    "TP.KeyWithIndexedAccess TP004 ergonomic"
                    "TP.KeyOverOperand TP005 ergonomic"
                    "TP.TypeParameterErased TP006 widened"
                    "TP.UnnamedTypeParametersCounted TP007 widened"
                    "TP.ConstraintNotProvenNominal TP008 ergonomic"
                    "TP.DuplicateTypeParameterCollapsed TP009 ergonomic"
                    "MB.OptionalParameterAsOption MB001 ergonomic"
                    "MB.SymbolKeyedMemberDropped MB002 widened"
                    "MB.OptionalMemberAsOption MB003 ergonomic"
                    "MB.IndexSignatureAsIndexer MB004 ergonomic"
                    "MB.OptionalHookAsInterface MB005 ergonomic"
                    "MB.OptionalParameterFromUnion MB006 ergonomic"
                    "HG.AmbientModuleDropped HG001 escape"
                    "HG.UnwritableGlobalDropped HG002 escape"
                    "HG.NothingHarvested HG003 escape"
                    "HG.AmbientModuleHarvested HG004 exact"
                    "HG.AmbientModuleWildcard HG005 escape"
                    "HG.NamespaceIsModuleBody HG006 exact"
                    "RE.FacetNotResolved RE001 escape"
                    "RT.FrontierNotResolved RT001 widened"
                    "RT.TypeNotResolved RT002 widened"
                    "LU.NonStringLiteralCase LU001 exact"
                    "DT.ArmNotPlainData DT001 ergonomic"
                    "DT.TaggedUnion DT002 exact"
                    "SY.InstantiationNamedOnce SY001 exact"
                    "SY.HoistArgumentsNotRecovered SY002 widened"
                    "SY.IntersectionOperandNotHoisted SY003 widened"
                    "SY.NameNestedUnderOwner SY004 exact"
                    "SY.NameSanitisedForIdentifier SY005 ergonomic"
                    "SI.HybridLosesCallSignatures SI001 widened"
                    "SI.BaseMembersFlattened SI002 ergonomic"
                    "SI.IntersectionFlattened SI003 ergonomic"
                    "SI.ConstructorObjectDeclared SI004 ergonomic"
                    "SI.BaseInherited SI005 exact"
                    "SI.BaseNotDeclaredHere SI006 ergonomic"
                    "SI.BaseWouldCycle SI007 ergonomic"
                    "SA.BrandAsMeasure SA001 ergonomic"
                    "SA.PhantomComputation SA002 widened"
                    "SA.AbbreviationNameTaken SA003 widened"
                    "SC.ClassWithoutValueType SC001 escape"
                    "SC.StaticMemberDropped SC002 widened"
                    "SC.StaticReadOnly SC003 widened"
                    "SC.StaticWithoutDeclaration SC004 widened"
                    "SC.StaticMethodWithoutSignatures SC005 widened"
                    "SC.StaticSettable SC006 exact"
                    "SC.EntrypointClassEmitted SC007 exact"
                    "SC.EntrypointClassRefused SC008 widened"
                    "SC.EntrypointClassInheritsExn SC009 ergonomic"
                    "SE.NoValueType SE001 escape"
                    "SE.RuntimeSpecifierDerived SE002 ergonomic"
                    "SE.MutableValueReadOnly SE003 widened"
                    "SP.ParamObjectSynthesized SP001 ergonomic"
                    "SP.MethodMemberAsCreateParameter SP002 ergonomic"
                    "SP.CreateNotSynthesized SP003 ergonomic"
                    "DO.OverloadDropped DO001 widened"
                    "DO.OverloadsDistinguishedByLiteral DO002 exact"
                    "RA.GenericAliasDropped RA001 widened"
                    "RA.ReferenceToDroppedAlias RA002 widened"
                    "RA.GenericWithoutArguments RA003 widened"
                    "RA.ArityMismatch RA004 widened"
                    "RA.ReadWithoutWrite RA005 ergonomic"
                    "RA.AliasKeptAsPhantom RA006 widened"
                    "AC.ExportNotRepresented AC001 escape"
                    "GE.GroupShipped GE001 exact"
                    "GE.ShippedGroupWithoutDeclarations GE002 widened"
                    "GE.GroupModuleCollision GE003 escape"
                    "GE.GroupModuleFromNamespace GE004 escape"
                  ]

              Expect.equal
                  table
                  expected
                  "a finding's key is its name: renaming a case, or moving a code in FindingCodes.table, moves the \
                   key a consumer holds"


          testCase "a pass is labelled with the prefix of the union it owns" <| fun _ ->
              Expect.equal (FindingCatalogue.passLabel "shape-interfaces") "SI - shape-interfaces" "a pass with a union"
              Expect.equal (FindingCatalogue.passLabel "name-exports") "name-exports" "a pass without one stays bare"

              let passes = FindingCatalogue.passPrefixes |> Map.toList |> List.map fst
              Expect.equal passes.Length 14 "every per-pass union names its pass"

          testCase "a finding derives name, key, tier, payload and message from its kind" <| fun _ ->
              let finding = Finding.make "Options.legacy" (TypeReference.UnionTooWide(5, 4))
              Expect.equal finding.Name "TR.UnionTooWide" "name"
              Expect.equal finding.Key "TR036" "key"
              Expect.equal finding.Tier Widened "tier"

              Expect.equal
                  (finding.Payload |> Array.map (fun (field, value) -> field, string value))
                  [| "arms", "5"; "cap", "4" |]
                  "the payload is the case's fields, under their declared names and in order"

              Expect.equal
                  finding.Message
                  "union of 5 distinct types widened to obj (D4 caps the erased union at 4)"
                  "message interpolates the payload"

          testCase "a case declaring no fields carries no payload" <| fun _ ->
              let finding = Finding.make "Options.legacy" TypeReference.AnyToObj
              Expect.isEmpty finding.Payload "no fields to report" ]
