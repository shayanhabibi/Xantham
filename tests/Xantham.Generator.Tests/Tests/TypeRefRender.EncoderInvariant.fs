module Xantham.Generator.Tests.Tests.TypeRefRenderEncoderInvariant

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Mocking.ArenaInterner.ResolvedType
open Xantham.Generator.Generator
open Xantham.Generator.Types

let ctx = GeneratorContext.Empty

// TypeScript declaration merging can produce a TypeReference whose
// argument count doesn't match the inner type's declared type parameter
// count — e.g. lib.dom Body declares 0 type params but a downstream
// class `extends Body<X>` is authored against a merged extension. This
// shape really occurs in `agents` and `@cloudflare/workers-types` (35
// instances at the time of writing). Previously the generator raised
// EncoderInvariantViolation here and halted the entire run.
//
// The current behaviour is to align the args to the declared arity:
// truncate excess args, pad missing args with `obj`. That way the
// emitted F# is well-formed regardless of which direction the mismatch
// goes — a class with 2 declared params still gets a 2-arg application
// rather than an arg-less reference that wouldn't compile.

let private prerenderTypeRef typeRef =
    let scope = RenderScopeStore.create ()
    prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeRef)

let private prefixArgsCount (ref: TypeRefRender) =
    match ref.Kind with
    | TypeRefKind.Molecule (TypeRefMolecule.Prefix (_, args)) -> Some args.Length
    | _ -> None

[<Tests>]
let encoderInvariantTests =
    testList "TypeReference arity mismatch" [
        // args > declared > 0: truncate to declared count.
        // Was the existing #throws test's shape; with the strict raise
        // gone, the contract is "render with declared-many args".
        testCase "args=3, declared=2: truncates to 2-arg Prefix" <| fun _ ->
            let interface2Params =
                Interface.create "TestInterface"
                |> Interface.withTypeParameters [
                    TypeParameter.create "T"
                    TypeParameter.create "U"
                ]
                |> Interface.wrap
            let typeRef =
                TypeReference.create interface2Params
                |> TypeReference.withTypeArguments [
                    TypeParameter.create "A" |> TypeParameter.wrap
                    TypeParameter.create "B" |> TypeParameter.wrap
                    TypeParameter.create "C" |> TypeParameter.wrap
                ]
                |> TypeReference.wrap
            prerenderTypeRef typeRef
            |> prefixArgsCount
            |> Flip.Expect.equal "expected Prefix molecule with 2 args after truncation" (Some 2)

        // args > declared = 0 (Body case): truncate produces empty arg
        // list; we fall back to the inner type alone (no Prefix wrapper).
        testCase "args=1, declared=0 (Body case): renders inner type without Prefix" <| fun _ ->
            let body =
                Interface.create "Body"
                |> Interface.wrap
            let typeRef =
                TypeReference.create body
                |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
                |> TypeReference.wrap
            // No Prefix should be emitted — Body has no params, so the
            // inner type is rendered alone.
            prerenderTypeRef typeRef
            |> prefixArgsCount
            |> Flip.Expect.equal "expected no Prefix molecule (inner type alone)" None

        // args < declared: pad with obj to reach declared count.
        testCase "args=1, declared=2: pads with obj to 2-arg Prefix" <| fun _ ->
            let interface2Params =
                Interface.create "TestInterface"
                |> Interface.withTypeParameters [
                    TypeParameter.create "T"
                    TypeParameter.create "U"
                ]
                |> Interface.wrap
            let typeRef =
                TypeReference.create interface2Params
                |> TypeReference.withTypeArguments [
                    TypeParameter.create "A" |> TypeParameter.wrap
                ]
                |> TypeReference.wrap
            prerenderTypeRef typeRef
            |> prefixArgsCount
            |> Flip.Expect.equal "expected Prefix molecule with 2 args after padding" (Some 2)

        // Sanity: matching arity goes through the normal path.
        testCase "args=2, declared=2: renders 2-arg Prefix without warning" <| fun _ ->
            let interface2Params =
                Interface.create "TestInterface"
                |> Interface.withTypeParameters [
                    TypeParameter.create "T"
                    TypeParameter.create "U"
                ]
                |> Interface.wrap
            let typeRef =
                TypeReference.create interface2Params
                |> TypeReference.withTypeArguments [
                    TypeParameter.create "A" |> TypeParameter.wrap
                    TypeParameter.create "B" |> TypeParameter.wrap
                ]
                |> TypeReference.wrap
            prerenderTypeRef typeRef
            |> prefixArgsCount
            |> Flip.Expect.equal "expected Prefix molecule with 2 args" (Some 2)
    ]
