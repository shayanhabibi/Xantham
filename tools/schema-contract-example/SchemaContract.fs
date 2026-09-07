namespace SchemaContractExample

open System
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Data.JsonSchema
open FSharp.Data.JsonSchema.Core
open NJsonSchema

/// Example serialization-policy adapter over generated schemas. No config field,
/// union case name, description, or JSON schema document is transcribed here.
module SchemaContract =
    let private isGeneric definition (ty: Type) =
        ty.IsGenericType && ty.GetGenericTypeDefinition() = definition

    let generate (rootType: Type) : JsonSchema =
        let generateSchema = Generator.Create(unionEncoding = UnionEncodingStyle.Untagged)

        let coreConfig =
            { SchemaGeneratorConfig.defaults with
                UnionEncoding = UnionEncodingStyle.Untagged
            }

        let name = coreConfig.PropertyNamingPolicy
        let visited = HashSet<Type * JsonSchema>()

        // Optional means absent in this contract. Removing null does not make a
        // field required. The current library emits nullable record options.
        let omitNull (schema: JsonSchema) =
            schema.Type <- schema.Type &&& (~~~JsonObjectType.Null)

            let nullBranches =
                schema.OneOf
                |> Seq.filter (fun s -> s.Type = JsonObjectType.Null)
                |> Seq.toArray

            for branch in nullBranches do
                schema.OneOf.Remove branch |> ignore

            if schema.OneOf.Count = 1 then
                Seq.head schema.OneOf
            else
                schema

        let rec adapt (ty: Type) (node: JsonSchema) =
            let schema = node.ActualSchema

            if visited.Add(ty, schema) then
                if isGeneric typedefof<option<_>> ty then
                    adapt (ty.GetGenericArguments()[0]) (omitNull schema)
                elif FSharpType.IsRecord ty then
                    // Shayan's analyzer already reads DescriptionAttribute. The
                    // pinned NJsonSchema translator does not copy that metadata.
                    let metadata = SchemaAnalyzer.analyze coreConfig ty

                    match metadata.Root with
                    | SchemaNode.Object record ->
                        schema.Description <- Option.toObj record.Description

                        for field in FSharpType.GetRecordFields ty do
                            let key = name field.Name
                            let property = schema.Properties[key]

                            property.Description <-
                                record.Properties
                                |> List.find (fun p -> p.Name = key)
                                |> _.Description
                                |> Option.toObj

                            adapt field.PropertyType property
                    | _ -> invalidOp $"Expected generated record metadata for {ty.FullName}"
                elif isGeneric typedefof<Map<_, _>> ty then
                    if ty.GetGenericArguments()[0] <> typeof<string> then
                        invalidOp "This example supports string-keyed JSON maps."

                    let valueType = ty.GetGenericArguments()[1]
                    // The pinned translator loses AdditionalPropertiesSchema
                    // when copying a map definition. Regenerate that child using
                    // the library, rather than admitting arbitrary map values.
                    if isNull schema.AdditionalPropertiesSchema then
                        schema.AdditionalPropertiesSchema <- generateSchema valueType

                    adapt valueType schema.AdditionalPropertiesSchema
                elif isGeneric typedefof<list<_>> ty || ty.IsArray then
                    let element =
                        if ty.IsArray then
                            ty.GetElementType()
                        else
                            ty.GetGenericArguments()[0]

                    adapt element schema.Item
                elif FSharpType.IsUnion ty then
                    let cases = FSharpType.GetUnionCases ty

                    if cases |> Array.forall (fun c -> c.GetFields().Length = 0) then
                        schema.Enumeration.Clear()
                        schema.EnumerationNames.Clear()

                        for case in cases do
                            schema.Enumeration.Add(name case.Name)
                            schema.EnumerationNames.Add(name case.Name)
                    else
                        // The library generates untagged case objects. This
                        // contract unwraps their single payload: string | object,
                        // for example. Other case shapes need a declared policy.
                        let branches = schema.AnyOf |> Seq.toArray

                        if branches.Length <> cases.Length then
                            invalidOp $"Unexpected generated union shape for {ty.FullName}"

                        let payloads =
                            Array.map2
                                (fun (case: UnionCaseInfo) (branch: JsonSchema) ->
                                    let fields = case.GetFields()

                                    if fields.Length <> 1 then
                                        invalidOp $"{ty.FullName}.{case.Name} must carry one JSON value."

                                    let payload = branch.ActualSchema.Properties[name fields[0].Name]
                                    adapt fields[0].PropertyType payload
                                    JsonSchema(Reference = payload))
                                cases
                                branches

                        schema.AnyOf.Clear()

                        for payload in payloads do
                            schema.AnyOf.Add payload

        let schema = generateSchema rootType
        adapt rootType schema
        schema
