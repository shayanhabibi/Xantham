/// <summary>
/// Utility functions and types that make working with the xantham paths and
/// output easier.
/// </summary>
module Xantham.Decoder.Utils

open Xantham

let rec private getKeysFromMember (memberInfo: TsMember) =
    match memberInfo with
    | TsMember.Method glueMethod -> glueMethod.ToList() |> List.map _.Type
    | TsMember.Property glueProperty -> [ glueProperty.Type ]
    | TsMember.GetAccessor glueGetAccessor -> [ glueGetAccessor.Type ]
    | TsMember.SetAccessor glueSetAccessor -> [ glueSetAccessor.ArgumentType ]
    | TsMember.CallSignature glueCallSignature ->
        glueCallSignature.ToList()
        |> List.collect (fun glueCallSignature ->
            [
                glueCallSignature.Type
                yield! glueCallSignature.Parameters |> List.map _.Type
            ]
        )
    | TsMember.IndexSignature glueIndexSignature ->
        [
            glueIndexSignature.Type
            yield! glueIndexSignature.Parameters |> List.map _.Type
        ]
    | TsMember.ConstructSignature glueConstruct ->
        glueConstruct.ToList() |> List.collect (fun glueConstruct ->
            [
                glueConstruct.Type
                yield! glueConstruct.Parameters |> List.map _.Type
            ])

/// <summary>
/// <b>For Debugging Purposes Primarily:</b><br/>
/// Recursively collects all keys from a GlueType.
/// </summary>
let rec getKeys typ =
    match typ with
    | TsType.Array node -> getKeys node
    | TsType.GlobalThis -> []
    | TsType.Conditional glueConditionalType ->
        [
            glueConditionalType.Check
            glueConditionalType.Extends
            glueConditionalType.True
            glueConditionalType.False
        ]
    | TsType.Interface glueInterface -> [
        yield!
            glueInterface.Members
            |> List.collect getKeysFromMember
        yield! glueInterface.TypeParameters |> List.map fst
        yield!
            glueInterface.TypeParameters
            |> List.choose (snd >> _.Default)
        yield!
            glueInterface.TypeParameters
            |> List.choose (snd >> _.Constraint)
        yield!
            glueInterface.Heritage.Extends
            |> List.collect (fun heritage -> heritage.Type :: heritage.TypeArguments)
        ]
    | TsType.Class glueClass -> [
        yield!
            glueClass.Members
            |> List.collect getKeysFromMember
        yield! glueClass.TypeParameters |> List.map fst
        yield!
            glueClass.TypeParameters
            |> List.choose (snd >> _.Default)
        yield!
            glueClass.TypeParameters
            |> List.choose (snd >> _.Constraint)
        yield!
            glueClass.Heritage.Extends
            |> List.collect (fun heritage -> heritage.Type :: heritage.TypeArguments)
        match glueClass.Heritage.Implements with
        | Some value ->
            value.Type
            yield! value.TypeArguments
        | _ -> ()
        yield!
            glueClass.Constructors
            |> List.collect (_.Parameters >> List.map _.Type)
        ]
    | TsType.Variable glueVariable -> [ glueVariable.Type ]
    | TsType.Primitive _ -> []
    | TsType.Enum _ -> []
    | TsType.TypeAlias glueTypeAlias -> [
            glueTypeAlias.Type
            yield!
                glueTypeAlias.TypeParameters
                |> List.collect (function
                    typ,{ Constraint = c; Default = d } ->
                        [
                            typ
                            match c with Some v -> v | _ -> ()
                            match d with Some v -> v | _ -> ()
                        ])
        ]
    | TsType.Function glueFunctionDeclaration -> glueFunctionDeclaration.ToList() |> List.collect (fun glueFunctionDeclaration ->
        [
            glueFunctionDeclaration.Type
            yield!
                glueFunctionDeclaration.Parameters
                |> List.map _.Type
            yield!
                glueFunctionDeclaration.TypeParameters
                |> List.collect (function
                    typ,{ Constraint = c; Default = d } ->
                        [
                            typ
                            match c with Some v -> v | _ -> ()
                            match d with Some v -> v | _ -> ()
                        ])
        ])
    | TsType.Intersection (TsTypeIntersection values) 
    | TsType.Union (TsTypeUnion values) -> values
    | TsType.Literal _ -> []
    | TsType.IndexedAccess glueIndexAccessType ->
        [
            glueIndexAccessType.Object
            glueIndexAccessType.Index
        ]
    | TsType.Module glueModuleDeclaration -> glueModuleDeclaration.Types
    | TsType.TypeReference glueTypeReference ->
        [
            glueTypeReference.Type
            yield!
                glueTypeReference.TypeArguments
        ]
    | TsType.TypeParameter glueTypeParameter ->
        [
            match glueTypeParameter.Constraint with Some v -> v | _ -> ()
            match glueTypeParameter.Default with Some v -> v | _ -> ()
        ]
    | TsType.ReadOnly glueType -> getKeys glueType
    | TsType.Tuple glueTuple ->
        glueTuple.Types
        |> List.map (function
            | TsTupleElement.Variadic t
            | TsTupleElement.FixedLabeled(_, { Type = t })
            | TsTupleElement.Fixed { Type = t } -> t
            )
    | TsType.Index glueIndex -> glueIndex.Type |> List.singleton
    | TsType.Predicate glueTypePredicate -> glueTypePredicate.Type |> List.singleton
    | TsType.TypeLiteral glueTypeLiteral -> glueTypeLiteral.Members |> List.collect getKeysFromMember
    | TsType.EnumCase _ -> []
    | TsType.TemplateLiteral tsTemplateLiteralType -> tsTemplateLiteralType.Types
    | TsType.Optional tsTypeReference -> getKeys (TsType.TypeReference tsTypeReference)