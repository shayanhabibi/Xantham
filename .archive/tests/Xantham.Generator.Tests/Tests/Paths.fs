module Xantham.Generator.Tests.Tests.Paths

open Expecto
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham
open Xantham.Decoder

[<Tests>]
let tests =
    testList "Relative Anchor Paths" [
        testCase "Ref type in parent module" <| fun _ ->
            let barPath =
                ModulePath.createFromList [ "Root"; "Foo" ]
                |> TypePath.create "Bar"
            let localPath =
                ModulePath.createFromList [ "Root"; "Foo"; "Nested" ]
                |> TypePath.create "Local"
                |> MemberPath.createOnType "localProp"
                |> ParameterPath.create "para"
                |> AnchorPath.Parameter
            Path.getRelativePath barPath localPath
            |> List.map Name.Case.valueOrModified
            |> Flip.Expect.equal "" [ "Bar" ]
        testCase "Ref type from shared module" <| fun _ ->
            let sharedPath = ModulePath.createFromList [ "Shared"; "Ancestor" ]
            let targetPath =
                sharedPath
                |> ModulePath.create "TargetModule"
                |> TypePath.create "TargetType"
            let localPath =
                sharedPath
                |> ModulePath.create "LocalModule"
                |> MemberPath.createOnModule "localFunction"
                |> ParameterPath.create "refParameter"
                |> AnchorPath.Parameter
            Path.getRelativePath targetPath localPath
            |> List.map Name.Case.valueOrModified
            |> Flip.Expect.equal "" [ "TargetModule"; "TargetType" ]
        testCase "No shared module" <| fun _ ->
            let targetPath =
                ModulePath.createFromList [ "TargetRoot"; "TargetModule" ]
                |> TypePath.create "TargetType"
            let localPath =
                ModulePath.createFromList [ "LocalRoot"; "LocalModule" ]
                |> MemberPath.createOnModule "localFunction"
                |> AnchorPath.Member
            Path.getRelativePath targetPath localPath
            |> List.map Name.Case.valueOrModified
            |> Flip.Expect.equal "" [ "TargetRoot"; "TargetModule"; "TargetType" ]
        testCase "Ref type from nested module" <| fun _ ->
            let targetPath =
                ModulePath.createFromList [ "Root"; "Module"; "LocalModule"; "TargetModule" ]
                |> TypePath.create "TargetType"
            let localPath =
                ModulePath.createFromList [ "Root"; "Module"; "LocalModule" ]
                |> TypePath.create "LocalType"
                |> AnchorPath.Type
            Path.getRelativePath targetPath localPath
            |> List.map Name.Case.valueOrModified
            |> Flip.Expect.equal "" [ "TargetModule"; "TargetType" ]
    ]
    
[<Tests>]
let tests2 = testList "Transient Path Interactions" [
    testCase "T" <| fun _ ->
        let anchorPath =
            ModulePath.createFromList [ "Root"; "Anchor" ]
            |> TypePath.create "Foo"
            |> MemberPath.createOnType "bar"
            |> ParameterPath.create "options"
            |> AnchorPath.Parameter
        let transientParameterType = TransientTypePath.Anchored
        let transientMemberPath =
            TransientMemberPath.createOnTransientType "optionProp" transientParameterType
        let subTransientType = TransientTypePath.Anchored
        let subTransientMember =
            TransientMemberPath.createOnTransientType "subOptionProp" subTransientType
        TransientTypePath.anchor anchorPath transientParameterType
        |> AnchorPath.Type
        |> TransientMemberPath.anchor
        |> funApply transientMemberPath
        |> AnchorPath.Member
        |> TransientTypePath.anchor
        |> funApply subTransientType
        |> AnchorPath.Type
        |> TransientMemberPath.anchor
        |> funApply subTransientMember
        |> AnchorPath.Member
        |> TransientTypePath.anchor
        |> funApply TransientTypePath.Anchored
        |> TypePath.flatten
        |> List.map Name.Case.valueOrModified
        |> Flip.Expect.equal "" [ "Root"; "Anchor"; "Foo"; "Bar"; "Options"; "OptionProp"; "SubOptionProp" ]
        
]