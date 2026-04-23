module Xantham.Generator.Tests.Tests.NamePath

open Expecto
open Xantham.Decoder
open Xantham.Generator.NamePath

/// <summary>
/// Builder for constructing paths with easily identifiable syntax for testing.
/// <br/><br/>
/// All concrete paths are preceded by a single <c>_</c>, while transient paths are preceded and followed by <c>_</c>.
/// <br/>
/// Therefor making a concrete path would be:
/// <code>
/// _module "Root"; _module "Module"; _type "Type"
/// </code>
/// And a transient path would be:
/// <code>
/// _module_ "Root"; _type_ "Type"
/// </code>
/// If you do not pass a string to the transient builder, it will be anchored.
/// <code>
/// _type_ // TransientTypePath.Anchored
/// _type_ "Type" // TransientTypePath.AnchoredAndMoored "Type"
/// _module_ "Module"; _type_ "Type" // TransientTypePath.Moored(TransientModulePath.AnchoredAndMoored "Module", "Type")
/// </code>
/// </summary>
type PathBuilder() =
    member inline _.Yield(a: unit) = ()
    member inline _.Yield(n: ModulePath) = n
    member inline _.Yield(n: TypePath) = n
    member inline _.Yield(n: MemberPath) = n
    member inline _.Yield(n: ParameterPath) = n
    member inline _.Yield(n: TypeParamPath) = n
    member inline _.Yield(n: TransientTypePath) = n
    member inline _.Yield(n: TransientModulePath) = n
    member inline _.Yield(n: TransientMemberPath) = n
    member inline _.Yield(n: TransientParameterPath) = n
    member inline _.Yield(n: AnchorPath) = n
    member inline _.Yield(n: TransientPath) = n
    member inline _.Yield(n: Path) = n
    member inline _.Yield(n: string) = n
    member inline _.Combine(a, b: unit) = a
    member inline _.Zero() = ()
    member inline this.Combine(a: ModulePath, b: string) = ModulePath.create b a
    member inline this.Combine(a: string, b: string) = ModulePath.init a |> ModulePath.create b
    member inline this.Combine(a: TypePath, b: string) = MemberPath.createOnType b a
    member inline this.Combine(a: MemberPath, b: string) = ParameterPath.create b a
    member inline this.Combine(a: TypePath, b: Name<Case.typar>) = TypeParamPath.createOnType b a
    member inline this.Combine(a: MemberPath, b: Name<Case.typar>) = TypeParamPath.createOnMember b a
    member inline this.Combine(a: ParameterPath, b: Name<Case.typar>) = TypeParamPath.createOnParameter b a
    member inline this.Combine(a: TransientModulePath, b: string) = TransientModulePath.createOnTransientModule b a
    member inline this.Combine(a: TransientTypePath, b: string) = TransientMemberPath.createOnTransientType b a
    member inline this.Combine(a: TransientMemberPath, b: string) = TransientParameterPath.createOnTransientMember b a
    member inline this.Delay([<InlineIfLambda>] a) = a()
    member inline this.For(a: ModulePath, b: unit -> string) = this.Combine(a, b())
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: unit, b: string) = TransientModulePath.AnchoredAndMoored(Name.Pascal.create b)
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: TransientModulePath, b: string) = TransientModulePath.Moored(a, Name.Pascal.create b)
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: string, b: string) =
        TransientModulePath.AnchoredAndMoored(Name.Pascal.create a)
        |> TransientModulePath.createOnTransientModule b
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: unit) = TransientModulePath.Anchored
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: unit) = TransientTypePath.Anchored
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: TransientModulePath, b: string) = TransientTypePath.Moored(a, Name.Pascal.create b)
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: unit, b: string) = TransientTypePath.AnchoredAndMoored(Name.Pascal.create b)
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: unit) = TransientMemberPath.Anchored
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: TransientTypePath, b: string) = TransientMemberPath.Moored(a, Name.Camel.create b)
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: unit, b: string) = TransientMemberPath.AnchoredAndMoored(Name.Camel.create b)
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: unit) = TransientParameterPath.Anchored
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: TransientMemberPath, b: string) = TransientParameterPath.Moored(a, Name.Camel.create b)
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: unit, b: string) = TransientParameterPath.AnchoredAndMoored(Name.Camel.create b)
    [<CustomOperation "_module">]
    member inline _.MakeModule(a: unit, b: string) = ModulePath.init b
    [<CustomOperation "_module">]
    member inline _.MakeModule(a: ModulePath, b: string) = ModulePath.create b a
    [<CustomOperation "_type">]
    member inline _.MakeType(a: ModulePath, b: string) = TypePath.create b a
    [<CustomOperation "_member">]
    member inline _.MakeMember(a: TypePath, b: string) = MemberPath.createOnType b a
    [<CustomOperation "_member">]
    member inline _.MakeMember(a: ModulePath, b: string) = MemberPath.createOnModule b a
    [<CustomOperation "_parameter">]
    member inline _.MakeParameter(a: MemberPath, b: string) = ParameterPath.create b a
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: TypePath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: MemberPath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: ParameterPath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "asAnchorPath">]
    member inline _.AsAnchorPath(a) = AnchorPath.create a
    [<CustomOperation "asTransientPath">]
    member inline _.AsTransientPath(a) = TransientPath.create a
    [<CustomOperation "asPath">]
    member inline _.AsPath(a) = Path.create a
    member inline _.Run(a) = a
let pathCe = PathBuilder()

/// Anchors the transient like path on the right side to the anchor path on the left.
let inline (-->) a b = TransientPath.anchor a (TransientPath.create b)
/// Tuples the elements on the left and right side together
let inline (==>) a b = a,b
/// Applies an anchorpath predicate on the right side to the fst value of a tuple (anchor path) on the left side and tuples
/// the results together.
let inline (<==) (a,b) (c: AnchorPath -> bool) = a, b, c a

[<Tests>]
let pathTests = testList "Anchor" [
        testList "Transient" [
            testTheory "TransientTypeAnchor" [
    // TEST FIXTURE
    // `[CONCRETE PATH] ==> [TransientTypePath.Anchored STRING REPR]`
    let inline (==>) a b = TransientPath.anchor (AnchorPath.create a) (pathCe { _type_; asTransientPath }), b
    
    pathCe { _module "Root"; _module "Foo"; _type "Bar"; _member "op" } ==> "Root.Foo.Bar.Op"
    
    pathCe { _module "Root"; _type "Type"; _member "op"; _parameter "para" } ==> "Root.Type.Op.Para"
    
    pathCe { _module "Root"; _type "Type" } ==> "Root.Type.Type" // if a transient type is being opened by a concrete type, then we still nest it.
    
            ] <| fun (a,b) ->
                a
                |> AnchorPath.flattenCaseless
                |> List.map Name.Case.valueOrModified
                |> String.concat "."
                |> Flip.Expect.equal "" b
            testTheory "TransientTypeAnchorMoored" [
    // TEST FIXTURE
    // `[CONCRETE PATH]
    //  --> [TransientTypePath.AnchoredAndMoored name]
    // ==> [STRING REPR] <== [AnchorPath predicate]`
    
    // A moored type anchors directly to a module
    pathCe { _module "Root"; _module "Type"; asAnchorPath }
    --> pathCe { _type_ "Foo" } ==> "Root.Type.Foo" <== _.IsType
    
    // A moored type anchors to a module with the type name of the anchor
    pathCe { _module "Root"; _type "Foo"; asAnchorPath }
    --> pathCe { _type_ "Bar" } ==> "Root.Foo.Bar" <== _.IsType
    
    pathCe { _module "Root"; _type "Foo"; _member "member"; asAnchorPath }
    --> pathCe { _type_ "MType" } ==> "Root.Foo.Member.MType" <== _.IsType
            
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _module_ "T"; _type_ "C" } ==> "A.B.T.C" <== _.IsType
    
    pathCe { _module "AA"; _type "BB"; _member "MM"; asAnchorPath }
    --> pathCe { _module_ "TT"; _type_ "CC" } ==> "AA.BB.MM.TT.CC" <== _.IsType
            ] <| fun (a, b, c) ->
                a
                |> AnchorPath.flattenCaseless
                |> List.map Name.Case.valueOrModified
                |> String.concat "."
                |> Flip.Expect.equal "" b
                c
                |> Flip.Expect.isTrue ""
            testTheory "Chained TransientTypeAnchorMoored" [
                
    pathCe { _module "Root"; _type "Foo"; asAnchorPath }
    --> pathCe { _type_ } --> pathCe { _type_ } ==> "Root.Foo.Foo.Foo" <== _.IsType
    
    pathCe { _module "Root"; _type "Bar"; asAnchorPath }
    --> pathCe { _type_ "Foo" } --> pathCe { _type_ "Bar" } ==> "Root.Bar.Foo.Bar" <== _.IsType
    
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _type_ } --> pathCe { _type_ "C" } ==> "A.B.B.C" <== _.IsType
    
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _type_ "C" } --> pathCe { _type_ } ==> "A.B.C.C" <== _.IsType
    
            ] <| fun (a, b, c) ->
                a
                |> AnchorPath.flattenCaseless
                |> List.map Name.Case.valueOrModified
                |> String.concat "."
                |> Flip.Expect.equal "" b
                c
                |> Flip.Expect.isTrue ""
        
            testTheory "TransientMemberAnchor" [
    let inline (==>) a b = TransientPath.anchor (AnchorPath.create a) (pathCe { _member_; asTransientPath }), b
    
    pathCe { _module "Root"; _type "Foo" } ==> "Root.Foo.foo" <== _.IsMember
    
    pathCe { _module "Root"; _module "Foo"; _type "Bar"} ==> "Root.Foo.Bar.bar" <== _.IsMember
    
    pathCe { _module "Root"; _type "Foo"; _member "op" } ==> "Root.Foo.Op.op" <== _.IsMember
    
            ] <| fun (a, b, c) ->
                a
                |> AnchorPath.flattenCaseless
                |> List.map Name.Case.valueOrModified
                |> String.concat "."
                |> Flip.Expect.equal "" b
                c
                |> Flip.Expect.isTrue ""
            testTheory "TransientMemberAnchorMoored" [
                
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _member_ "op" } ==> "A.B.op" <== _.IsMember
    
    pathCe { _module "AA"; _module "BB"; _type "CC"; asAnchorPath }
    --> pathCe { _member_ "op" } ==> "AA.BB.CC.op" <== _.IsMember
    
    pathCe { _module "AA"; _type "BB"; _member "op"; _parameter "para"; asAnchorPath }
    --> pathCe { _member_ "op" } ==> "AA.BB.Op.Para.op" <== _.IsMember
    
            ] <| fun (a, b, c) ->
                a
                |> AnchorPath.flattenCaseless
                |> List.map Name.Case.valueOrModified
                |> String.concat "."
                |> Flip.Expect.equal "" b
                c
                |> Flip.Expect.isTrue ""
        ]
        
    ]

let inline (=-=) a b =
    Path.getRelativePath b a
    |> List.map Name.Case.valueOrModified
    |> String.concat "."

[<Tests>]
let relativePathTests = testList "Relative Path" [
    testTheory "Type - Type" [
        pathCe {
            _module "Root"; _type "Type"; _member "op"; asAnchorPath
        } =-= pathCe { _module "Root"; _type "Type" } ==> "Type"
        
        pathCe {
            _module "Module"; _type "Foo"; _member "op"; _parameter "para"; asAnchorPath
        } =-= pathCe { _module "Module"; _type "Foo" } ==> "Foo"
        
        pathCe {
            _module "RootA"; _module "Bar"; _module "Op"; _type "Para"; _member "para"; asAnchorPath
        } =-= pathCe { _module "RootA"; _type "Bar" } ==> "Bar"
        
        pathCe {
            _module "Root"; _type "Type"; _member "op"; asAnchorPath
        } =-= pathCe { _module "Module"; _type "Type" } ==> "Module.Type"
    ] <| fun (actual, expected) ->
        Expect.equal actual expected ""
    
    testTheory "Transient Type" [
        pathCe { _module "Root"; _type "Type"; asAnchorPath }
        --> pathCe { _type_ } // Root.Type.Type
        =-= pathCe { _module "Root"; _type "Type" }
        ==> "Type"
        
        pathCe { _module "Root"; _type "Type"; _member "op"; asAnchorPath }
        --> pathCe { _type_ } // Root.Type.Op
        =-= pathCe { _module "Root"; _module "Bar"; _module "Root"; _type "Type" }
        ==> "Bar.Root.Type"
    ] <| fun (actual, expected) ->
        Expect.equal actual expected ""
]