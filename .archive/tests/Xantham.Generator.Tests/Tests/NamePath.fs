module Xantham.Generator.Tests.Tests.NamePath

open Expecto
open Xantham.Decoder
open Xantham.Generator.NamePath

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
    
    // type alias body: type Type = { ... }  →  the body IS the type, not a child of it
    pathCe { _module "Root"; _type "Type" } ==> "Root.Type"
    
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
    // Named (AnchoredAndMoored) chains correctly extend the module path at each step
    pathCe { _module "Root"; _type "Bar"; asAnchorPath }
    --> pathCe { _type_ "Foo" } --> pathCe { _type_ "Bar" } ==> "Root.Bar.Foo.Bar" <== _.IsType

    // Unnamed (Anchored) transient on a type anchor collapses to the anchor itself — chaining is idempotent
    pathCe { _module "Root"; _type "Foo"; asAnchorPath }
    --> pathCe { _type_ } --> pathCe { _type_ } ==> "Root.Foo" <== _.IsType

    // Mixed: unnamed step collapses, subsequent named step is relative to the (collapsed) anchor
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _type_ } --> pathCe { _type_ "D" } ==> "A.B.D" <== _.IsType

    // Mixed: named step first, then unnamed collapses to it
    pathCe { _module "A"; _type "B"; asAnchorPath }
    --> pathCe { _type_ "C" } --> pathCe { _type_ } ==> "A.B.C" <== _.IsType
    
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
        --> pathCe { _type_ } // Root.Type
        =-= pathCe { _module "Root"; _type "Type" }
        ==> "Type"
        
        pathCe { _module "Root"; _type "Type"; _member "op"; asAnchorPath }
        --> pathCe { _type_ } // Root.Type.Op
        =-= pathCe { _module "Root"; _module "Bar"; _module "Root"; _type "Type" }
        ==> "Bar.Root.Type"
    ] <| fun (actual, expected) ->
        Expect.equal actual expected ""
]