module Xantham.Generator.Shape.ParamObjects

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Parameters beyond this stop being construction ergonomics: a Create this wide is unusable
/// at a call site, and each one is quadratic work for the F# typechecker.
[<Literal>]
let private CreateParameterBudget = 24

/// Construction ergonomics (D3, §4.4): every plain-data interface - properties only - gains a
/// `[<ParamObject; Emit("$0")>]` Create overload mirroring its members, required members
/// first, so consumers never hand-build objects.
let synthesizeParamObjects: Pass<ShapeModel> =
    {
        Name = "synthesize-paramobjects"
        Run =
            fun _ model ->
                async {
                    let mutable findings = []

                    let decls =
                        model.Decls
                        |> List.map (function
                            | FsInterface decl when
                                not decl.Members.IsEmpty
                                && decl.Members.Length <= CreateParameterBudget
                                && decl.Members
                                   |> List.forall (function
                                       | FsProperty _ -> true
                                       // An index signature has no name to bind a Create
                                       // parameter to, so a type carrying one is not plain data.
                                       // A constructor object already has the `Create` members
                                       // its construct signatures gave it (§4.4), and a
                                       // synthesized one would collide with them.
                                       | FsMethod _
                                       | FsConstructor _
                                       | FsIndexer _ -> false)
                                ->
                                let parameters =
                                    decl.Members
                                    |> List.map (function
                                        | FsProperty p ->
                                            let optional =
                                                match p.Type with
                                                | FsOption _ -> true
                                                | _ -> false

                                            {
                                                Name = p.Name
                                                Optional = optional
                                                Rest = false
                                                Type = p.Type
                                            }
                                        | FsMethod _
                                        | FsConstructor _
                                        | FsIndexer _ -> failwith "unreachable: filtered to properties")

                                let required, optional = parameters |> List.partition (fun p -> not p.Optional)

                                findings <-
                                    findings
                                    @ [ Finding.make decl.Name SynthesizeParamObjects.ParamObjectSynthesized ]

                                FsInterface
                                    { decl with
                                        CreateOverloads = [ required @ optional ]
                                    }
                            | decl -> decl)

                    let model = { model with Decls = decls }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
