module Xantham.Generator.Shape.Classes

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The F# name an already-shaped member answers to, for the collision test below. An indexer is
/// spelled `Item` (§4.10), which is a name a static could carry too.
let private shapedMemberName (m: FsMember) =
    match m with
    | FsProperty p -> p.Name
    | FsMethod m -> m.Name
    | FsIndexer _ -> "Item"
    | FsConstructor _ -> "Create"

/// Whether F# admits a static beside an instance member of the same name. Only between two
/// methods (`Response.json` is both): property over property is FS0441, method over property
/// FS0434, and a static property under an abstract method is FS3214 at every use.
let private staticFitsBeside (instance: FsMember) (isMethod: bool) =
    match instance with
    | FsMethod _ -> isMethod
    | FsProperty _
    | FsIndexer _
    | FsConstructor _ -> false

/// A class's static, bound through a dotted selector off whatever the class itself binds to:
/// `[<Import("Counter.MAX", "pkg")>]` is `import { Counter }` then `Counter.MAX`, and
/// `[<Global("Gadget.SPEED")>]` is the bare `Gadget.SPEED`; `default.MAX` reads off the default.
let private staticBinding (binding: ImportBinding) (key: string) =
    match binding with
    | ImportDefault -> ImportNamed $"{Naming.defaultImportKey}.{key}"
    | ImportNamed name -> ImportNamed $"{name}.{key}"
    | GlobalName name -> GlobalName $"{name}.{key}"
    | ImportFrom(name, specifier) -> ImportFrom($"{name}.{key}", specifier)

/// Constructor members on `Exports` for exported classes: `Exports.Name(...)` is
/// `new Name(...)` through `[<EmitConstructor>]` (§4.4). The same pass shapes the class's
/// *statics* onto its own interface, so a consumer writes `Counter.MAX` as TypeScript does.
let shapeClasses: Pass<ShapeModel> =
    {
        Name = "shape-classes"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []
                    let emit finding = findings <- findings @ [ finding ]
                    let fallback = defaultExportName ctx

                    // Statics sit on the class's own declaration, so the pass has to know which
                    // interfaces the shaping tier already emitted and what names they carry.
                    let declared =
                        model.Decls
                        |> List.choose (function
                            | FsInterface decl ->
                                Some(decl.Name, decl.Members |> List.map shapedMemberName |> Set.ofList)
                            | _ -> None)
                        |> Map.ofList

                    let mutable statics: Map<string, FsExportMember list> = Map.empty

                    /// One static, shaped: a method where the checker says so and it has a
                    /// signature, a property otherwise - settable where TypeScript declares it
                    /// assignable, get-only where it declares it `readonly`.
                    let shapeStatic (export: HarvestedExport) (name: string) (m: ResolvedMember) =
                        let key = Naming.memberName m.Symbol.Name
                        let owner = $"{name}.{key}"
                        let binding = staticBinding (bindingOf export) key

                        let asMethod =
                            if not (hasAny SymbolFlags.Method m.Symbol.Flags) then
                                None
                            else
                                match Map.tryFind m.TypeId model.Types with
                                | Some memberFacts when not memberFacts.CallSignatures.IsEmpty -> Some memberFacts
                                | _ -> None

                        let collides =
                            match Map.tryFind name declared with
                            | None ->
                                // No instance members, so no declaration to hang a static on.
                                emit (Finding.make owner ShapeClasses.StaticWithoutDeclaration)
                                true
                            | Some names when Set.contains key names ->
                                let instance =
                                    model.Decls
                                    |> List.tryPick (function
                                        | FsInterface decl when decl.Name = name ->
                                            decl.Members |> List.tryFind (shapedMemberName >> (=) key)
                                        | _ -> None)

                                match instance with
                                | Some instance when staticFitsBeside instance asMethod.IsSome -> false
                                | _ ->
                                    emit (Finding.make owner ShapeClasses.StaticMemberDropped)
                                    true
                            | Some _ -> false

                        if collides then
                            []
                        else
                            match asMethod with
                            | Some memberFacts ->
                                memberFacts.CallSignatures
                                |> List.map (fun signature ->
                                    let typeParameters, parameters, returns, signatureFindings =
                                        shapeSignature ctx model (Some name) owner signature

                                    findings <- findings @ signatureFindings

                                    {
                                        Name = key
                                        Docs = m.Docs
                                        Tags = m.Tags
                                        TypeParameters = typeParameters
                                        Binding = binding
                                        Body = ExportFunction(parameters, returns)
                                        Settable = false
                                    })
                            | None ->
                                let reference, refFindings = typeRef ctx model (Some name) owner m.TypeId
                                findings <- findings @ refFindings

                                if m.Optional then
                                    emit (Finding.make owner Members.OptionalMemberAsOption)

                                // A settable static binds through the class itself, and the
                                // attribute goes on the declaration - the placement under which
                                // Fable compiles `Counter.tick <- 8.0` to `Counter.tick = 8`.
                                let settable = not (hasAny SymbolFlags.Method m.Symbol.Flags) && not m.ReadOnly

                                if hasAny SymbolFlags.Method m.Symbol.Flags then
                                    // A method the checker gave no call signatures: its type is
                                    // declared in a group resolved identity-only, so there is
                                    // nothing to shape a method from.
                                    let declaredIn =
                                        Map.tryFind m.TypeId model.Types
                                        |> Option.bind (fun facts -> GeneratorConfig.groupKey facts.Origin)
                                        |> Option.defaultValue "another group"

                                    emit (Finding.make owner (ShapeClasses.StaticMethodWithoutSignatures declaredIn))
                                elif settable then
                                    emit (Finding.make owner ShapeClasses.StaticSettable)

                                [
                                    {
                                        Name = key
                                        Docs = m.Docs
                                        Tags = m.Tags
                                        TypeParameters = []
                                        Binding = (if settable then bindingOf export else binding)
                                        Body = ExportValue(optionalRef m.Optional reference)
                                        Settable = settable
                                    }
                                ]

                    let members =
                        model.Harvest.Exports
                        |> List.indexed
                        |> List.collect (fun (index, export) ->
                            if not (hasAny SymbolFlags.Class export.Symbol.Flags) then
                                []
                            else
                                let name = fsName fallback export

                                let valueFacts =
                                    Map.tryFind export.Symbol.Id model.ExportTypes
                                    |> Option.bind _.Value
                                    |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                                match valueFacts with
                                | None ->
                                    findings <- findings @ [ Finding.make name ShapeClasses.ClassWithoutValueType ]

                                    []
                                | Some facts ->
                                    // `prototype` is the instance side, which the shaping tier
                                    // already declared; a symbol-keyed static is unrepresentable
                                    // for the same reason an instance one is (§4.14).
                                    let shaped =
                                        facts.Members
                                        |> List.filter (fun m ->
                                            if m.Symbol.Name = "prototype" then
                                                false
                                            elif isSymbolKeyed m.Symbol.Name then
                                                let stable = m.Symbol.Name.Substring(0, m.Symbol.Name.LastIndexOf '@')

                                                emit (
                                                    Finding.make $"{name}.{stable}" Members.SymbolKeyedMemberDropped
                                                )

                                                false
                                            else
                                                true)
                                        |> List.collect (shapeStatic export name)

                                    if not shaped.IsEmpty then
                                        statics <-
                                            Map.add
                                                name
                                                ((Map.tryFind name statics |> Option.defaultValue []) @ shaped)
                                                statics

                                    facts.ConstructSignatures
                                    |> List.map (fun signature ->
                                        let typeParameters, parameters, returns, signatureFindings =
                                            shapeSignature ctx model (Some name) name signature

                                        findings <- findings @ signatureFindings

                                        index,
                                        {
                                            Name = name
                                            Docs = export.Docs
                                            Tags = export.Tags
                                            TypeParameters = typeParameters
                                            Binding = bindingOf export
                                            Body = ExportConstructor(parameters, returns)
                                            Settable = false
                                        }))

                    let decls =
                        model.Decls
                        |> List.map (function
                            | FsInterface decl when (Map.containsKey decl.Name statics) ->
                                FsInterface
                                    { decl with
                                        Statics = decl.Statics @ Map.find decl.Name statics
                                    }
                            | other -> other)

                    let model =
                        { model with
                            Decls = decls
                            ExportMembers = model.ExportMembers @ members
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
