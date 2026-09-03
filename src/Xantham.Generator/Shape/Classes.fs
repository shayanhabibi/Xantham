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

/// The closed vocabulary `SC008` reports, so a corpus aggregates by reason.
module private Refusal =
    [<Literal>]
    let NoDeclaration = "the run emits no interface under the class's name"

    [<Literal>]
    let FreeTypeParameter =
        "the constructor binds a type parameter the declaration's head does not"

    [<Literal>]
    let InheritedBase =
        "the class inherits a base whose constructor arguments have no F# form"

/// The type variables a reference mentions. A primary constructor is written under the
/// declaration's own head, so its parameters may name those variables and no others.
let rec private typeVars (reference: FsTypeRef) =
    match reference with
    | FsTypeVar name -> Set.singleton name
    | FsOption inner
    | FsArray inner
    | FsBranded(inner, _) -> typeVars inner
    | FsTuple parts
    | FsErasedUnion parts
    | FsApp(_, parts) -> parts |> List.fold (fun found part -> Set.union found (typeVars part)) Set.empty
    | FsDelegate(parameters, returns) ->
        returns :: parameters
        |> List.fold (fun found part -> Set.union found (typeVars part)) Set.empty
    | _ -> Set.empty

/// A class an ambient module exports for consumers to derive from: `abstract`, or carrying a
/// base of its own. F# admits no `inherit` of an interface (FS0946), so this is the one shape
/// that reaches a consumer's `type Actor(ctx, env) = inherit DurableObject(ctx, env)`. Every
/// other class keeps the interface form, where the `[<ParamObject>]` Create is the construction
/// a consumer wants.
let private isEntrypoint (export: HarvestedExport) (constructSignatures: ResolvedSignature list) (bases: int list) =
    match export.Origin with
    | FromAmbientModule _ -> (constructSignatures |> List.exists _.IsAbstract) || not bases.IsEmpty
    | FromGlobal
    | FromModule -> false

/// Constructor members on `Exports` for exported classes: `Exports.Name(...)` is
/// `new Name(...)` through `[<EmitConstructor>]` (§4.4). The same pass shapes the class's
/// *statics* onto its own interface, so a consumer writes `Counter.MAX` as TypeScript does, and
/// converts an ambient module's entrypoint classes to the `[<AbstractClass>]` form.
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

                    // The declarations that convert to the class form, by name.
                    let mutable entrypoints: Map<string, FsEntrypoint> = Map.empty

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

                    /// One class in the entrypoint form: the declaration's own head, the
                    /// parameters of its first construct signature, and the import that binds the
                    /// JavaScript constructor. Refused where F# would not admit the result, and
                    /// the declaration then keeps the interface form it already has.
                    let admitEntrypoint (export: HarvestedExport) (facts: TypeFacts) (name: string) =
                        let declaration =
                            model.Decls
                            |> List.tryPick (function
                                | FsInterface decl when decl.Name = name -> Some decl
                                | _ -> None)

                        let refuse reason =
                            emit (Finding.make name (ShapeClasses.EntrypointClassRefused reason))

                        match declaration with
                        | None -> refuse Refusal.NoDeclaration
                        // An F# class reaches its base through a constructor call, and a base
                        // this run declares is an interface with none. The interface form keeps
                        // the is-a relation the `inherit` line already carries.
                        | Some declaration when not declaration.Inherits.IsEmpty -> refuse Refusal.InheritedBase
                        | Some declaration ->
                            // The constructor overloads beyond the first have no F# form: a
                            // primary constructor takes one parameter list. `Exports` still
                            // carries every one of them under `[<EmitConstructor>]`.
                            let parameters =
                                match facts.ConstructSignatures with
                                | [] -> []
                                | signature :: _ ->
                                    let _, parameters, _, _ = shapeSignature ctx model (Some name) name signature
                                    parameters

                            let bound = declaration.TypeParameters |> List.map _.Name |> Set.ofList

                            let free =
                                parameters
                                |> List.fold (fun found p -> Set.union found (typeVars p.Type)) Set.empty
                                |> fun mentioned -> Set.difference mentioned bound

                            if not free.IsEmpty then
                                refuse Refusal.FreeTypeParameter
                            else
                                let specifier =
                                    match export.Origin with
                                    | FromAmbientModule specifier -> specifier
                                    | FromGlobal
                                    | FromModule -> ""

                                entrypoints <-
                                    Map.add
                                        name
                                        {
                                            Binding = bindingOf export
                                            Parameters = parameters
                                        }
                                        entrypoints

                                emit (Finding.make name (ShapeClasses.EntrypointClassEmitted specifier))

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

                                    let declaredId =
                                        Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared

                                    let bases =
                                        declaredId
                                        |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)
                                        |> Option.map _.BaseTypes
                                        |> Option.defaultValue []

                                    if isEntrypoint export facts.ConstructSignatures bases then
                                        // The name the *instance* side is declared under, which a
                                        // clash renames: `cloudflare:workers`'s `DurableObject`
                                        // class is `DurableObject2` beside the global interface of
                                        // that name, and the class form belongs to the class.
                                        let declaredName =
                                            declaredId
                                            |> Option.bind (fun typeId -> Map.tryFind typeId model.DeclNames)

                                        admitEntrypoint export facts (Option.defaultValue name declaredName)

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
                            | FsInterface decl ->
                                FsInterface
                                    { decl with
                                        Statics =
                                            decl.Statics @ (Map.tryFind decl.Name statics |> Option.defaultValue [])
                                        Entrypoint = Map.tryFind decl.Name entrypoints
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
