#nowarn FS0040
#nowarn FS0046

// ReSharper disable FSharpInterpolatedString
// ReSharper disable FSharpRedundantNameQualifier

[<AutoOpen>]
module Xantham.TypeScript.Types

open System
open System.Collections.Generic
open System.ComponentModel
open System.Runtime.CompilerServices
open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open FsToolkit.ErrorHandling
open Xantham.Fable.Tracer

// ---------------------------------------------------------------------------
// File structure:
// Define types in top level modules.
// Implementations and modules are defined in the `Core` and `CoreExtensions`
// recursive modules to allow sharing of core logic.
// The only exception to this is type members which are required to be
// placed no the original definition (such as op_Implicit members and interfaces).
// ---------------------------------------------------------------------------

/// <summary>
/// Defines symbols that are used to embed objects generically at runtime.
/// </summary>
module SymbolTypeKeys =
    let symbolNameSigil = SymbolTypeKey.create<SymbolName> "SymbolName"
    let typeCheckerSigil = SymbolTypeKey.create<Ts.TypeChecker> "TypeChecker"
    let programSigil = SymbolTypeKey.create<Ts.Program> "Program"
    module SymbolName =
        let inline accessOrInit ([<InlineIfLambda>] fn: 'T -> SymbolName) (value: 'T) =
            SymbolTypeKey.accessOrInit symbolNameSigil (fun () -> fn value) value
    module TypeChecker =
        let inline setIfAbsent (checker: Ts.TypeChecker) (value: 'T) =
            SymbolTypeKey.setIfAbsent typeCheckerSigil checker value
        let inline get (value: 'T) = SymbolTypeKey.unsafeAccess typeCheckerSigil value
        let inline addIfAbsent checker value =
            setIfAbsent checker value; value
    module Program =
        let inline setIfAbsent (program: Ts.Program) (value: 'T) =
            SymbolTypeKey.setIfAbsent programSigil program value
        let inline addIfAbsent program value =
            setIfAbsent program value; value
        let inline get (value: 'T) = SymbolTypeKey.unsafeAccess programSigil value
    
type Ts.Program with
    static member Create(files: string list, ?compilerOptions) =
        let program = ts.createProgram(Ts.CreateProgramOptions.Create(files, ?compilerOptions = compilerOptions))
        let checker = program.getTypeChecker()
        SymbolTypeKeys.Program.setIfAbsent program checker
        SymbolTypeKeys.Program.setIfAbsent program program
        SymbolTypeKeys.TypeChecker.setIfAbsent checker program
        SymbolTypeKeys.TypeChecker.setIfAbsent checker checker
        program
type Ts.TypeChecker with
    member inline this.program = SymbolTypeKeys.Program.get this
type Ts.Type with
    member inline this.program = this.checker.program
    
/// <summary>
/// Interfaces that represent or tag type variations/behaviours.
/// </summary>
[<AutoOpen>]
module TypeInterfaces =
    /// <summary>
    /// An erased type interface that inherits a pattern unwrapper and value property
    /// for the provided type parameter.
    /// </summary>
    [<Interface>]
    type IUnwrappable<'ErasedUnderlyingType> = interface end
    [<Interface>]
    type IFastUnionUnwrappable<'UnderlyingType> = interface end
    /// <summary>
    /// An interface intended for discriminated unions which indicates it has the
    /// typescript compiler program and checker inlined into the object.
    /// </summary>
    [<Interface>]
    type IInlinedProgram = interface end
    [<Interface>]
    type IInlinedTypeChecker = interface end
    /// <summary>
    /// An interface that indicates the type is a unique identifier.
    /// </summary>
    [<Interface>]
    type IIdentityKey<'T> = interface end
    
    /// <summary>
    /// Xantham generic wrapper for symbols
    /// </summary>
    [<Interface; AllowNullLiteral>]
    type ISymbol = static member inline op_Implicit(other: ISymbol): Ts.Symbol = unbox other
    /// <summary>
    /// Xantham generic wrapper for types
    /// </summary>
    [<Interface; AllowNullLiteral>]
    type IType = static member inline op_Implicit(other: IType): Ts.Type = unbox other
    /// <summary>
    /// Xantham generic wrapper for nodes
    /// </summary>
    [<Interface; AllowNullLiteral>]
    type INode = static member inline op_Implicit(other: INode): Ts.Node = unbox other
    
    /// <summary>
    /// Inheriting types are used to hint what behaviour you can expect from the
    /// type script compiler.
    /// </summary>
    type ICompilerBehaviourHint = interface end
    /// <summary>
    /// Indicates the underlying object never has a symbol
    /// </summary>
    type INeverSymbol = inherit ICompilerBehaviourHint
    /// <summary>
    /// Indicates the underlying object can have a symbol
    /// </summary>
    type ICanHaveSymbol = inherit ICompilerBehaviourHint
    /// <summary>
    /// Indicates the underlying object can have a symbol, and it can
    /// be of the specific symbol annotation
    /// </summary>
    type ICanHaveSymbol<'T when 'T :> ISymbol> = inherit ICanHaveSymbol
    /// <summary>
    /// Indicates the underlying object always has a symbol
    /// </summary>
    type IAlwaysSymbol = inherit ICanHaveSymbol
    /// <summary>
    /// Indicates the underlying object can have a symbol, and it always be guaranteed
    /// to be of the specific symbol annotation
    /// </summary>
    type IAlwaysSymbol<'T when 'T :> ISymbol> = inherit IAlwaysSymbol
    
    /// <summary>
    /// Indicates that the underlying object can have type parameters.
    /// </summary>
    type ICanHaveTypeParameters =
        inherit ICompilerBehaviourHint
        abstract TypeParameters: NonEmptyArray<Ts.TypeParameterDeclaration> option

    /// <summary>
    /// Indicates the underlying object never has a type
    /// </summary>
    type INeverType = inherit ICompilerBehaviourHint
    /// <summary>
    /// Indicates the underlying object can have a type
    /// </summary>
    type ICanHaveType = inherit ICompilerBehaviourHint
    /// <summary>
    /// Indicates the underlying object always has a type
    /// </summary>
    type IAlwaysType = inherit ICanHaveType
    
    /// <summary>
    /// Used when typical acquisition of a type/symbol/node would not work via
    /// the public API, but instead the object is embedded in the inheriting type
    /// itself by the compiler.
    /// </summary>
    type IEmbedded<'EmbeddedTag when 'EmbeddedTag :> ICompilerBehaviourHint> =
        inherit ICompilerBehaviourHint
    
    
    type IUnwrappable<'T> with
        member inline this.Value = unbox<'T> this
    
    type IFastUnionUnwrappable<'T> with
        member inline this.Value: 'T = emitJsExpr this "$0.fields[0]"

    type IIdentityKey<'T> with
        member inline this.Value = unbox<int> this
        static member inline Create(key: int): 'T = unbox<'T> key
        
    let inline (|Unwrap|) (value: IUnwrappable<'T>): 'T = unbox<'T> value
    
[<AutoOpen>]
module UniqueIdentifiers =
    [<Erase>]
    type SymbolKey = SymbolKey of int interface IIdentityKey<SymbolKey>
    [<Erase>]
    type TypeKey = TypeKey of int interface IIdentityKey<TypeKey>
    [<Erase>]
    type NodeKey = NodeKey of int interface IIdentityKey<NodeKey>
    
    type CompositeKey = private {
        Symbol: SymbolKey option
        Node: NodeKey option
        Type: TypeKey option
    } with
        // Because the shape is private, the members for creation and introspection must be defined here.
        static member Create(?symbol,?node,?typ) = { Symbol = symbol; Node = node; Type = typ }
        member this.symbolKey = this.Symbol
        member this.nodeKey = this.Node
        member this.typeKey = this.Type
        
    module NodeKey =
        let inline fromNode (node: Ts.Node): NodeKey = ts.getNodeId node |> NodeKey.Create
        let inline fromINode (node: INode): NodeKey = ts.getNodeId node |> NodeKey.Create
        type SRTPHelper =
            static member inline fromNode node = fromNode node
            static member inline fromINode node = fromINode node
        let inline get node = ((^T or SRTPHelper):(static member fromNode: ^T -> NodeKey) node)
    module TypeKey =
        let inline fromType (typ: Ts.Type): TypeKey = typ.id |> TypeKey.Create
        let inline fromIType (typ: IType): TypeKey = unbox<Ts.Type> typ |> _.id |> TypeKey.Create
        type SRTPHelper =
            static member inline fromType typ = fromType typ
            static member inline fromType typ = fromIType typ
        let inline get typ = ((^T or SRTPHelper):(static member fromType: ^T -> TypeKey) typ)
    module SymbolKey =
        let inline fromSymbol (sym: Ts.Symbol): SymbolKey = ts.getSymbolId sym |> SymbolKey.Create
        let inline fromISymbol (sym: ISymbol): SymbolKey = ts.getSymbolId sym |> SymbolKey.Create
        type SRTPHelper =
            static member inline fromSymbol sym = fromSymbol sym
            static member inline fromSymbol sym = fromISymbol sym
        let inline get sym = ((^T or SRTPHelper):(static member fromSymbol: ^T -> SymbolKey) sym)

/// <summary>
/// Types that are used to wrap other types to tag them with various guarantees.
/// </summary>
[<AutoOpen>]
module TypeWrappers =
    /// <summary>
    /// Enforce that user code cannot create these types without using the constructors.
    /// </summary>
    [<Erase>]
    type InlinedProgram<'T> = private InlinedProgram of obj with
        static member inline op_Implicit(other: InlinedProgram<'T>): 'T = unbox other
        interface IUnwrappable<'T>
        interface IInlinedProgram
    type inlinedProgram<'T> = InlinedProgram<'T>

    
    /// <summary>
    /// Enforce that user code cannot create these types without using the constructors.
    /// </summary>
    [<Erase>]
    type ParentInlinedProgram<'T> = private ParentInlinedProgram of obj with
        static member inline op_Implicit(other: ParentInlinedProgram<'T>): 'T = unbox other
        interface IUnwrappable<'T>
    type parentInlinedProgram<'T> = ParentInlinedProgram<'T>
    
    module InlinedProgram =
        let inline create<'T> (program: Ts.Program) (value: 'T) =
            if SymbolTypeKey.has SymbolTypeKeys.programSigil value then value |> unbox<InlinedProgram<'T>> else
            SymbolTypeKey.addIfAbsent SymbolTypeKeys.programSigil program value
            |>  SymbolTypeKey.addIfAbsent SymbolTypeKeys.typeCheckerSigil (program.getTypeChecker())
            |> unbox<InlinedProgram<'T>>
        /// Internal only
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline inject<'T> (program: Ts.Program) (value: 'T) = create program value |> unbox<'T>
        /// Internal only
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline extract<'T> (value: 'T) = unbox<InlinedProgram<'T>> value
        let inline program (inlinedProgram: InlinedProgram<'T>) =
            SymbolTypeKey.unsafeAccess SymbolTypeKeys.programSigil inlinedProgram
        let inline checker (inlinedProgram: InlinedProgram<'T>) =
            SymbolTypeKey.unsafeAccess SymbolTypeKeys.typeCheckerSigil inlinedProgram
        let inline value (inlinedProgram: InlinedProgram<'T>) =
            unbox<'T> inlinedProgram
        let inline (|Unwrap|) (inlinedProgram: InlinedProgram<'T>): 'T =
            (|Unwrap|) inlinedProgram
            
    type IInlinedProgram with
        member inline this.program = InlinedProgram.program (unbox this)
        member inline this.checker = InlinedProgram.checker (unbox this)
    type InlinedProgram<'T> with
        member inline this.program = InlinedProgram.program (unbox this)
        member inline this.checker = InlinedProgram.checker (unbox this)
            
    module ParentInlinedProgram =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline wrap<'T, 'U> program ([<InlineIfLambda>] fn: 'T parentInlinedProgram -> 'U) (value: 'T) =
            fn (ParentInlinedProgram value)
            |> InlinedProgram.inject program
            
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline value (value: 'T ParentInlinedProgram): 'T = unbox value
        
        let inline (|Unwrap|) (value: 'T ParentInlinedProgram): 'T = (|Unwrap|) value

            
// ---------------------------------------------------------------------------------------
/// <summary>
/// We categorise a symbol based on its canonical declaration kind.<br/>
/// We expect symbols to merge with other declarations that are compatible.<br/>
/// The compatibility of merges is determined by the SymbolFlag exclusion masks.<br/>
/// A structural test exists in the Generator suite which generates the interfaces
/// for the symbol merge compatibilities based on the symbol flags.<br/>
/// It is unclear whether a lack of an exclusion mask implies that a symbol is compatible
/// with any other kind of symbol (see: <c>Ts.SymbolFlags.Signature</c>).
/// <br/><br/>
/// Where a symbol is not transient, we can guarantee the presence of:<br/>
/// 1. At least one declaration<br/>
/// 2. A canonical declaration<br/>
/// <br/>
/// Non-transient symbols are considered 'concrete' in the sense that we can guarantee
/// a declaration. This is reflected by the distinction of interfaces in the <c>Symbol.Transient</c>
/// bucket verse the standard <c>Symbol</c> bucket.
/// <br/><br/>
/// We then can pattern match against the canonical symbol kind flags to determine what other
/// declarations we can expect. Or, just attempt to gather the declarations from compatible
/// symbol kinds. If they exist, you will get a value.<br/>
/// We test all the symbols in our corpus of fixtures (see <c>tests/Fixtures.Setup.fsx</c>)
/// for compatibility with this wrapper. 100% coverage is achieved.
/// </summary>
// ---------------------------------------------------------------------------------------
module Symbol =
    // Tags
    type IOptional =
        inherit ISymbol
    type ITransient =
        inherit ISymbol
    /// <summary>Indicates this can merge with declarations of its own kind.</summary>
    type IDuplicates = interface end

    [<RequireQualifiedAccess>]
    module Transient =
        // Composites
        type IEnum =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IValue =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IType =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type INamespace =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IModule =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IAccessor =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IModuleMember =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IExportHasLocal =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IPropertyOrAccessor =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IClassMember =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
    
        // Flags
        type IParameter =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IModuleMember
        type IVariable =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IModuleMember
        type IProperty =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IPropertyOrAccessor
            inherit IClassMember
        type IEnumMember =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IType
        type IFunction =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IModuleMember
            inherit IExportHasLocal
        type IClass =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IType
            inherit IModuleMember
            inherit IExportHasLocal
        type IInterface =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IType
            inherit IModuleMember
        type IConstEnum =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IEnum
            inherit IValue
            inherit IType
            inherit INamespace
            inherit IModuleMember
            inherit IExportHasLocal
        type ITypeEnum =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IEnum
            inherit IValue
            inherit IType
            inherit INamespace
            inherit IModuleMember
            inherit IExportHasLocal
        type IValueModule =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit INamespace
            inherit IModule
            inherit IModuleMember
            inherit IExportHasLocal
        type INamespaceModule =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit INamespace
            inherit IModule
            inherit IModuleMember
        type ITypeLiteral =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IType
        type IObjectLiteral =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
        type IMethod =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IClassMember
        type IConstructor =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type IGetAccessor =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IAccessor
            inherit IPropertyOrAccessor
            inherit IClassMember
        type ISetAccessor =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IValue
            inherit IAccessor
            inherit IPropertyOrAccessor
            inherit IClassMember
        type ISignature =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
        type ITypeParameter =
            inherit IDuplicates
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IType
        type ITypeAlias =
            inherit ISymbol
            inherit ITransient
            inherit IOptional
            inherit IType
            inherit IModuleMember
    
        // Symbols
        type Parameter =
            inherit IParameter
            inherit IInterface
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type Variable =
            inherit IVariable
            inherit IInterface
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type Property =
            inherit IProperty
            inherit IParameter
            inherit IVariable
            inherit IEnumMember
            inherit IFunction
            inherit IClass
            inherit IInterface
            inherit IConstEnum
            inherit ITypeEnum
            inherit IValueModule
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IObjectLiteral
            inherit IMethod
            inherit IConstructor
            inherit IGetAccessor
            inherit ISetAccessor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type EnumMember =
            inherit IEnumMember
            inherit INamespaceModule
            inherit IConstructor
            inherit ISignature
            inherit IOptional
        type Function =
            inherit IFunction
            inherit IClass
            inherit IInterface
            inherit IValueModule
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type Class =
            inherit IClass
            inherit IFunction
            inherit IInterface
            inherit IValueModule
            inherit INamespaceModule
            inherit IConstructor
            inherit ISignature
            inherit IOptional
        type Interface =
            inherit IInterface
            inherit IParameter
            inherit IVariable
            inherit IProperty
            inherit IFunction
            inherit IClass
            inherit IValueModule
            inherit INamespaceModule
            inherit IObjectLiteral
            inherit IMethod
            inherit IConstructor
            inherit IGetAccessor
            inherit ISetAccessor
            inherit ISignature
            inherit IOptional
        type ConstEnum =
            inherit IConstEnum
            inherit INamespaceModule
            inherit IConstructor
            inherit ISignature
            inherit IOptional
        type TypeEnum =
            inherit ITypeEnum
            inherit IValueModule
            inherit INamespaceModule
            inherit IConstructor
            inherit ISignature
            inherit IOptional
        type ValueModule =
            inherit IValueModule
            inherit IFunction
            inherit IClass
            inherit IInterface
            inherit ITypeEnum
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type NamespaceModule =
            inherit INamespaceModule
            inherit IParameter
            inherit IVariable
            inherit IProperty
            inherit IEnumMember
            inherit IFunction
            inherit IClass
            inherit IInterface
            inherit IConstEnum
            inherit ITypeEnum
            inherit IValueModule
            inherit ITypeLiteral
            inherit IObjectLiteral
            inherit IMethod
            inherit IConstructor
            inherit IGetAccessor
            inherit ISetAccessor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type TypeLiteral =
            inherit IOptional
        type ObjectLiteral =
            inherit IOptional
        type Method =
            inherit IMethod
            inherit IInterface
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type Constructor =
            inherit IConstructor
            inherit IOptional
        type GetAccessor =
            inherit IGetAccessor
            inherit IInterface
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit ISetAccessor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type SetAccessor =
            inherit ISetAccessor
            inherit IInterface
            inherit INamespaceModule
            inherit ITypeLiteral
            inherit IConstructor
            inherit IGetAccessor
            inherit ISignature
            inherit ITypeParameter
            inherit ITypeAlias
            inherit IOptional
        type Signature =
            inherit ISignature
            inherit IOptional
        type TypeParameter =
            inherit ITypeParameter
            inherit IParameter
            inherit IVariable
            inherit IProperty
            inherit IFunction
            inherit IValueModule
            inherit INamespaceModule
            inherit IObjectLiteral
            inherit IMethod
            inherit IConstructor
            inherit IGetAccessor
            inherit ISetAccessor
            inherit ISignature
            inherit IOptional
        type TypeAlias =
            inherit ITypeAlias
            inherit IParameter
            inherit IVariable
            inherit IProperty
            inherit IFunction
            inherit IValueModule
            inherit INamespaceModule
            inherit IObjectLiteral
            inherit IMethod
            inherit IConstructor
            inherit IGetAccessor
            inherit ISetAccessor
            inherit ISignature
            inherit IOptional
    
        // Discriminated Union
        [<RequireQualifiedAccess>]
        type Kind =
            | Parameter of Parameter
            | Variable of Variable
            | Property of Property
            | EnumMember of EnumMember
            | Function of Function
            | Class of Class
            | Interface of Interface
            | ConstEnum of ConstEnum
            | TypeEnum of TypeEnum
            | ValueModule of ValueModule
            | NamespaceModule of NamespaceModule
            | TypeLiteral of TypeLiteral
            | ObjectLiteral of ObjectLiteral
            | Method of Method
            | Constructor of Constructor
            | GetAccessor of GetAccessor
            | SetAccessor of SetAccessor
            | Signature of Signature
            | TypeParameter of TypeParameter
            | TypeAlias of TypeAlias
            | Unknown of ISymbol
            interface IFastUnionUnwrappable<ISymbol>
    
    // Composites
    type IEnum =
        inherit Transient.IEnum
        inherit IOptional
    type IValue =
        inherit Transient.IValue
        inherit IOptional
    type IType =
        inherit Transient.IType
        inherit IOptional
    type INamespace =
        inherit Transient.INamespace
        inherit IOptional
    type IModule =
        inherit Transient.IModule
        inherit IOptional
    type IAccessor =
        inherit Transient.IAccessor
        inherit IOptional
    type IModuleMember =
        inherit Transient.IModuleMember
        inherit IOptional
    type IExportHasLocal =
        inherit Transient.IExportHasLocal
        inherit IOptional
    type IPropertyOrAccessor =
        inherit Transient.IPropertyOrAccessor
        inherit IOptional
    type IClassMember =
        inherit Transient.IClassMember
        inherit IOptional

    // Flags
    type IParameter =
        inherit Transient.IParameter
        inherit IOptional
        inherit IValue
        inherit IModuleMember
    type IVariable =
        inherit Transient.IVariable
        inherit IOptional
        inherit IValue
        inherit IModuleMember
    type IProperty =
        inherit IDuplicates
        inherit Transient.IProperty
        inherit IOptional
        inherit IValue
        inherit IPropertyOrAccessor
        inherit IClassMember
    type IEnumMember =
        inherit Transient.IEnumMember
        inherit IOptional
        inherit IValue
        inherit IType
    type IFunction =
        inherit IDuplicates
        inherit Transient.IFunction
        inherit IOptional
        inherit IValue
        inherit IModuleMember
        inherit IExportHasLocal
    type IClass =
        inherit Transient.IClass
        inherit IOptional
        inherit IValue
        inherit IType
        inherit IModuleMember
        inherit IExportHasLocal
    type IInterface =
        inherit IDuplicates
        inherit Transient.IInterface
        inherit IOptional
        inherit IType
        inherit IModuleMember
    type IConstEnum =
        inherit IDuplicates
        inherit Transient.IConstEnum
        inherit IOptional
        inherit IEnum
        inherit IValue
        inherit IType
        inherit INamespace
        inherit IModuleMember
        inherit IExportHasLocal
    type ITypeEnum =
        inherit IDuplicates
        inherit Transient.ITypeEnum
        inherit IOptional
        inherit IEnum
        inherit IValue
        inherit IType
        inherit INamespace
        inherit IModuleMember
        inherit IExportHasLocal
    type IValueModule =
        inherit IDuplicates
        inherit Transient.IValueModule
        inherit IOptional
        inherit IValue
        inherit INamespace
        inherit IModule
        inherit IModuleMember
        inherit IExportHasLocal
    type INamespaceModule =
        inherit IDuplicates
        inherit Transient.INamespaceModule
        inherit IOptional
        inherit INamespace
        inherit IModule
        inherit IModuleMember
    type ITypeLiteral =
        inherit Transient.ITypeLiteral
        inherit IOptional
        inherit IType
    type IObjectLiteral =
        inherit Transient.IObjectLiteral
        inherit IOptional
        inherit IValue
    type IMethod =
        inherit IDuplicates
        inherit Transient.IMethod
        inherit IOptional
        inherit IValue
        inherit IClassMember
    type IConstructor =
        inherit Transient.IConstructor
        inherit IOptional
    type IGetAccessor =
        inherit Transient.IGetAccessor
        inherit IOptional
        inherit IValue
        inherit IAccessor
        inherit IPropertyOrAccessor
        inherit IClassMember
    type ISetAccessor =
        inherit Transient.ISetAccessor
        inherit IOptional
        inherit IValue
        inherit IAccessor
        inherit IPropertyOrAccessor
        inherit IClassMember
    type ISignature =
        inherit Transient.ISignature
        inherit IOptional
    type ITypeParameter =
        inherit IDuplicates
        inherit Transient.ITypeParameter
        inherit IOptional
        inherit IType
    type ITypeAlias =
        inherit Transient.ITypeAlias
        inherit IOptional
        inherit IType
        inherit IModuleMember

    // Symbols
    type Parameter =
        inherit Transient.Parameter
        inherit IParameter
        inherit IInterface
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type Variable =
        inherit Transient.Variable
        inherit IVariable
        inherit IInterface
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type Property =
        inherit Transient.Property
        inherit IProperty
        inherit IParameter
        inherit IVariable
        inherit IEnumMember
        inherit IFunction
        inherit IClass
        inherit IInterface
        inherit IConstEnum
        inherit ITypeEnum
        inherit IValueModule
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IObjectLiteral
        inherit IMethod
        inherit IConstructor
        inherit IGetAccessor
        inherit ISetAccessor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type EnumMember =
        inherit Transient.EnumMember
        inherit IEnumMember
        inherit INamespaceModule
        inherit IConstructor
        inherit ISignature
        inherit IOptional
    type Function =
        inherit Transient.Function
        inherit IFunction
        inherit IClass
        inherit IInterface
        inherit IValueModule
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type Class =
        inherit Transient.Class
        inherit IClass
        inherit IFunction
        inherit IInterface
        inherit IValueModule
        inherit INamespaceModule
        inherit IConstructor
        inherit ISignature
        inherit IOptional
    type Interface =
        inherit Transient.Interface
        inherit IInterface
        inherit IParameter
        inherit IVariable
        inherit IProperty
        inherit IFunction
        inherit IClass
        inherit IValueModule
        inherit INamespaceModule
        inherit IObjectLiteral
        inherit IMethod
        inherit IConstructor
        inherit IGetAccessor
        inherit ISetAccessor
        inherit ISignature
        inherit IOptional
    type ConstEnum =
        inherit Transient.ConstEnum
        inherit IConstEnum
        inherit INamespaceModule
        inherit IConstructor
        inherit ISignature
        inherit IOptional
    type TypeEnum =
        inherit Transient.TypeEnum
        inherit ITypeEnum
        inherit IValueModule
        inherit INamespaceModule
        inherit IConstructor
        inherit ISignature
        inherit IOptional
    type ValueModule =
        inherit Transient.ValueModule
        inherit IValueModule
        inherit IFunction
        inherit IClass
        inherit IInterface
        inherit ITypeEnum
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type NamespaceModule =
        inherit Transient.NamespaceModule
        inherit INamespaceModule
        inherit IParameter
        inherit IVariable
        inherit IProperty
        inherit IEnumMember
        inherit IFunction
        inherit IClass
        inherit IInterface
        inherit IConstEnum
        inherit ITypeEnum
        inherit IValueModule
        inherit ITypeLiteral
        inherit IObjectLiteral
        inherit IMethod
        inherit IConstructor
        inherit IGetAccessor
        inherit ISetAccessor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type TypeLiteral =
        inherit Transient.TypeLiteral
        inherit IOptional
    type ObjectLiteral =
        inherit Transient.ObjectLiteral
        inherit IOptional
    type Method =
        inherit Transient.Method
        inherit IMethod
        inherit IInterface
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type Constructor =
        inherit Transient.Constructor
        inherit IOptional
    type GetAccessor =
        inherit Transient.GetAccessor
        inherit IGetAccessor
        inherit IInterface
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit ISetAccessor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type SetAccessor =
        inherit Transient.SetAccessor
        inherit ISetAccessor
        inherit IInterface
        inherit INamespaceModule
        inherit ITypeLiteral
        inherit IConstructor
        inherit IGetAccessor
        inherit ISignature
        inherit ITypeParameter
        inherit ITypeAlias
        inherit IOptional
    type Signature =
        inherit Transient.Signature
        inherit ISignature
        inherit IOptional
    type TypeParameter =
        inherit Transient.TypeParameter
        inherit ITypeParameter
        inherit IParameter
        inherit IVariable
        inherit IProperty
        inherit IFunction
        inherit IValueModule
        inherit INamespaceModule
        inherit IObjectLiteral
        inherit IMethod
        inherit IConstructor
        inherit IGetAccessor
        inherit ISetAccessor
        inherit ISignature
        inherit IOptional
    type TypeAlias =
        inherit Transient.TypeAlias
        inherit ITypeAlias
        inherit IParameter
        inherit IVariable
        inherit IProperty
        inherit IFunction
        inherit IValueModule
        inherit INamespaceModule
        inherit IObjectLiteral
        inherit IMethod
        inherit IConstructor
        inherit IGetAccessor
        inherit ISetAccessor
        inherit ISignature
        inherit IOptional

    // Discriminated Union
    [<RequireQualifiedAccess>]
    type Kind =
        | Parameter of Parameter
        | Variable of Variable
        | Property of Property
        | EnumMember of EnumMember
        | Function of Function
        | Class of Class
        | Interface of Interface
        | ConstEnum of ConstEnum
        | TypeEnum of TypeEnum
        | ValueModule of ValueModule
        | NamespaceModule of NamespaceModule
        | TypeLiteral of TypeLiteral
        | ObjectLiteral of ObjectLiteral
        | Method of Method
        | Constructor of Constructor
        | GetAccessor of GetAccessor
        | SetAccessor of SetAccessor
        | Signature of Signature
        | TypeParameter of TypeParameter
        | TypeAlias of TypeAlias
        | Transient of Transient.Kind

    [<Erase>]
    type LocalTableSymbol = LocalTableSymbol of Ts.Symbol interface IUnwrappable<Ts.Symbol>
    [<Erase>]
    type ExportTableSymbol = ExportTableSymbol of Ts.Symbol interface IUnwrappable<Ts.Symbol>

[<AutoOpen>]
module Maps =
    type SymbolTable = Map<SymbolName, Ts.Symbol>
    type LocalSymbolTable = Map<SymbolName, Symbol.LocalTableSymbol>
    type ExportSymbolTable = Map<SymbolName, Symbol.ExportTableSymbol>
    
    module SymbolTable =
        let create (symbolTable: Ts.SymbolTable) =
            symbolTable.entries()
            |> Seq.map (fun (symbolName, symbol) ->
                SymbolName.Create symbolName, symbol)
            |> Map
        let createOrFail (symbolTable: Ts.SymbolTable option) =
            symbolTable
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SymbolTable.fromOption: unexpected failure to find symbol table. Please raise an issue."
                failwith "SymbolTable.fromOption: unexpected failure to find symbol table. Please raise an issue."
                )
            |> create
    
    module ExportSymbolTable =
        let create (symbolTable: Ts.SymbolTable): ExportSymbolTable =
            SymbolTable.create symbolTable
            |> unbox
        let createOrFail = SymbolTable.createOrFail >> unbox<ExportSymbolTable>
        let toSymbolTable: ExportSymbolTable -> SymbolTable = unbox
        
    module LocalSymbolTable =
        let create (symbolTable: Ts.SymbolTable): LocalSymbolTable =
            SymbolTable.create symbolTable
            |> unbox
        let createOrFail = SymbolTable.createOrFail >> unbox<LocalSymbolTable>
        let toSymbolTable: LocalSymbolTable -> SymbolTable = unbox
        
        let fromSourceFile (sourceFile: Ts.SourceFile): LocalSymbolTable =
            sourceFile?locals
            |> Option.ofObj
            |> createOrFail

module Node =
    [<RequireQualifiedAccess>]
    type JSDoc =
        | Tag of Ts.JSDocTag
        | Deprecated of Ts.JSDocDeprecatedTag
        | Type of Ts.JSDocTypeTag
        | Parameter of Ts.JSDocParameterTag
        | Private of Ts.JSDocPrivateTag
        | Return of Ts.JSDocReturnTag
        | See of Ts.JSDocSeeTag
        | Readonly of Ts.JSDocReadonlyTag
        | Override of Ts.JSDocOverrideTag
        | Public of Ts.JSDocPublicTag
        | Callback of Ts.JSDocCallbackTag
        | Throws of Ts.JSDocThrowsTag
        | Typedef of Ts.JSDocTypedefTag
        | Template of Ts.JSDocTemplateTag
        | Import of Ts.JSDocImportTag
        | Overload of Ts.JSDocOverloadTag
        | Augments of Ts.JSDocAugmentsTag
        | Class of Ts.JSDocClassTag
        interface IFastUnionUnwrappable<Ts.JSDocTag>
        
    type CommentPartLink = {
        Text: string option
        Name: Choice<Symbol.Kind, string> option
    }
        
    [<RequireQualifiedAccess>]
    type CommentPart =
        | Text of string array
        | Link of CommentPartLink
        | LinkCode of CommentPartLink
        | LinkPlain of CommentPartLink

    [<RequireQualifiedAccess>]
    type ModifierKeyword =
        | Export
        | Declare
        | Default
        | Abstract
        | ReadOnly
        | Static
        | Override
        | Public
        | Protected
        | Private
        | Out
        | In
        | Const
    
    [<RequireQualifiedAccess>]
    type TypeKeyword =
        | String
        | Number
        | Boolean
        | Null
        | Undefined
        | Void
        | Never
        | Any
        | Unknown
        | Object
        | Symbol
        | BigInt
        | Intrinsic
        interface IAlwaysType
    
    type [<Erase>] UnionType =
        | UnionType of Ts.UnionTypeNode inlinedProgram
        interface IEmbedded<INeverSymbol>
        interface INeverSymbol
        interface IAlwaysType
        interface IUnwrappable<Ts.UnionTypeNode>
        
    and [<RequireQualifiedAccess>] Literal =
        | Numeric of Ts.NumericLiteral parentInlinedProgram
        | String of Ts.StringLiteral parentInlinedProgram
        | Boolean of bool parentInlinedProgram
        | Null
        | BigInt of Ts.BigIntLiteral parentInlinedProgram
        | NoSubstitutionTemplateLiteral of Ts.NoSubstitutionTemplateLiteral parentInlinedProgram
        interface IInlinedProgram
    
    and [<Erase>] IntersectionType =
        | IntersectionType of Ts.IntersectionTypeNode inlinedProgram
        interface IEmbedded<INeverSymbol>
        interface INeverSymbol
        interface IAlwaysType
        interface IUnwrappable<Ts.IntersectionTypeNode>
    
    and [<Erase>] ArrayType =
        | ArrayType of Ts.ArrayTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.ArrayTypeNode>
    
    and [<Erase>] TupleType =
        | TupleType of Ts.TupleTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TupleTypeNode>
    
    and [<Erase>] NamedTupleMember =
        | NamedTupleMember of Ts.NamedTupleMember inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.NamedTupleMember>
    and [<Erase>] RestType =
        | RestType of Ts.RestTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.RestTypeNode>
    and [<Erase>] OptionalType =
        | OptionalType of Ts.OptionalTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.OptionalTypeNode>
    
    and [<Erase>] ParenthesizedType =
        | ParenthesizedType of Ts.ParenthesizedTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.ParenthesizedTypeNode>
    
    and [<Erase>] TypeParameterDeclaration =
        | TypeParameterDeclaration of Ts.TypeParameterDeclaration inlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TypeParameterDeclaration>
    
    and [<Erase>] InferType =
        | InferType of Ts.InferTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.InferTypeNode>
    
    and [<Erase>] TypePredicate =
        | TypePredicate of Ts.TypePredicateNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TypePredicateNode>
    
    and [<Erase>] TypeQuery =
        | TypeQuery of Ts.TypeQueryNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TypeQueryNode>
        
    and [<Erase>] IndexedAccessType =
        | IndexedAccessType of Ts.IndexedAccessTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.IndexedAccessTypeNode>
    
    and [<Erase>] MappedType =
        | MappedType of Ts.MappedTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.MappedTypeNode>
    
    and [<Erase>] ConditionalType =
        | ConditionalType of Ts.ConditionalTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.ConditionalTypeNode>
    
    and [<Erase>] TemplateLiteralType =
        | TemplateLiteralType of Ts.TemplateLiteralTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TemplateLiteralTypeNode>
    
    and [<Erase>] TemplateLiteralTypeSpan =
        | TemplateLiteralTypeSpan of Ts.TemplateLiteralTypeSpan inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TemplateLiteralTypeSpan>
    
    
    and [<Erase>] ImportType =
        | ImportType of Ts.ImportTypeNode inlinedProgram
        interface IAlwaysSymbol
        interface IAlwaysType
        interface IUnwrappable<Ts.ImportTypeNode>
    
    
    and FunctionType =
        | FunctionType of Ts.FunctionTypeNode parentInlinedProgram
        interface INeverSymbol
        interface IInlinedProgram
        interface IEmbedded<IAlwaysSymbol>
        interface IAlwaysType
        interface IFastUnionUnwrappable<Ts.FunctionTypeNode>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                ts.getEffectiveTypeParameterDeclarations !!this.Value
                |> NonEmptyArray.create
    
    
    and ConstructorType =
        | ConstructorType of Ts.ConstructorTypeNode parentInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IAlwaysType
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.ConstructorTypeNode>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                ts.getEffectiveTypeParameterDeclarations !!this.Value
                |> NonEmptyArray.create
    
    and [<Erase>] TypeLiteralType =
        | TypeLiteralType of Ts.TypeLiteralNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.TypeLiteralNode>
    
    and [<Erase>] LiteralType =
        | LiteralType of Ts.LiteralTypeNode inlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IUnwrappable<Ts.LiteralTypeNode>
    
    and [<Erase>] ThisType =
        | ThisType of Ts.ThisTypeNode inlinedProgram
        interface IAlwaysSymbol
        interface IAlwaysType
        interface IUnwrappable<Ts.ThisTypeNode>
    
    and [<RequireQualifiedAccess>] KeyOf =
        | Bounded of Type 
        | Unbounded of Type 
        | Generic of Type 
        | ConstrainedGeneric of Type 
        interface INeverSymbol
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Type>

    
    and [<RequireQualifiedAccess>] TypeOperator =
        | KeyOf of KeyOf
        | Readonly of Type 
        | Unique of Type 
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IInlinedProgram
        
    
    and TypeReference = TypeReference of Ts.TypeReferenceNode parentInlinedProgram with
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IFastUnionUnwrappable<Ts.TypeReferenceNode>
    
    and ExpressionWithTypeArguments = ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments parentInlinedProgram with
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IAlwaysType
        interface IFastUnionUnwrappable<Ts.ExpressionWithTypeArguments>
        
    /// <summary>
    /// </summary>
    /// <remarks>Percentage of cases based on the corpus of tests in Xantham.TypeScript. Percentages
    /// do not differentiate from whether they were a nested type node or not.</remarks>
    and [<RequireQualifiedAccess>] Type =
        /// ~39% of cases
        | Reference of TypeReference * Ts.TypeNode parentInlinedProgram
        /// ~29% of cases
        | Keyword of TypeKeyword * Ts.TypeNode parentInlinedProgram
        /// ~10% of cases
        | Literal of LiteralType * Ts.TypeNode parentInlinedProgram
        /// ~7% of cases
        | Union of UnionType * Ts.TypeNode parentInlinedProgram
        /// ~3% of cases
        | Function of FunctionType * Ts.TypeNode parentInlinedProgram
        /// ~2% of cases
        | TypeLiteral of TypeLiteralType * Ts.TypeNode parentInlinedProgram
        /// ~2% of cases
        | ExpressionWithTypeArguments of ExpressionWithTypeArguments * Ts.TypeNode parentInlinedProgram
        /// ~1.5% of cases
        | Array of ArrayType * Ts.TypeNode parentInlinedProgram
        /// ~1% of cases
        | TypeOperator of TypeOperator * Ts.TypeNode parentInlinedProgram
        /// ~1% of cases
        | IndexedAccess of IndexedAccessType * Ts.TypeNode parentInlinedProgram
        /// ~1% of cases
        | Parenthesized of ParenthesizedType * Ts.TypeNode parentInlinedProgram
        /// ~1% of cases
        | This of ThisType * Ts.TypeNode parentInlinedProgram
        /// ~0.5% of cases
        | Tuple of TupleType * Ts.TypeNode parentInlinedProgram
        /// ~0.25% of cases
        | Conditional of ConditionalType * Ts.TypeNode parentInlinedProgram
        /// ~0.25% of cases
        | Intersection of IntersectionType * Ts.TypeNode parentInlinedProgram
        /// ~0.25% of cases
        | TypeQuery of TypeQuery * Ts.TypeNode parentInlinedProgram
        /// ~0.1% of cases
        | Infer of InferType * Ts.TypeNode parentInlinedProgram
        /// ~0.1% of cases
        | TypePredicate of TypePredicate * Ts.TypeNode parentInlinedProgram
        /// ~0.1% of cases
        | TemplateLiteralSpan of TemplateLiteralTypeSpan * Ts.TypeNode parentInlinedProgram
        /// ~0.1% of cases
        | Mapped of MappedType * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | NamedTuple of NamedTupleMember * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | TemplateLiteral of TemplateLiteralType * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | Rest of RestType * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | Import of ImportType * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | Constructor of ConstructorType * Ts.TypeNode parentInlinedProgram
        /// &lt;0.1% of cases
        | Optional of OptionalType * Ts.TypeNode parentInlinedProgram
        interface IInlinedProgram
        interface ICanHaveSymbol
        interface IEmbedded<ICanHaveSymbol>
        interface IAlwaysType
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                match this with
                | Type.Constructor (c, _) -> c :> ICanHaveTypeParameters |> _.TypeParameters
                | Type.Function (f, _) -> f :> ICanHaveTypeParameters |> _.TypeParameters
                | _ -> None
    
    [<Erase>]
    type ParameterDeclaration = ParameterDeclaration of Ts.ParameterDeclaration with
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IUnwrappable<Ts.ParameterDeclaration>
        static member inline op_Implicit(other: Ts.ParameterDeclaration) = ParameterDeclaration other
        static member inline op_Implicit(ParameterDeclaration other) = other
        
    [<Erase>]
    type BindingElement = BindingElement of Ts.BindingElement with
        interface INeverSymbol
        interface IEmbedded<ICanHaveSymbol>
        interface IUnwrappable<Ts.BindingElement>
        static member inline op_Implicit(other: Ts.BindingElement) = BindingElement other
        static member inline op_Implicit(BindingElement other) = other
    
    [<RequireQualifiedAccess>]
    type ParameterKind =
        | Simple of ParameterDeclaration parentInlinedProgram
        | Binding of BindingElement parentInlinedProgram
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<ICanHaveSymbol>
        interface IFastUnionUnwrappable<Ts.NamedDeclaration>
        
    [<RequireQualifiedAccess>]
    type SignatureKind =
        | Index of Ts.IndexSignatureDeclaration parentInlinedProgram
        | Call of Ts.CallSignatureDeclaration parentInlinedProgram // can have typars
        | Construct of Ts.ConstructSignatureDeclaration parentInlinedProgram // can have typars
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IFastUnionUnwrappable<Ts.SignatureDeclaration>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                !!this.Value
                |> ts.getEffectiveTypeParameterDeclarations
                |> NonEmptyArray.create

    [<RequireQualifiedAccess>]
    type PropertyKind =
        | Class of Ts.PropertyDeclaration parentInlinedProgram
        | Type of Ts.PropertySignature parentInlinedProgram
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IFastUnionUnwrappable<Ts.Node>

    [<RequireQualifiedAccess>]
    type MethodKind =
        | Class of Ts.MethodDeclaration parentInlinedProgram
        | Type of Ts.MethodSignature parentInlinedProgram
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IFastUnionUnwrappable<Ts.Node>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                !!this.Value
                |> ts.getEffectiveTypeParameterDeclarations
                |> NonEmptyArray.create
        
    [<RequireQualifiedAccess>]
    type ClassMemberKind =
        | Method of Ts.MethodDeclaration parentInlinedProgram // can have typars
        | Property of Ts.PropertyDeclaration parentInlinedProgram
        | GetAccessor of Ts.GetAccessorDeclaration parentInlinedProgram // can have typars
        | SetAccessor of Ts.SetAccessorDeclaration parentInlinedProgram // can have typars
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IFastUnionUnwrappable<Ts.Declaration>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                !!this.Value
                |> ts.getEffectiveTypeParameterDeclarations
                |> NonEmptyArray.create
    
    [<Erase>]
    type Script = Script of Ts.SourceFile parentInlinedProgram with
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<INeverSymbol>
        interface IUnwrappable<Ts.SourceFile>
    [<Erase>]
    type ExternalModule = ExternalModule of Ts.SourceFile parentInlinedProgram with
        interface IInlinedProgram
        interface IAlwaysSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IUnwrappable<Ts.SourceFile>
    
    [<RequireQualifiedAccess>]
    type SourceKind =
        | Script of Script
        | ExternalModule of ExternalModule
        interface IInlinedProgram
        interface ICanHaveSymbol
        interface IEmbedded<ICanHaveSymbol>
        interface IFastUnionUnwrappable<Ts.SourceFile>
        
    [<RequireQualifiedAccess; Erase>]
    type ModuleDeclaration = ModuleDeclaration of Ts.ModuleDeclaration parentInlinedProgram with
        interface IInlinedProgram
        interface IUnwrappable<Ts.ModuleDeclaration>
        interface IEmbedded<IAlwaysSymbol>
        interface INeverSymbol
    
    [<RequireQualifiedAccess>]
    type ModuleKind =
        | Declaration of ModuleDeclaration
        | Source of SourceKind
        interface IInlinedProgram
        interface IEmbedded<ICanHaveSymbol>
        interface ICanHaveSymbol
        
    [<RequireQualifiedAccess>]
    type ModuleMemberKind =
        | Variable of Ts.VariableDeclaration parentInlinedProgram
        | Function of Ts.FunctionDeclaration parentInlinedProgram // can have typars
        | Class of Ts.ClassDeclaration parentInlinedProgram // can have typars
        | Interface of Ts.InterfaceDeclaration parentInlinedProgram // can have typars
        | Enum of Ts.EnumDeclaration parentInlinedProgram
        | Module of Ts.ModuleDeclaration parentInlinedProgram
        | TypeAlias of Ts.TypeAliasDeclaration parentInlinedProgram // can have typars
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Declaration>
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                ts.getEffectiveTypeParameterDeclarations !!this.Value
                |> NonEmptyArray.create
    
    type EnumMember = EnumMember of Ts.EnumMember parentInlinedProgram with
        interface IInlinedProgram
        interface INeverSymbol
        interface IEmbedded<IAlwaysSymbol>
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<Erase>]
    type Variable =
        | Variable of Ts.VariableDeclaration inlinedProgram 
        interface IUnwrappable<Ts.VariableDeclaration>
        interface IInlinedProgram

    /// <summary>
    /// Wrapper for <c>Ts.Declaration</c>
    /// </summary>
    [<RequireQualifiedAccess>]
    type DeclarationKind =
        | Variable of Variable parentInlinedProgram
        | TypeAlias of Ts.TypeAliasDeclaration parentInlinedProgram // can have typars
        | Function of Ts.FunctionDeclaration parentInlinedProgram // can have typars
        | Parameter of ParameterKind
        | Interface of Ts.InterfaceDeclaration parentInlinedProgram // can have typars
        | Property of PropertyKind
        | Signature of SignatureKind // can have typars
        | Method of MethodKind // can have typars
        | TypeParameter of TypeParameterDeclaration
        | Module of ModuleKind
        | GetAccessor of Ts.GetAccessorDeclaration parentInlinedProgram // can have typars
        | SetAccessor of Ts.SetAccessorDeclaration parentInlinedProgram // can have typars
        | Class of Ts.ClassDeclaration parentInlinedProgram // can have typars
        | ImportSpecifier of Ts.ImportSpecifier parentInlinedProgram
        | ExportSpecifier of Ts.ExportSpecifier parentInlinedProgram
        | NamespaceImport of Ts.NamespaceImport parentInlinedProgram
        | ExportAssignment of Ts.ExportAssignment parentInlinedProgram
        | NamespaceExport of Ts.NamespaceExport parentInlinedProgram
        | Constructor of Ts.ConstructorDeclaration parentInlinedProgram
        | Enum of Ts.EnumDeclaration parentInlinedProgram
        | ImportClause of Ts.ImportClause parentInlinedProgram
        | EnumMember of EnumMember
        | ImportEquals of Ts.ImportEqualsDeclaration parentInlinedProgram
        | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration parentInlinedProgram
        /// <summary>
        /// These only appear in situations where the type node declaration site IS the canonical declaration site.
        /// This includes things like type literals, and anonymous functions.
        /// </summary>
        | Type of Type // can have typars
        interface IInlinedProgram
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                match this with
                | DeclarationKind.TypeAlias (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | DeclarationKind.Function (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | DeclarationKind.Interface (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | DeclarationKind.Signature t -> t :> ICanHaveTypeParameters |> _.TypeParameters
                | DeclarationKind.Method t -> t :> ICanHaveTypeParameters |> _.TypeParameters
                | DeclarationKind.Class (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | DeclarationKind.GetAccessor (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | DeclarationKind.SetAccessor (Unwrap t) -> !!t |> ts.getEffectiveTypeParameterDeclarations |> NonEmptyArray.create
                | _ -> None
    
    [<RequireQualifiedAccess>]
    type SemanticToken =
        | Spread
        | EoF
        | Minus
        | Optional
        | Asserts
        | PrivateField

    [<RequireQualifiedAccess>]
    type BindingPattern =
        | Object of Ts.ObjectBindingPattern parentInlinedProgram
        | Array of Ts.ArrayBindingPattern parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<RequireQualifiedAccess>]
    type Expression =
        | PrefixUnary of Ts.PrefixUnaryExpression parentInlinedProgram
        | PropertyAccess of Ts.PropertyAccessExpression parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Expression>
    
    [<RequireQualifiedAccess>]
    type TemplatePart =
        | Head of Ts.TemplateHead parentInlinedProgram
        | Middle of Ts.TemplateMiddle parentInlinedProgram
        | Tail of Ts.TemplateTail parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<Erase>]
    type ComputedPropertyName = ComputedPropertyName of Ts.ComputedPropertyName parentInlinedProgram with
        interface IUnwrappable<Ts.ComputedPropertyName>
        interface IInlinedProgram
    
    [<RequireQualifiedAccess>]
    type Identifier =
        | ComputedPropertyName of ComputedPropertyName
        | Identifier of Ts.Identifier parentInlinedProgram
        | QualifiedName of Ts.QualifiedName parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<RequireQualifiedAccess>]
    type JSDocIdentifier =
        | Identifier of Identifier
        | JSDoc of Ts.JSDocMemberName parentInlinedProgram
        interface IInlinedProgram
    
    [<RequireQualifiedAccess>]
    type Container =
        | VariableStatement of Ts.VariableStatement parentInlinedProgram
        | VariableDeclarationList of Ts.VariableDeclarationList parentInlinedProgram
        | ModuleBlock of Ts.ModuleBlock parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<RequireQualifiedAccess>]
    type HeritageClause =
        | Implements of NonEmptyArray<ExpressionWithTypeArguments> * Ts.HeritageClause parentInlinedProgram
        | Extends of NonEmptyArray<ExpressionWithTypeArguments> * Ts.HeritageClause parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<NonEmptyArray<ExpressionWithTypeArguments>>
    
    [<RequireQualifiedAccess>]
    type TypeHeritageClause =
        | Extends of NonEmptyArray<ExpressionWithTypeArguments> * Ts.HeritageClause parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<NonEmptyArray<ExpressionWithTypeArguments>>
    
    [<RequireQualifiedAccess>]
    type ClassLikeHeritageClause =
        | Implements of NonEmptyArray<ExpressionWithTypeArguments> * Ts.HeritageClause parentInlinedProgram
        | Extends of ExpressionWithTypeArguments * Ts.HeritageClause parentInlinedProgram
        | ImplementsAndExtends of
            implements: NonEmptyArray<ExpressionWithTypeArguments> *
            extends: ExpressionWithTypeArguments *
            implementsClause: Ts.HeritageClause *
            extendsClause: Ts.HeritageClause
        interface IInlinedProgram
    
    [<RequireQualifiedAccess>]
    type ImportExportControl =
        | ExportDeclaration of Ts.ExportDeclaration parentInlinedProgram
        | NamedExports of Ts.NamedExports parentInlinedProgram
        | ImportDeclaration of Ts.ImportDeclaration parentInlinedProgram
        | NamedImports of Ts.NamedImports parentInlinedProgram
        | ExternalModuleReference of Ts.ExternalModuleReference parentInlinedProgram
        interface IInlinedProgram
        interface IFastUnionUnwrappable<Ts.Node>
    
    [<RequireQualifiedAccess>]
    type Kind =
        | DeclarationOrType of DeclarationKind // can have typars
        | Modifier of ModifierKeyword
        | JSDoc of JSDoc
        | Semantic of SemanticToken
        | BindingPattern of BindingPattern
        | Expression of Expression
        | Identifier of Identifier
        | TemplatePart of TemplatePart
        | Container of Container
        | HeritageClause of HeritageClause
        | Literal of Literal
        | ImportExportControl of ImportExportControl
        | JSDocIdentifier of JSDocIdentifier
        interface ICanHaveTypeParameters with
            member this.TypeParameters =
                match this with
                | Kind.DeclarationOrType t -> t :> ICanHaveTypeParameters |> _.TypeParameters
                | _ -> None

module Type =
    [<RequireQualifiedAccess>]
    type TypeValidationError =
        /// <summary>
        /// This is a type error object without a symbol.
        /// </summary>
        | SymbolessErrorType
        /// <summary>
        /// This is a type object that still has a symbol (which is also be an unknown symbol kind).
        /// The storage of the information is despite the compiler diagnostic error for parsing the type.
        /// </summary>
        | ErrorType
    
    [<RequireQualifiedAccess>]
    type PrimitiveSingleton=
        | Any of Ts.Type
        | Unknown of Ts.Type
        | String of Ts.Type
        | Number of Ts.Type
        | Boolean of Ts.Type
        | BigInt of Ts.Type
        | ESSymbol of Ts.Type
        | Void of Ts.Type
        | Undefined of Ts.Type
        | Null of Ts.Type
        | Never of Ts.Type
        | NonPrimitive of Ts.Type
        interface INeverSymbol
        interface IFastUnionUnwrappable<Ts.Type>
        
    [<RequireQualifiedAccess>]
    type PrimitiveLiteral =
        | String of Ts.StringLiteralType
        | Number of Ts.NumberLiteralType
        | BigInt of Ts.BigIntLiteralType
        | Boolean of Ts.LiteralType
        interface INeverSymbol
        interface IFastUnionUnwrappable<Ts.LiteralType>
        
    [<RequireQualifiedAccess; Erase>]
    type UniqueESSymbol =
        | UniqueESSymbol of Ts.UniqueESSymbolType
        interface IAlwaysSymbol
        interface ICanHaveSymbol<Symbol.Property>
        interface ICanHaveSymbol<Symbol.Method>
        interface IUnwrappable<Ts.UniqueESSymbolType>
    
    type EnumMember = EnumMember of PrimitiveLiteral parentInlinedProgram with
        interface IAlwaysSymbol
        interface IInlinedProgram
        interface IAlwaysSymbol<Symbol.EnumMember>
        interface IFastUnionUnwrappable<PrimitiveLiteral>

    [<RequireQualifiedAccess>]
    type Literal =
        | UniqueESSymbol of UniqueESSymbol
        | PrimitiveLiteral of PrimitiveLiteral
        | EnumMember of EnumMember
        interface ICanHaveSymbol
        
    [<RequireQualifiedAccess>]
    type StringMapping =
        | Capitalize of Ts.StringMappingType
        | Lowercase of Ts.StringMappingType
        | Uppercase of Ts.StringMappingType
        | Uncapitalize of Ts.StringMappingType
        interface IAlwaysSymbol
        interface IAlwaysSymbol<Symbol.TypeAlias>
        interface IFastUnionUnwrappable<Ts.StringMappingType>
    [<RequireQualifiedAccess; Erase>]
    type TypeParameter = TypeParameter of Ts.TypeParameter with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.TypeParameter>
    [<RequireQualifiedAccess; Erase>]
    type Intersection = Intersection of Ts.IntersectionType with
        interface INeverSymbol
        interface IUnwrappable<Ts.IntersectionType>
    [<RequireQualifiedAccess; Erase>]
    type Union = Union of Ts.UnionType with
        interface IInlinedTypeChecker
        interface INeverSymbol
        interface IUnwrappable<Ts.UnionType>
    [<RequireQualifiedAccess; Erase>]
    type TemplateLiteral = TemplateLiteral of Ts.TemplateLiteralType with
        interface INeverSymbol
        interface IUnwrappable<Ts.TemplateLiteralType>
    [<RequireQualifiedAccess; Erase>]
    type Index = Index of Ts.IndexType with
        interface INeverSymbol
        interface IUnwrappable<Ts.IndexType>
        
    [<RequireQualifiedAccess>]
    type InstantiablePrimitive =
        | Index of Index
        | StringMapping of StringMapping
        | TemplateLiteral of TemplateLiteral
        interface ICanHaveSymbol
    [<RequireQualifiedAccess; Erase>]
    type Substitution = Substitution of Ts.SubstitutionType with
        interface INeverSymbol
        interface IUnwrappable<Ts.SubstitutionType>
    [<RequireQualifiedAccess; Erase>]
    type Conditional = Conditional of Ts.ConditionalType with
        interface INeverSymbol
        interface IUnwrappable<Ts.ConditionalType>
    [<RequireQualifiedAccess; Erase>]
    type IndexedAccess = IndexedAccess of Ts.IndexedAccessType with
        interface INeverSymbol
        interface IUnwrappable<Ts.IndexedAccessType>
        
    [<RequireQualifiedAccess>]
    type InstantiableNonPrimitive =
        | Conditional of Conditional
        | Substitution of Substitution
        | TypeParameter of TypeParameter
        | IndexedAccess of IndexedAccess
        interface ICanHaveSymbol
        interface IFastUnionUnwrappable<Ts.InstantiableType>
        
    [<RequireQualifiedAccess>]
    type Instantiable =
        | Primitive of InstantiablePrimitive
        | NonPrimitive of InstantiableNonPrimitive
        interface INeverSymbol
    
    [<RequireQualifiedAccess>]
    type Primitive =
        | Singleton of PrimitiveSingleton
        | Literal of Literal
        interface ICanHaveSymbol

    [<RequireQualifiedAccess; Erase>]
    type Interface = Interface of Ts.InterfaceType with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.InterfaceType>
    
    [<RequireQualifiedAccess; Erase>]
    type Class = Class of Ts.InterfaceType with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.InterfaceType>
    
    [<RequireQualifiedAccess; Erase>]
    type PureTypeReference = TypeReference of Ts.TypeReference with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.TypeReference>
        
    [<RequireQualifiedAccess; Erase>]
    type InterfaceReference = InterfaceReference of Ts.TypeReference with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.TypeReference>
        
    [<RequireQualifiedAccess; Erase>]
    type ClassReference = ClassReference of Ts.TypeReference with
        interface IAlwaysSymbol
        interface IUnwrappable<Ts.TypeReference>
        
    [<RequireQualifiedAccess; Erase>]
    type ArrayReference = ArrayReference of Ts.TypeReference with
        interface INeverSymbol
        interface IUnwrappable<Ts.TypeReference>
        
    [<RequireQualifiedAccess; Erase>]
    type TupleReference = TupleReference of Ts.TypeReference with
        interface INeverSymbol
        interface IUnwrappable<Ts.TypeReference>
    
    [<RequireQualifiedAccess>]
    type TypeReference =
        | Pure of PureTypeReference
        | Array of ArrayReference
        | Tuple of TupleReference
        | Interface of InterfaceReference
        | Class of ClassReference
        interface ICanHaveSymbol
        interface IFastUnionUnwrappable<Ts.TypeReference>
        
    [<RequireQualifiedAccess>]
    type Anonymous =
        | ObjectRest of Ts.ObjectType
        | InstantiationExpression of Ts.ObjectType
        | Anonymous of Ts.ObjectType
        | Instantiated of Ts.ObjectType
        interface IAlwaysSymbol
        interface IFastUnionUnwrappable<Ts.ObjectType>
    
    [<RequireQualifiedAccess>]
    type Mapped =
        | Mapped of Ts.ObjectType
        | Instantiated of Ts.ObjectType
        interface IAlwaysSymbol<Symbol.TypeLiteral>
        interface IFastUnionUnwrappable<Ts.ObjectType>
    
    [<RequireQualifiedAccess>]
    type Structural =
        | Mapped of Mapped
        | Union of Union
        | Intersection of Intersection
        | Anonymous of Anonymous
        | TypeReference of TypeReference
        | Interface of Interface
        | Class of Class
        interface ICanHaveSymbol
    
    [<RequireQualifiedAccess; Erase>]
    type Enum = Enum of Ts.EnumType inlinedProgram with
        interface ICanHaveSymbol<Symbol.TypeEnum>
        interface ICanHaveSymbol<Symbol.ConstEnum>
        interface IAlwaysSymbol<Symbol.IEnum>
        interface IInlinedProgram
        interface IUnwrappable<Ts.EnumType>
    
    [<RequireQualifiedAccess>]
    type Kind =
        | Primitive of Primitive
        | Structural of Structural
        | Instantiable of Instantiable
        | Enum of Enum
        interface ICanHaveSymbol

    

[<EditorBrowsable(EditorBrowsableState.Never)>]
module InternalTracer =
    type ProgramTracerMap =
        inherit Tracer<Ts.Program>
    let private symbolMapKey =
        SymbolTypeKey.create<Dictionary<UniqueIdentifiers.SymbolKey, Tracer<Symbol.Kind>>> "SymbolMap"
    let private nodeMapKey =
        SymbolTypeKey.create<Dictionary<UniqueIdentifiers.NodeKey, Tracer<Node.Kind>>> "NodeMap"
    let private typeMapKey =
        SymbolTypeKey.create<Dictionary<UniqueIdentifiers.TypeKey, Tracer<Type.Kind>>> "TypeMap"
    type ProgramTracerMap with
        member this.SymbolMap =
            SymbolTypeKey.accessOrInit symbolMapKey (fun () -> Dictionary()) this
        member this.NodeMap =
            SymbolTypeKey.accessOrInit nodeMapKey (fun () -> Dictionary()) this
        member this.TypeMap =
            SymbolTypeKey.accessOrInit typeMapKey (fun () -> Dictionary()) this

[<AutoOpen>]
module Tracers =
    open Symbol
    /// Only use with Unions where the narrowed value is in the first field of the union.
    [<Interface>]
    type NarrowedTracer<'MainKind, 'NarrowedValue> =
        inherit Tracer<'MainKind>
    module NarrowedTracer =
        type Handlers<'T, 'O, 'MainKind, 'NarrowedValue when 'O :> Tracer<'MainKind> and 'T :> NarrowedTracer<'MainKind, 'NarrowedValue>> = {
            tryGet: 'O -> 'T option
            value: 'T -> 'NarrowedValue
        }
        
        
    type SymbolTracer =
        inherit Tracer<Kind>
        abstract SymbolKey: SymbolKey with get,set
        abstract program: Ts.Program with get,set
        abstract checker: Ts.TypeChecker with get,set
    type SymbolTypeParameterTracer =
        inherit NarrowedTracer<Kind, Choice<TypeParameter, Transient.TypeParameter>>
        inherit SymbolTracer
    type SymbolClassLikeTracer =
        inherit NarrowedTracer<Kind, Transient.IClass>
        inherit SymbolTracer
    type SymbolParameterTracer =
        // we've come across one case in the corpus where the canonical type is a type parameter
        inherit NarrowedTracer<Kind, IParameter>
        inherit SymbolTracer
    type SymbolUniqueESSymbolTracer =
        inherit NarrowedTracer<Kind, Choice<Parameter, Property>>
        inherit SymbolTracer
        
    type TypeTracer =
        inherit Tracer<Type.Kind>
        abstract TypeKey: TypeKey with get,set
        abstract program: Ts.Program with get,set
        abstract checker: Ts.TypeChecker with get,set
    type TypeTypeParameterTracer =
        inherit NarrowedTracer<Type.Kind, Type.TypeParameter>
        inherit TypeTracer
    type TypeClassTracer =
        inherit NarrowedTracer<Type.Kind, Choice<Type.Class, Type.ClassReference>>
        inherit TypeTracer
    type TypeUniqueESSymbolTracer =
        inherit NarrowedTracer<Type.Kind, Type.UniqueESSymbol>
        inherit TypeTracer
        
    type NodeTracer =
        inherit Tracer<Node.Kind>
        abstract NodeKey: NodeKey with get,set
        abstract program: Ts.Program with get,set
        abstract checker: Ts.TypeChecker with get,set
    type NodeDeclarationKindTracer =
        inherit NarrowedTracer<Node.Kind, Node.DeclarationKind>
        inherit NodeTracer
    type NodeTypeParameterTracer =
        inherit NarrowedTracer<Node.Kind, Node.TypeParameterDeclaration>
        inherit NodeTracer
    type NodeTypeTracer =
        inherit NarrowedTracer<Node.Kind, Node.Type>
        inherit NodeTracer
    type NodeClassTracer =
        inherit NarrowedTracer<Node.Kind, Ts.ClassDeclaration>
        inherit NodeTracer
    type NodeParameterTracer =
        inherit NarrowedTracer<Node.Kind, Node.ParameterDeclaration>
        inherit NodeTracer
    type NodeUniqueESSymbolTracer =
        inherit NarrowedTracer<Node.Kind, Choice<Node.PropertyKind, Node.Variable>>
        inherit NodeTracer

/// <summary>
/// Symbol, node, and type information is precollected and preserved where possible across all three
/// hierarchies.
/// Where this is not possible, the type should be explicit in not containing the field.
/// We defer to the most unique identifier available as the container. sub declarations et al must be contained
/// within that parent as an array/list.
/// </summary>
module Wrapped =
    type PrimitiveKind =
        | String
        | Number
        | BigInt
        | Any
        | Unknown
        | Never
        | Undefined
        | Void
        | Null
        | Boolean
        | ESSymbol
        | NonPrimitive
        | Intrinsic
        
    type TypeParameter = {
        Key: UniqueIdentifiers.CompositeKey
        Node: NodeTypeParameterTracer
        Type: TypeTypeParameterTracer
        Symbol: SymbolTypeParameterTracer
        AliasNodes: NonEmptyArray<NodeTypeParameterTracer> option
    }
        
        
module Root =
    module Symbol = Symbol
    module Type = Type
    module Node = Node
open Symbol
open Node
// Implementations for types MUST be defined in the Core module.
// Many operations will utilise operations from other types in traversals.
// For this reason, they need to be in a recursive module/namespace.
[<AutoOpen>]
module rec Core =
    module ISymbol =
        let inline symbolKey (symbol: ISymbol) = SymbolKey.fromISymbol symbol
        let inline program (symbol: ISymbol) = SymbolTypeKeys.Program.get symbol
        let inline checker (symbol: ISymbol) = SymbolTypeKeys.TypeChecker.get symbol
        let inline toSymbol (symbol: ISymbol) = unbox<Ts.Symbol> symbol 
        let inline hasFlag (flag: Ts.SymbolFlags) = toSymbol >> _.flags.HasFlag(flag)
        let inline name (symbol: ISymbol) = toSymbol symbol |> _.escapedName |> SymbolName.Create
        let create (program: Ts.Program) (symbol: Ts.Symbol) =
            let checker = program.getTypeChecker()
            if symbol.flags.HasFlag(Ts.SymbolFlags.Alias) then
                checker.getAliasedSymbol(symbol)
            else symbol
            |> checker.getMergedSymbol
            |> unbox<ISymbol>
            |> SymbolTypeKeys.Program.addIfAbsent program
            |> SymbolTypeKeys.TypeChecker.addIfAbsent checker
        let declarations symbol =
            toSymbol symbol
            |> _.getDeclarations()
            |> Option.filter (_.AsArray >> Array.isEmpty >> not)
            |> Option.map _.AsArray
            |> Option.orElse (toSymbol symbol |> _.valueDeclaration |> Option.map Array.singleton)
            |> Option.orElse (
                checker symbol
                |> _.getRootSymbols(toSymbol symbol)
                |> _.AsArray
                |> Array.tryPick(
                    _.getDeclarations()
                    >> Option.bind (_.AsArray >> NonEmptyArray.create)
                    >> Option.map _.Values
                    )
                )
            |> Option.bind NonEmptyArray.create
        let tryPickDeclaration (fn: Ts.Declaration -> 'T option) symbol =
            declarations symbol
            |> Option.bind (NonEmptyArray.tryPick fn)
        let chooseDeclarations (fn: Ts.Declaration -> 'T option) symbol =
            declarations symbol
            |> Option.bind (NonEmptyArray.choose fn)
        let declaredType symbol =
            toSymbol symbol |> (checker symbol).getDeclaredTypeOfSymbol
        let getTypes symbol =
            declarations symbol
            |> Option.map (NonEmptyArray.map (fun node -> (checker symbol).getTypeOfSymbolAtLocation(symbol, node)))
    
    [<RequireQualifiedAccess>]
    module Node =
        /// <summary>
        /// Operator to simplify <c>Ts.SyntaxKind</c> to <c>Node</c> constructor maps.
        /// Majority of the time, our constructors take two parameters, the program and the node. The program
        /// is usually injected into the node at some point. Where this isn't required, we can just ignore the first
        /// parameter in the constructor.<br/><br/>
        /// Left side is the syntax kind which maps to the constructor on the right side. The constructor must take
        /// two parameters, the program and the node.
        /// </summary>
        let inline private (==>) a (b: Ts.Program -> 'NodeType -> 'FinalKind): KeyValuePair<Ts.SyntaxKind, Ts.Program -> obj -> 'FinalKind> = KeyValuePair(a, unbox b)
        /// <summary>
        /// Operator to simplify <c>Ts.SyntaxKind</c> to <c>Node</c> constructor maps.
        /// See <c>==&gt;</c>.
        /// This variant is for constructing nodes that are fieldless (such as repr of keywords) while maintaining
        /// the 2 parameter constructor signature.
        /// </summary>
        let inline private (==>!) a (b: 'FinalKind): KeyValuePair<Ts.SyntaxKind, Ts.Program -> obj -> 'FinalKind> = KeyValuePair(a, fun _ _ -> b)
        /// <summary>
        /// Utility function to simplify embeddeding the constructor map of one node type in another.
        /// You must ensure to <c>yield!</c> the result in your constructor map.
        /// The first parameter is the constructor map to embed, and the second parameter is the transformer which
        /// wraps the result of the embedded constructor map into a constructor of the new constructor map.
        /// </summary>
        let inline private fetchMap (kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> 'IntermediateKind>) (finaliser: Ts.Program -> 'IntermediateKind -> 'FinalKind) = seq {
            for KeyValue(k, v) in kindMap do
                k ==> fun program node -> v program node |> finaliser program
        }
        module SemanticToken =
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.QuestionToken ==>! SemanticToken.Optional
                Ts.SyntaxKind.AssertsKeyword ==>! SemanticToken.Asserts
                Ts.SyntaxKind.DotDotDotToken ==>! SemanticToken.Spread
                Ts.SyntaxKind.EndOfFileToken ==>! SemanticToken.EoF
                Ts.SyntaxKind.MinusToken ==>! SemanticToken.Minus
                Ts.SyntaxKind.PrivateIdentifier ==>! SemanticToken.PrivateField
            ]
            let kindSet = kindMap.Keys |> Set
            let isSemanticToken (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program (node: Ts.Node) =
                if not <| isSemanticToken node then None else
                kindMap[node.kind] program node |> Some
            let unsafeCreate (node: Ts.Node) =
                tryCreate JS.undefined node |> Option.defaultWith (fun () -> failwithf "Could not create semantic token from node %A" node)
        module BindingPattern =
            let fromObjectBinding program (node: Ts.ObjectBindingPattern) =
                ParentInlinedProgram.wrap program BindingPattern.Object node
            let fromArrayBinding program (node: Ts.ArrayBindingPattern) =
                ParentInlinedProgram.wrap program BindingPattern.Array node
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> BindingPattern> = Dictionary [
                Ts.SyntaxKind.ObjectBindingPattern ==> fromObjectBinding
                Ts.SyntaxKind.ArrayBindingPattern ==> fromArrayBinding
            ]
            let kindSet = kindMap.Keys |> Set
            let isBindingPattern (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isBindingPattern node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create binding pattern from node %A" node)
        module Expression =
            let fromPrefixUnary program (node: Ts.PrefixUnaryExpression) =
                ParentInlinedProgram.wrap program Expression.PrefixUnary node
            let fromPropertyAccess program (node: Ts.PropertyAccessExpression) =
                ParentInlinedProgram.wrap program Expression.PropertyAccess node
            let internal kindMap = Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Expression> [
                Ts.SyntaxKind.PrefixUnaryExpression ==> fromPrefixUnary
                Ts.SyntaxKind.PropertyAccessExpression ==> fromPropertyAccess
            ]
            let kindSet = kindMap.Keys |> Set
            let isExpression (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isExpression node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create expression from node %A" node)
        module TemplatePart =
            let fromTemplateHead program (node: Ts.TemplateHead) =
                ParentInlinedProgram.wrap program TemplatePart.Head node
            let fromTemplateMiddle program (node: Ts.TemplateMiddle) =
                ParentInlinedProgram.wrap program TemplatePart.Middle node
            let fromTemplateTail program (node: Ts.TemplateTail) =
                ParentInlinedProgram.wrap program TemplatePart.Tail node
                
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> TemplatePart> = Dictionary [
                Ts.SyntaxKind.TemplateHead ==> fromTemplateHead
                Ts.SyntaxKind.TemplateMiddle ==> fromTemplateMiddle
                Ts.SyntaxKind.TemplateTail ==> fromTemplateTail
            ]
            let kindSet = kindMap.Keys |> Set
            let isTemplatePart (node: Ts.Node) = kindSet.Contains node.kind 
            let tryCreate program node =
                if not <| isTemplatePart node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create template part from node %A" node)
        module ComputedPropertyName =
            let isComputedPropertyName = ts.isComputedPropertyName
            let create program (node: Ts.ComputedPropertyName) =
                ParentInlinedProgram.wrap program ComputedPropertyName.ComputedPropertyName node
            let tryCreate program node =
                if not <| isComputedPropertyName node then None else
                create program !!node |> Some
            let unsafeCreate program (node: Ts.Node) =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create computed property name from node %A" node.kind.Name)
            let expression (prop: ComputedPropertyName) =
                match prop.Value.expression with
                | Patterns.Node.PropertyAccessExpression node -> Choice1Of2 node
                | Patterns.Node.Identifier node -> Choice2Of2 node
                | expr -> failwithf "Computed property name %A is not an expression we were expecting" expr.kind.Name
                
        module Identifier =
            let fromComputedPropertyName program (node: Ts.ComputedPropertyName) =
                ComputedPropertyName.create program node
                |> Identifier.ComputedPropertyName 
            let fromIdentifier program (node: Ts.Identifier) =
                ParentInlinedProgram.wrap program Identifier.Identifier node
            let fromQualifiedName program (node: Ts.QualifiedName) =
                ParentInlinedProgram.wrap program Identifier.QualifiedName node
            let fromEntityName program (node: Ts.EntityName) =
                match node with
                | Patterns.Node.EntityNamePatterns.Identifier node -> fromIdentifier program node
                | Patterns.Node.EntityNamePatterns.QualifiedName node -> fromQualifiedName program node
                
            let internal kindMap = Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Identifier> [
                Ts.SyntaxKind.ComputedPropertyName ==> fromComputedPropertyName
                Ts.SyntaxKind.Identifier ==> fromIdentifier
                Ts.SyntaxKind.QualifiedName ==> fromQualifiedName
            ]
            let kindSet = kindMap.Keys |> Set
            let isIdentifier (node: Ts.Node) = kindSet.Contains node.kind 
            let tryCreate program node =
                if not <| isIdentifier node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create identifier from node %A" node)
            let toNode = function
                | Identifier.ComputedPropertyName value -> value.Value :> Ts.Node
                | Identifier.Identifier value -> value.Value :> Ts.Node
                | Identifier.QualifiedName value -> value.Value :> Ts.Node
            let getSymbolKind identifier =
                let node = toNode identifier
                identifier.checker.getSymbolAtLocation(node)
                |> Option.map (Symbol.Kind.create identifier.program)
            let inline private identifierToString (identifier: Ts.Identifier) = identifier.text
            let flattenToString (identifier: Identifier) =
                match identifier with
                | Identifier.Identifier value -> value.Value |> identifierToString
                | Identifier.QualifiedName value ->
                    value.Value
                    |> _.right
                    |> identifierToString
                | Identifier.ComputedPropertyName value -> value.Value |> _.getText()
            let flattenToStringArray (identifier: Identifier) =
                match identifier with
                | Identifier.Identifier value -> [| value.Value |> identifierToString |]
                | Identifier.QualifiedName value ->
                    let qualification =
                        Some value.Value.left
                        |> Array.unfold (function
                            | Some (Patterns.Node.EntityNamePatterns.Identifier node) -> Some (node.text, None)
                            | Some (Patterns.Node.EntityNamePatterns.QualifiedName node) -> Some (node.right.text, Some node.left)
                            | None -> None
                            )
                    let ident = value.Value.right.text
                    qualification
                    |> Array.insertAt 0 ident
                    |> Array.rev
                | Identifier.ComputedPropertyName value -> [| value.Value.getText() |]
            let flattenToStringWithQualification (identifier: Identifier) =
                flattenToStringArray identifier
                |> String.concat "."
            
        module JSDocIdentifier =
            let fromJSDocMemberName program (node: Ts.JSDocMemberName) =
                ParentInlinedProgram.wrap program JSDocIdentifier.JSDoc node
            let fromIdentifier program (identifier: Identifier) =
                InlinedProgram.create program (JSDocIdentifier.Identifier identifier)
                |> _.Value
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.JSDocMemberName ==> fromJSDocMemberName
                yield! fetchMap Identifier.kindMap fromIdentifier
            ]
            let kindSet = kindMap.Keys |> Set
            let isJSDocIdentifier (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isJSDocIdentifier node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create JSDoc identifier from node %A" node)
            let toNode = function
                | JSDocIdentifier.JSDoc value -> value.Value :> Ts.Node
                | JSDocIdentifier.Identifier value -> Identifier.toNode value
            let tryGetSymbolKind identifier =
                let node = toNode identifier
                identifier.checker.getSymbolAtLocation(node)
                |> Option.map (Symbol.Kind.create identifier.program)
            let inline private jsdocMemberNameToString (node: Ts.JSDocMemberName) = node.right.text
            let flattenToString (identifier: JSDocIdentifier) =
                match identifier with
                | JSDocIdentifier.JSDoc value -> value.Value |> jsdocMemberNameToString
                | JSDocIdentifier.Identifier value -> Identifier.flattenToString value
            module private Patterns =
                let inline (|Identifier|QualifiedName|JSDocMemberName|) (value: U2<Ts.EntityName, Ts.JSDocMemberName>) =
                    match unbox<Ts.Node> value with
                    | Patterns.Node.Identifier node -> Identifier node
                    | Patterns.Node.QualifiedName node -> QualifiedName node
                    | Patterns.Node.JSDocMemberName node -> JSDocMemberName node
                    | _ -> failwithf "Expected identifier, qualified name, or jsdoc member name, got %A" (unbox<Ts.Node> value).kind.Name
            let flattenToStringArray (identifier: JSDocIdentifier) =
                match identifier with
                | JSDocIdentifier.JSDoc value ->
                    let qualification =
                        Some value.Value.left
                        |> Array.unfold (function
                            | Some (Patterns.Identifier node) -> Some (node.text, None)
                            | Some (Patterns.QualifiedName node) -> Some (node.right.text, U2.Case1 node.left |> Some)
                            | Some (Patterns.JSDocMemberName node) -> Some (node.right.text, Some node.left)
                            | None -> None
                            )
                    let ident = value.Value.right.text
                    qualification
                    |> Array.insertAt 0 ident
                    |> Array.rev
                | JSDocIdentifier.Identifier value -> value |> Identifier.flattenToStringArray
            let flattenToStringWithQualification (identifier: JSDocIdentifier) =
                flattenToStringArray identifier
                |> String.concat "."

        module Container =
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Container> = Dictionary [
                Ts.SyntaxKind.VariableStatement ==> fun program -> ParentInlinedProgram.wrap program Container.VariableStatement
                Ts.SyntaxKind.VariableDeclarationList ==> fun program -> ParentInlinedProgram.wrap program Container.VariableDeclarationList
                Ts.SyntaxKind.ModuleBlock ==> fun program -> ParentInlinedProgram.wrap program Container.ModuleBlock
            ]
            let kindSet = kindMap.Keys |> Set
            let isContainer (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isContainer node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create container from node %A" node)
        module HeritageClause =
            let create program (node: Ts.HeritageClause) =
                let payload =
                    node.types.AsArray
                    |> Array.map (Node.ExpressionWithTypeArguments.create program)
                    |> NonEmptyArray.create
                    |> Option.defaultWith (fun () -> failwith "Expected at least one type in heritage clause, got none.")
                match node.token with
                | Ts.SyntaxKind.ExtendsKeyword ->
                    ParentInlinedProgram.wrap program (fun node -> HeritageClause.Extends(payload, node)) node
                | Ts.SyntaxKind.ImplementsKeyword ->
                    ParentInlinedProgram.wrap program (fun node -> HeritageClause.Implements(payload, node)) node
                | _ -> failwithf "Unexpected heritage clause token %A" node.token.Name
            let tryCreate program (node: Ts.Node) =
                if not <| node.kind.Equals(Ts.SyntaxKind.HeritageClause) then None else
                node :?> Ts.HeritageClause
                |> create program
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create heritage clause from node %A" node.kind.Name)
            let heritageClause = function
                | HeritageClause.Extends(_,Unwrap node) 
                | HeritageClause.Implements(_,Unwrap node) -> node
            let types = function
                | HeritageClause.Extends(types, _)
                | HeritageClause.Implements(types, _) -> types
            let private parent = heritageClause >> _.parent
            let parentIsInterface = parent >> unbox<Ts.Node> >> _.kind.Equals(Ts.SyntaxKind.InterfaceDeclaration)
            let parentIsClass = parentIsInterface >> not
        module TypeHeritageClause =
            let private validateHeritageClause = function
                | HeritageClause.Extends(expressions, original) as input when HeritageClause.parentIsInterface input -> Some(expressions, original)
                | _ -> None
            let tryFromHeritageClause heritageClause =
                match validateHeritageClause heritageClause with
                | Some payload ->
                    TypeHeritageClause.Extends payload
                    |> InlinedProgram.inject heritageClause.program
                    |> Some
                | _ -> None
            let tryFromHeritageClauses heritageClauses =
                if NonEmptyArray.length heritageClauses > 1 then None else
                tryFromHeritageClause heritageClauses.Value
            let unsafeFromHeritageClause =
                tryFromHeritageClause
                >> Option.defaultWith (fun () -> failwith "Expected a valid interface heritage clause, got a class heritage clause")
            let unsafeFromHeritageClauses =
                tryFromHeritageClauses
                >> Option.defaultWith (fun () -> failwith "Expected a valid interface heritage clause, got multiple interface heritage clauses")
            let tryCreate program node =
                HeritageClause.tryCreate program node
                |> Option.bind tryFromHeritageClause
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create type heritage clause from node %A" node.kind.Name)
            let types = function
                | TypeHeritageClause.Extends(expressions, _) -> expressions
            let heritageClause = function
                | TypeHeritageClause.Extends(_, Unwrap node) -> node
            let parent = heritageClause >> _.parent >> unbox<Ts.InterfaceDeclaration>
        module ClassLikeHeritageClause =
            let private validateHeritageClause = function
                | HeritageClause.Extends(expressions, original) as input when HeritageClause.parentIsClass input ->
                    let head,tail = NonEmptyArray.popHead expressions
                    tail
                    |> Option.iter (fun _ ->
                        failwith "Expected a single type in class heritage extends clause, got multiple types")
                    Choice1Of2(head, original)
                    |> Some
                | HeritageClause.Implements(expressions, original) as input when HeritageClause.parentIsClass input ->
                    Choice2Of2(expressions, original)
                    |> Some
                | _ -> None
            let tryFromHeritageClause heritageClause =
                validateHeritageClause heritageClause
                |> Option.map (function
                    | Choice1Of2 payload ->
                        ClassLikeHeritageClause.Extends payload
                        |> InlinedProgram.inject heritageClause.program
                    | Choice2Of2 payload ->
                        ClassLikeHeritageClause.Implements payload
                        |> InlinedProgram.inject heritageClause.program
                    )
            let tryFromHeritageClauses heritageClauses =
                let head, tail = NonEmptyArray.popHead heritageClauses
                match tail |> Option.map _.Value with
                | Some clause ->
                    match validateHeritageClause head, validateHeritageClause clause with
                    | Some _, None ->
                        tryFromHeritageClause head
                    | None, Some _ ->
                        tryFromHeritageClause clause
                    | None, None -> None
                    | Some l, Some r ->
                        match l, r with
                        | Choice1Of2 extendsPayload, Choice2Of2 implementsPayload
                        | Choice2Of2 implementsPayload, Choice1Of2 extendsPayload ->
                            ClassLikeHeritageClause.ImplementsAndExtends(
                                fst implementsPayload,
                                fst extendsPayload,
                                (snd implementsPayload).Value,
                                (snd extendsPayload).Value
                            )
                            |> InlinedProgram.inject head.program
                            |> Some
                        | Choice2Of2 implementsPayload, Choice2Of2 implementsPayload2 ->
                            ClassLikeHeritageClause.Implements(
                                NonEmptyArray.append (fst implementsPayload) (fst implementsPayload2),
                                snd implementsPayload
                                )
                            |> InlinedProgram.inject head.program
                            |> Some
                        | _, _ ->
                            failwith "Expected a valid class heritage clause, got two cases for extends instead."
                | None ->
                    tryFromHeritageClause head
            let tryFromCurriedHeritageClauses clause clause2 =
                NonEmptyArray.create [ clause; clause2 ]
                |> Option.bind tryFromHeritageClauses
            let implements = function
                | ClassLikeHeritageClause.Implements(types, _)
                | ClassLikeHeritageClause.ImplementsAndExtends(implements = types) -> Some types
                | _ -> None
            let extends = function
                | ClassLikeHeritageClause.Extends(typ, _)
                | ClassLikeHeritageClause.ImplementsAndExtends(extends = typ) -> Some typ
                | _ -> None
            let heritageClause = function
                | ClassLikeHeritageClause.Extends(_, Unwrap node)
                | ClassLikeHeritageClause.Implements(_, Unwrap node)
                | ClassLikeHeritageClause.ImplementsAndExtends(implementsClause = node) -> node
            let parent = heritageClause >> _.parent >> unbox<Ts.ClassLikeDeclaration>
        module Literal =
            let fromStringLiteral program (node: Ts.StringLiteral) =
                ParentInlinedProgram.wrap program Literal.String node
            let fromNumericLiteral program (node: Ts.NumericLiteral) =
                ParentInlinedProgram.wrap program Literal.Numeric node
            let fromBigIntLiteral program (node: Ts.BigIntLiteral) =
                ParentInlinedProgram.wrap program Literal.BigInt node
            let fromBooleanLiteral program (node: Ts.BooleanLiteral) =
                match unbox<Ts.Node> node with
                | Patterns.SyntaxKind.TrueKeyword _ -> ParentInlinedProgram.wrap program Literal.Boolean true
                | _ -> ParentInlinedProgram.wrap program Literal.Boolean false
            let fromNullLiteral program (_: Ts.NullLiteral) =
                InlinedProgram.create program Literal.Null
                |> _.Value
            let fromNoSubstitutionTemplateLiteral program (node: Ts.NoSubstitutionTemplateLiteral) =
                ParentInlinedProgram.wrap program Literal.NoSubstitutionTemplateLiteral node
            
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Literal> = Dictionary [
                Ts.SyntaxKind.StringLiteral ==> fromStringLiteral
                Ts.SyntaxKind.NumericLiteral ==> fromNumericLiteral
                Ts.SyntaxKind.BigIntLiteral ==> fromBigIntLiteral
                Ts.SyntaxKind.TrueKeyword ==> fromBooleanLiteral
                Ts.SyntaxKind.FalseKeyword ==> fromBooleanLiteral
                Ts.SyntaxKind.NullKeyword ==> fromNullLiteral
                Ts.SyntaxKind.NoSubstitutionTemplateLiteral ==> fromNoSubstitutionTemplateLiteral
            ]
            let kindSet = kindMap.Keys |> Set
            
            let isLiteral (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isLiteral node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create literal from node %A" node.kind.Name)
        module Kind =
            let fromModifier _ node = Kind.Modifier node
            let fromSemanticToken _ node = Kind.Semantic node
            let fromBindingPattern _ node = Kind.BindingPattern node
            let fromExpression _ node = Kind.Expression node
            let fromIdentifier _ node = Kind.Identifier node
            let fromTemplatePart _ node = Kind.TemplatePart node
            let fromJSDoc _ node = Kind.JSDoc node
            let fromContainer _ node = Kind.Container node
            let private createHeritageClause program node = HeritageClause.create program node |> Kind.HeritageClause
            let fromLiteral _ node = Kind.Literal node
            let fromImportExportControl _ node = Kind.ImportExportControl node
            let fromJSDocIdentifier _ node = Kind.JSDocIdentifier node
            let fromDeclarationKind _ node = Kind.DeclarationOrType node
            let internal kindMap = Dictionary [
                yield! fetchMap DeclarationKind.kindMap fromDeclarationKind
                yield! fetchMap ModifierKeyword.kindMap fromModifier
                yield! fetchMap SemanticToken.kindMap fromSemanticToken
                yield! fetchMap BindingPattern.kindMap fromBindingPattern
                yield! fetchMap Expression.kindMap fromExpression
                yield! fetchMap Identifier.kindMap fromIdentifier
                yield! fetchMap TemplatePart.kindMap fromTemplatePart
                yield! fetchMap JSDoc.kindMap fromJSDoc
                yield! fetchMap Container.kindMap fromContainer
                Ts.SyntaxKind.HeritageClause ==> createHeritageClause
                yield! fetchMap Literal.kindMap fromLiteral
                yield! fetchMap ImportExportControl.kindMap fromImportExportControl
                // This is a superset of Identifier. The distinction is present because
                // not all identifiers in the JSDocs resolve to symbols unlike the rest of the identifiers.
                for KeyValue(k, v) in JSDocIdentifier.kindMap do
                    if Identifier.kindMap.ContainsKey k |> not then
                        k ==> (v >> fromJSDocIdentifier)
            ]
            let kindSet = kindMap.Keys |> Set
            let isKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isKind node then None else
                kindMap[node.kind] program node
                |> Some
            let create program node =
                tryCreate program node |> Option.defaultWith (fun () -> failwithf "Could not create kind from node %A" node.kind.Name)
        module CommentLink =
            let private flattenLinkName (linkName: U2<Ts.EntityName, Ts.JSDocMemberName> option) =
                let rec flattenEntityName acc = function
                    | Patterns.Node.EntityNamePatterns.Identifier identifier ->
                        identifier.text :: acc
                    | Patterns.Node.EntityNamePatterns.QualifiedName qualifiedName ->
                        qualifiedName.right.text :: (flattenEntityName acc qualifiedName.left)
                and flattenQualifiedName acc (qualifiedName: Ts.QualifiedName) =
                    qualifiedName.right.text :: (flattenEntityName acc qualifiedName.left)
                and flattenJSDocMemberName acc (jsdoc: Ts.JSDocMemberName) =
                    match unbox jsdoc.left with
                    | Patterns.Node.JSDocMemberName node ->
                        jsdoc.right.text :: (flattenJSDocMemberName acc node)
                    | node ->
                        match unbox<Ts.EntityName> node with
                        | Patterns.Node.EntityNamePatterns.Identifier identifier ->
                            jsdoc.right.text :: identifier.text :: acc
                        | Patterns.Node.EntityNamePatterns.QualifiedName qualifiedName ->
                            jsdoc.right.text :: (flattenQualifiedName acc qualifiedName)
                linkName
                |> Option.map (unbox >> function
                    | Patterns.Node.Identifier _ | Patterns.Node.QualifiedName _ as node ->
                        flattenEntityName [] (unbox node)
                        |> List.rev
                    | node ->
                        unbox node |> flattenJSDocMemberName []
                        |> List.rev
                    )
            let unsafeCreate (program: Ts.Program) = function
                | Patterns.Node.JSDocLink _ | Patterns.Node.JSDocLinkCode _ | Patterns.Node.JSDocLinkPlain _ as node ->
                    let node = node :> Ts.JSDocLink
                    match 
                        node.name
                        |> Option.bind (unbox<Ts.Node> >> program.getTypeChecker().getSymbolAtLocation)
                    with
                    | Some symbol ->
                        Symbol.createKind program symbol
                        |> Choice1Of2
                        |> Some
                    | None ->
                        node.name
                        |> flattenLinkName
                        |> Option.map (String.concat "." >> Choice2Of2)
                    |> fun name ->
                        {
                            Text =
                                if String.IsNullOrWhiteSpace(node.text) then
                                    None
                                else Some node.text
                            Name = name
                        }
                        |> match node with
                            | Patterns.Node.JSDocLink _ -> CommentPart.Link
                            | Patterns.Node.JSDocLinkCode _ -> CommentPart.LinkCode
                            | Patterns.Node.JSDocLinkPlain _ -> CommentPart.LinkPlain
                            | _ -> failwith "Unexpected node type"
                | _ -> failwith "Unexpected node kind"
            
        module CommentPart =
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> CommentPart> = Dictionary [
                Ts.SyntaxKind.JSDocText ==> (fun _ -> unbox<Ts.JSDocText> >>  _.text >> String.splitLines >> CommentPart.Text)
                Ts.SyntaxKind.JSDocLink ==> CommentLink.unsafeCreate
                Ts.SyntaxKind.JSDocLinkCode ==> CommentLink.unsafeCreate
                Ts.SyntaxKind.JSDocLinkPlain ==> CommentLink.unsafeCreate
            ]
            let kindSet = kindMap.Keys |> Set
            let isCommentPart (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isCommentPart node then None else
                kindMap[node.kind] program node
                |> Some
            let create (program: Ts.Program) (node: Ts.JSDocComment) =
                if not <| isCommentPart (unbox<Ts.Node> node) then failwith $"Unexpected node type in jsdoccomment {(unbox<Ts.Node> node).kind.Name}"
                kindMap[(unbox<Ts.Node> node).kind] program node
            let fromJSDocTag (program: Ts.Program) tag =
                let tag = JSDoc.toJSDocTag tag
                tag.comment
                |> Option.map (fun comment ->
                    if jsTypeof comment = "string" then
                        unbox<string> comment
                        |> String.splitLines
                        |> CommentPart.Text
                        |> Array.singleton
                    else
                        unbox<Ts.JSDocComment array> comment
                        |> Array.map (create program)
                    )
                |> Option.defaultValue [||]
                |> NonEmptyArray.create
        module JSDoc =
            let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> JSDoc> = Dictionary [
                Ts.SyntaxKind.JSDocParameterTag ==> fun _ -> JSDoc.Parameter
                Ts.SyntaxKind.JSDocThrowsTag ==> fun _ -> JSDoc.Throws
                Ts.SyntaxKind.JSDocReturnTag ==> fun _ -> JSDoc.Return
                Ts.SyntaxKind.JSDocTypeTag ==> fun _ -> JSDoc.Type
                Ts.SyntaxKind.JSDocTemplateTag ==> fun _ -> JSDoc.Template
                Ts.SyntaxKind.JSDocDeprecatedTag ==> fun _ -> JSDoc.Deprecated
                Ts.SyntaxKind.JSDocCallbackTag ==> fun _ -> JSDoc.Callback
                Ts.SyntaxKind.JSDocTypedefTag ==> fun _ -> JSDoc.Typedef
                Ts.SyntaxKind.JSDocAugmentsTag ==> fun _ -> JSDoc.Augments
                Ts.SyntaxKind.JSDocSeeTag ==> fun _ -> JSDoc.See
                Ts.SyntaxKind.JSDocOverrideTag ==> fun _ -> JSDoc.Override
                Ts.SyntaxKind.JSDocClassTag ==> fun _ -> JSDoc.Class
                Ts.SyntaxKind.JSDocPublicTag ==> fun _ -> JSDoc.Public
                Ts.SyntaxKind.JSDocPrivateTag ==> fun _ -> JSDoc.Private
                Ts.SyntaxKind.JSDocReadonlyTag ==> fun _ -> JSDoc.Readonly
                Ts.SyntaxKind.JSDocImportTag ==> fun _ -> JSDoc.Import
                Ts.SyntaxKind.JSDocTag ==> fun _ -> JSDoc.Tag
                Ts.SyntaxKind.JSDocOverloadTag ==> fun _ -> JSDoc.Overload
            ]
            let kindSet = kindMap.Keys |> Set
            let isJSDoc (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program (node: Ts.Node) =
                if not <| isJSDoc node then None else
                kindMap[node.kind] program node
                |> Some
            let create (program: Ts.Program) (node: Ts.JSDocTag) : JSDoc =
                if not <| isJSDoc node then failwithf "Could not create JSDoc from node %A" node.kind.Name
                kindMap[node.kind] program node
            let collectForNode (program: Ts.Program) (node: Ts.Node) =
                ts.getAllJSDocTags(node, fun _ -> true).AsArray
                |> Array.map (create program)
                |> NonEmptyArray.create
            let toJSDocTag: JSDoc -> Ts.JSDocTag = function
                | JSDoc.Parameter node -> node :> Ts.JSDocTag
                | JSDoc.Tag node -> node
                | JSDoc.Deprecated node -> node
                | JSDoc.Type node -> node
                | JSDoc.Private node -> node
                | JSDoc.Return node -> node
                | JSDoc.See node -> node
                | JSDoc.Readonly node -> node
                | JSDoc.Override node -> node
                | JSDoc.Public node -> node
                | JSDoc.Callback node -> node
                | JSDoc.Throws node -> node
                | JSDoc.Typedef node -> node
                | JSDoc.Template node -> node
                | JSDoc.Import node -> node
                | JSDoc.Overload node -> node
                | JSDoc.Augments node -> node
                | JSDoc.Class node -> node
            let getTag = toJSDocTag >> _.tagName.text
            let getComment program = CommentPart.fromJSDocTag program

        module Script =
            let isValid (sourceFile: Ts.SourceFile) = not <| ts.isExternalModule sourceFile
            let unsafeCreate program sourceFile = ParentInlinedProgram.wrap program Script sourceFile
            let create program sourceFile = if isValid sourceFile then unsafeCreate program sourceFile |> ValueSome else ValueNone
            let toSourceFile (script: Node.Script) = unbox<Ts.SourceFile> script
            let toDeclaration script = toSourceFile script :> Ts.Declaration
            let mapSourceFile (fn: Ts.SourceFile -> 'T) = toSourceFile >> fn
            let getSymbol (script: Node.Script) =
                script |> mapSourceFile (
                    script.checker.getSymbolAtLocation
                    >> Option.defaultWith (fun () ->
                        (toSourceFile script).fileName
                        |> Logging.Log.Default.logfe "SourceFile marked as script had no symbol associated: %s{fileName}"
                        failwith "SourceFile marked as script had no symbol associated"
                        )
                    )
            let getSymbolLocals script = toSourceFile script |> LocalSymbolTable.fromSourceFile
            let getStatements = toSourceFile >> _.statements.AsArray >> Array.map TopLevelStatements.Create

        module ExternalModule =
            let isValid (sourceFile: Ts.SourceFile) = ts.isExternalModule sourceFile
            let unsafeCreate program sourceFile = ParentInlinedProgram.wrap program ExternalModule sourceFile
            let create program sourceFile = if isValid sourceFile then unsafeCreate program sourceFile |> ValueSome else ValueNone
            let toSourceFile (externalModule: Node.ExternalModule) = unbox<Ts.SourceFile> externalModule
            let toDeclaration externalModule = toSourceFile externalModule :> Ts.Declaration
            let mapSourceFile (fn: Ts.SourceFile -> 'T) = toSourceFile >> fn
            let getSymbol (externalModule: Node.ExternalModule) =
                externalModule
                |> mapSourceFile (
                    externalModule.checker.getSymbolAtLocation
                    >> Option.defaultWith (fun () ->
                        (toSourceFile externalModule).fileName
                        |> Logging.Log.Default.logfe "SourceFile marked as external module had no symbol associated: %s{fileName}" 
                        failwith "SourceFile marked as external module had no symbol associated"
                        ))
            let getSymbolExports (externalModule: Node.ExternalModule) =
                getSymbol externalModule
                |> _.exports
                |> Option.defaultWith (fun () ->
                    Logging.Log.Default.logfe "SourceFile marked as external module symbol had no exports associated: %s{fileName}" (toSourceFile externalModule).fileName
                    failwith "SourceFile marked as external module symbol had no exports associated"
                    )
                |> ExportSymbolTable.create
            let getSymbolGlobalExports (externalModule: Node.ExternalModule) =
                getSymbol externalModule
                |> _.globalExports
                |> Option.map ExportSymbolTable.create
                |> Option.toValueOption
            let getSymbolLocals (externalModule: Node.ExternalModule) =
                toSourceFile externalModule
                |> LocalSymbolTable.fromSourceFile
            let getModuleSpecifiers (externalModule: Node.ExternalModule) =
                let program = externalModule.program
                let symbol = getSymbol externalModule
                let moduleSpecifier = program.GetModuleSpecifier symbol
                moduleSpecifier.kind |> Option.defaultWith (fun () ->
                    Logging.Log.Default.logfe "SourceFile marked as external module symbol had no module specifier kind associated: %s{fileName}" symbol.name
                    failwith $"SourceFile marked as external module symbol had no module specifier kind associated: {symbol.name}"
                    ),
                moduleSpecifier.moduleSpecifiers
                |> NonEmptyArray.vcreate
                |> ValueOption.defaultWith(fun () ->
                    Logging.Log.Default.logfe "SourceFile marked as external module symbol had no module specifiers associated: %s{fileName}" symbol.name
                    failwith $"SourceFile marked as external module symbol had no module specifiers associated: {symbol.name}"
                    )
            let inline getModuleSpecifiersObj externalModule =
                let kind,specifiers = getModuleSpecifiers externalModule
                {| kind = kind; specifiers = specifiers |}
            let getStatements = toSourceFile >> _.statements.AsArray >> Array.map TopLevelStatements.Create

        module SourceKind =
            let toDeclaration = function
                | Node.SourceKind.Script script -> Script.toDeclaration script
                | Node.SourceKind.ExternalModule externalModule -> ExternalModule.toDeclaration externalModule
            let create (program: Ts.Program) (sourceFile: Ts.SourceFile) =
                if Script.isValid sourceFile then
                    ParentInlinedProgram.wrap program Script sourceFile
                    |> Node.SourceKind.Script
                else
                    ParentInlinedProgram.wrap program ExternalModule sourceFile
                    |> Node.SourceKind.ExternalModule
            let toSourceFile = function
                | Node.SourceKind.Script script -> script |> Script.toSourceFile
                | Node.SourceKind.ExternalModule externalModule -> externalModule |> ExternalModule.toSourceFile
            let isDefaultLib sourceKind =
                toSourceFile sourceKind
                |> sourceKind.program.isSourceFileDefaultLibrary
            let getStatements = function
                | Node.SourceKind.Script script -> Script.getStatements script
                | Node.SourceKind.ExternalModule externalModule -> ExternalModule.getStatements externalModule

        module MethodKind =
            let private methodSignatureSigil = SymbolTypeKey.create<Ts.ObjectType> "methodSignatureType"
            let inline private unwrap (methodKind: Node.MethodKind) =
                match methodKind with
                | Node.MethodKind.Class (Unwrap method) -> method :> Ts.Node
                | Node.MethodKind.Type (Unwrap method) -> method
            let createClass program methodDeclaration =
                ParentInlinedProgram.wrap program Node.MethodKind.Class methodDeclaration
            let createType program methodDeclaration =
                ParentInlinedProgram.wrap program Node.MethodKind.Type methodDeclaration
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.MethodDeclaration ==> createClass
                Ts.SyntaxKind.MethodSignature ==> createType
            ]
            let kindSet = kindMap.Keys |> Set
            let isMethodKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isMethodKind node then None else
                kindMap[node.kind] program node
                |> Some
            let create program node =
                match node with
                | Patterns.Node.MethodDeclaration node -> createClass program node |> Some
                | Patterns.Node.MethodSignature node -> createType program node |> Some
                | _ -> None
            let inline unsafeCreate program node =
                create program node |> Option.defaultWith(fun () -> failwithf $"Unknown MethodDeclaration %A{node.kind.Name}")
            
            let isOptional = function
                | Node.MethodKind.Class method -> method.Value.questionToken.IsSome
                | Node.MethodKind.Type method -> method.Value.questionToken.IsSome
            
            let getMethodType (methodKind: Node.MethodKind) =
                methodKind
                |> SymbolTypeKey.accessOrInit methodSignatureSigil (fun () ->
                    let typ = methodKind.checker.getTypeAtLocation (unwrap methodKind)
                    if isOptional methodKind then
                        typ :?> Ts.UnionType
                        |> _.types.AsArray
                        |> Array.find (_.flags.HasFlag(Ts.TypeFlags.Undefined) >> not)
                    else typ
                    :?> Ts.ObjectType
                    )
            
            let toDeclaration = function
                | MethodKind.Class method -> method.Value :> Ts.Declaration
                | MethodKind.Type method -> method.Value 
                
        module ParameterKind =
            let createSimple program parameterDeclaration =
                ParameterDeclaration parameterDeclaration
                |> ParentInlinedProgram.wrap program Node.ParameterKind.Simple 
            let createBinding program bindingElement =
                BindingElement bindingElement
                |> ParentInlinedProgram.wrap program Node.ParameterKind.Binding
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.Parameter ==> createSimple
                Ts.SyntaxKind.BindingElement ==> createBinding
            ]
            let kindSet = kindMap.Keys |> Set
            let isParameterKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isParameterKind node then None else
                kindMap[node.kind] program node
                |> Some
            let create program node =
                match node with
                | Patterns.Node.Parameter node -> createSimple program node |> Some
                | Patterns.Node.BindingElement node -> createBinding program node |> Some
                | _ -> None
            let unsafeCreate program node =
                create program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown ParameterDeclaration %A{node.kind.Name}")
            let toDeclaration = function
                | ParameterKind.Simple parameter -> parameter.Value.Value :> Ts.Declaration
                | ParameterKind.Binding binding -> binding.Value.Value :> Ts.Declaration
        
        module SignatureKind =
            let createCall program callSigDeclaration =
                ParentInlinedProgram.wrap program Node.SignatureKind.Call callSigDeclaration
            let createConstruct program constructSigDeclaration =
                ParentInlinedProgram.wrap program Node.SignatureKind.Construct constructSigDeclaration
            let createIndex program indexSigDeclaration =
                ParentInlinedProgram.wrap program Node.SignatureKind.Index indexSigDeclaration
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.CallSignature ==> createCall
                Ts.SyntaxKind.ConstructSignature ==> createConstruct
                Ts.SyntaxKind.IndexSignature ==> createIndex
            ]
            let kindSet = kindMap.Keys |> Set
            let isSignatureKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isSignatureKind node then None else
                kindMap[node.kind] program node
                |> Some
            let toDeclaration = function
                | SignatureKind.Call node -> node.Value :> Ts.Declaration
                | SignatureKind.Construct node -> node.Value
                | SignatureKind.Index node -> node.Value
            let inline create program node =
                match node with
                | Patterns.Node.CallSignatureDeclaration node -> createCall program node |> Some
                | Patterns.Node.ConstructSignatureDeclaration node -> createConstruct program node |> Some
                | Patterns.Node.IndexSignatureDeclaration node -> createIndex program node |> Some
                | _ -> None
            let unsafeCreate program node =
                create program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown SignatureDeclaration %A{node.kind.Name}")

        module PropertyKind =
            let toDeclaration = function
                | PropertyKind.Class property -> property.Value :> Ts.Declaration
                | PropertyKind.Type property -> property.Value
            let createClass program propertyDeclaration =
                ParentInlinedProgram.wrap program Node.PropertyKind.Class propertyDeclaration
            let createType program propertyDeclaration =
                ParentInlinedProgram.wrap program Node.PropertyKind.Type propertyDeclaration
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.PropertyDeclaration ==> createClass
                Ts.SyntaxKind.PropertySignature ==> createType
            ]
            let kindSet = kindMap.Keys |> Set
            let isPropertyKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isPropertyKind node then None else
                kindMap[node.kind] program node
                |> Some
            let inline create program node =
                match node with
                | Patterns.Node.PropertyDeclaration node -> createClass program node |> Some
                | Patterns.Node.PropertySignature node -> createType program node |> Some
                | _ -> None
            let unsafeCreate program node =
                create program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown PropertyDeclaration %A{node.kind.Name}")

        module ClassMemberKind =
            let createMethod program methodDeclaration =
                ParentInlinedProgram.wrap program Node.ClassMemberKind.Method methodDeclaration
            let createProperty program propertyDeclaration =
                ParentInlinedProgram.wrap program Node.ClassMemberKind.Property propertyDeclaration
            let createGetAccessor program getAccessorDeclaration =
                ParentInlinedProgram.wrap program Node.ClassMemberKind.GetAccessor getAccessorDeclaration
            let createSetAccessor program setAccessorDeclaration =
                ParentInlinedProgram.wrap program Node.ClassMemberKind.SetAccessor setAccessorDeclaration
            let inline create program node =
                match node with
                | Patterns.Node.MethodDeclaration node -> createMethod program node |> Some
                | Patterns.Node.PropertyDeclaration node -> createProperty program node |> Some
                | Patterns.Node.GetAccessorDeclaration node -> createGetAccessor program node |> Some
                | Patterns.Node.SetAccessorDeclaration node -> createSetAccessor program node |> Some
                | _ -> None
            let unsafeCreate program node =
                create program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown ClassMemberDeclaration %A{node.kind.Name}")
            let toDeclaration = function
                | ClassMemberKind.Method method -> method.Value :> Ts.Declaration
                | ClassMemberKind.Property property -> property.Value :> Ts.Declaration
                | ClassMemberKind.GetAccessor getAccessor -> getAccessor.Value :> Ts.Declaration
                | ClassMemberKind.SetAccessor setAccessor -> setAccessor.Value :> Ts.Declaration
            
        module ModuleDeclaration =
            let create program moduleDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleDeclaration.ModuleDeclaration moduleDeclaration
            let tryCreate program node =
                match node with
                | Patterns.Node.ModuleDeclaration node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node =
                tryCreate program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown ModuleDeclaration %A{node.kind.Name}")
            let toDeclaration (moduleDecl: Node.ModuleDeclaration) = moduleDecl.Value :> Ts.Declaration 

        module ModuleKind =
            let createDeclaration program moduleDeclaration =
                ModuleDeclaration.create program moduleDeclaration
                |> Node.ModuleKind.Declaration
            let createSource program source =
                SourceKind.create program source
                |> Node.ModuleKind.Source
            let createSourceKind = Node.ModuleKind.Source
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.ModuleDeclaration ==> createDeclaration
                Ts.SyntaxKind.SourceFile ==> createSource
            ]
            let kindSet = kindMap.Keys |> Set
            let isModuleKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isModuleKind node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown ModuleDeclaration %A{node.kind.Name}")
            let toDeclaration = function
                | ModuleKind.Declaration moduleDecl -> ModuleDeclaration.toDeclaration moduleDecl
                | ModuleKind.Source source -> SourceKind.toDeclaration source
                
        module ModuleMemberKind =
            let createVariable program variableDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Variable variableDeclaration
            let createFunction program functionDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Function functionDeclaration
            let createClass program classDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Class classDeclaration
            let createEnum program enumDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Enum enumDeclaration
            let createInterface program interfaceDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Interface interfaceDeclaration
            let createModule program moduleDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.Module moduleDeclaration
            let createTypeAlias program typeAliasDeclaration =
                ParentInlinedProgram.wrap program Node.ModuleMemberKind.TypeAlias typeAliasDeclaration
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.VariableDeclaration ==> createVariable
                Ts.SyntaxKind.FunctionDeclaration ==> createFunction
                Ts.SyntaxKind.ClassDeclaration ==> createClass
                Ts.SyntaxKind.EnumDeclaration ==> createEnum
                Ts.SyntaxKind.InterfaceDeclaration ==> createInterface
                Ts.SyntaxKind.ModuleDeclaration ==> createModule
                Ts.SyntaxKind.TypeAliasDeclaration ==> createTypeAlias
            ]
            let kindSet = kindMap.Keys |> Set
            let isModuleMemberKind (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isModuleMemberKind node then None else
                kindMap[node.kind] program node
                |> Some
            let inline create program node =
                match node with
                | Patterns.Node.VariableDeclaration node -> createVariable program node |> Some
                | Patterns.Node.FunctionDeclaration node -> createFunction program node |> Some
                | Patterns.Node.ClassDeclaration node -> createClass program node |> Some
                | Patterns.Node.EnumDeclaration node -> createEnum program node |> Some
                | Patterns.Node.InterfaceDeclaration node -> createInterface program node |> Some
                | Patterns.Node.ModuleDeclaration node -> createModule program node |> Some
                | Patterns.Node.TypeAliasDeclaration node -> createTypeAlias program node |> Some
                | _ -> None
            let unsafeCreate program node =
                create program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown ModuleMemberDeclaration %A{node.kind.Name}")
            let toDeclaration = function
                | ModuleMemberKind.Variable variable -> variable.Value :> Ts.Declaration
                | ModuleMemberKind.Function function_ -> function_.Value :> Ts.Declaration
                | ModuleMemberKind.Class class_ -> class_.Value :> Ts.Declaration
                | ModuleMemberKind.Enum enum_ -> enum_.Value :> Ts.Declaration
                | ModuleMemberKind.Interface interface_ -> interface_.Value :> Ts.Declaration
                | ModuleMemberKind.Module module_ -> module_.Value :> Ts.Declaration
                | ModuleMemberKind.TypeAlias typeAlias -> typeAlias.Value :> Ts.Declaration

        module EnumMember =
            let inline private unwrap (EnumMember enumMember) = enumMember.Value
            let create program enumMemberDeclaration =
                ParentInlinedProgram.wrap program EnumMember enumMemberDeclaration
            let tryCreate program node =
                match node with
                | Patterns.Node.EnumMember node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node =
                tryCreate program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown EnumMemberDeclaration %A{node.kind.Name}")
            let getSymbol enumMember =
                unwrap enumMember
                |> _.name
                |> unbox<Ts.Node>
                |> enumMember.checker.getSymbolAtLocation
                |> Option.defaultWith(fun () -> failwith "Could not find symbol for enum member")
            let isCanonical enumMember =
                getSymbol enumMember
                |> _.valueDeclaration.Value
                |> NodeKey.fromNode
                |> (=) (NodeKey.fromNode <| unwrap enumMember)
            let inline private getValueFromType (typ: Ts.Type) =
                if typ.flags.HasFlag Ts.TypeFlags.NumberLiteral then
                    let typ = typ :?> Ts.NumberLiteralType
                    if JS.Constructors.Number.isSafeInteger typ.value then
                        Choice2Of3 (int typ.value)
                    else
                        Choice3Of3 typ.value
                else
                    let typ = typ :?> Ts.StringLiteralType
                    Choice1Of3 typ.value
            let getType enumMember =
                let enumNode = unwrap enumMember
                enumMember.checker.getTypeAtLocation enumNode
            let getValue = getType >> getValueFromType
            let toDeclaration (EnumMember enumMember) = enumMember.Value :> Ts.Declaration
        module Variable =
            let inline private unwrap (Variable variable) = variable.Value
            let isVariable: Ts.Node -> bool = ts.isVariableDeclaration
            let create program variableDeclaration =
                InlinedProgram.create program variableDeclaration
                |> Node.Variable.Variable
            let tryCreate program (node: Ts.Node) =
                if not <| isVariable node then None else
                node :?> Ts.VariableDeclaration
                |> create program
                |> Some
            let unsafeCreate program node =
                tryCreate program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown VariableDeclaration %A{node.kind.Name}")
            let toVariableDeclaration = unwrap
        module DeclarationKind =
            let fromVariableDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Variable node
            let fromFunctionDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Function node
            let fromParameterKind _ node = Node.DeclarationKind.Parameter node
            let fromInterfaceDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Interface node
            let fromMethodKind _ node = Node.DeclarationKind.Method node
            let fromTypeAliasDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.TypeAlias node
            let fromPropertyKind _ node = Node.DeclarationKind.Property node
            let fromTypeParameterDeclaration program node =
                TypeParameterDeclaration.create program node
                |> Node.DeclarationKind.TypeParameter 
            let fromModuleKind _ node = Node.DeclarationKind.Module node
            let fromGetAccessorDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.GetAccessor node
            let fromSetAccessorDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.SetAccessor node
            let fromClassDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Class node
            let fromImportSpecifier program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.ImportSpecifier node
            let fromExportSpecifier program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.ExportSpecifier node
            let fromNamespaceImport program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.NamespaceImport node
            let fromImportClause program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.ImportClause node
            let fromEnumMember _ node = Node.DeclarationKind.EnumMember node
            let fromExportAssignment program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.ExportAssignment node
            let fromNamespaceExport program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.NamespaceExport node
            let fromConstructor program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Constructor node
            let fromEnum program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.Enum node
            let fromImportEquals program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.ImportEquals node
            let fromNamespaceExportDeclaration program node =
                ParentInlinedProgram.wrap program Node.DeclarationKind.NamespaceExportDeclaration node
            let fromSignatureKind _ node = Node.DeclarationKind.Signature node
            let fromType _ node = Node.DeclarationKind.Type node
            
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.VariableDeclaration ==> fromVariableDeclaration
                Ts.SyntaxKind.TypeAliasDeclaration ==> fromTypeAliasDeclaration
                Ts.SyntaxKind.FunctionDeclaration ==> fromFunctionDeclaration
                Ts.SyntaxKind.InterfaceDeclaration ==> fromInterfaceDeclaration
                yield! fetchMap ParameterKind.kindMap fromParameterKind
                yield! fetchMap PropertyKind.kindMap fromPropertyKind
                yield! fetchMap SignatureKind.kindMap fromSignatureKind
                yield! fetchMap MethodKind.kindMap fromMethodKind
                Ts.SyntaxKind.TypeParameter ==> fromTypeParameterDeclaration
                yield! fetchMap ModuleKind.kindMap fromModuleKind
                Ts.SyntaxKind.GetAccessor ==> fromGetAccessorDeclaration
                Ts.SyntaxKind.SetAccessor ==> fromSetAccessorDeclaration
                Ts.SyntaxKind.ClassDeclaration ==> fromClassDeclaration
                Ts.SyntaxKind.ImportSpecifier ==> fromImportSpecifier
                Ts.SyntaxKind.ImportClause ==> fromImportClause
                Ts.SyntaxKind.ExportSpecifier ==> fromExportSpecifier
                Ts.SyntaxKind.NamespaceImport ==> fromNamespaceImport
                Ts.SyntaxKind.NamespaceExport ==> fromNamespaceExport
                Ts.SyntaxKind.ExportAssignment ==> fromExportAssignment
                Ts.SyntaxKind.Constructor ==> fromConstructor
                Ts.SyntaxKind.EnumDeclaration ==> fromEnum
                Ts.SyntaxKind.EnumMember ==> fromEnumMember
                Ts.SyntaxKind.ImportEqualsDeclaration ==> fromImportEquals
                Ts.SyntaxKind.NamespaceExportDeclaration ==> fromNamespaceExportDeclaration
                yield! fetchMap Type.kindMap fromType
            ]
            let internal kindSet = kindMap.Keys |> Set
            let isDeclarationKind (node: Ts.Node) = kindSet.Contains node.kind 
            
            let tryCreate program node =
                if not <| isDeclarationKind node then None else
                kindMap[node.kind] program node
                |> Some
            let unsafeCreate program node =
                tryCreate program node
                |> Option.defaultWith(fun () -> failwithf $"Unknown Declaration %A{node.kind.Name}")
            let create program (declaration: Ts.Declaration) =
                unsafeCreate program declaration
            let toNode = function
                | DeclarationKind.Variable variable -> variable.Value.Value :> Ts.Node
                | DeclarationKind.Function function_ -> function_.Value :> Ts.Node
                | DeclarationKind.Parameter parameter -> ParameterKind.toDeclaration parameter
                | DeclarationKind.Interface interface_ -> interface_.Value :> Ts.Node
                | DeclarationKind.Method method -> MethodKind.toDeclaration method
                | DeclarationKind.TypeAlias typeAlias -> typeAlias.Value :> Ts.Node
                | DeclarationKind.Property property -> PropertyKind.toDeclaration property
                | DeclarationKind.TypeParameter typeParameter -> typeParameter.Value :> Ts.Node
                | DeclarationKind.Module module_ -> ModuleKind.toDeclaration module_ 
                | DeclarationKind.GetAccessor getAccessor -> getAccessor.Value :> Ts.Node
                | DeclarationKind.SetAccessor setAccessor -> setAccessor.Value :> Ts.Node
                | DeclarationKind.Class class_ -> class_.Value :> Ts.Node
                | DeclarationKind.ImportSpecifier importSpecifier -> importSpecifier.Value :> Ts.Node
                | DeclarationKind.ExportSpecifier exportSpecifier -> exportSpecifier.Value :> Ts.Node
                | DeclarationKind.NamespaceImport namespaceImport -> namespaceImport.Value :> Ts.Node
                | DeclarationKind.ExportAssignment exportAssignment -> exportAssignment.Value :> Ts.Node
                | DeclarationKind.NamespaceExport namespaceExport -> namespaceExport.Value :> Ts.Node
                | DeclarationKind.Enum enum_ -> enum_.Value :> Ts.Node
                | DeclarationKind.ImportClause importClause -> importClause.Value :> Ts.Node
                | DeclarationKind.EnumMember enumMember -> EnumMember.toDeclaration enumMember
                | DeclarationKind.ImportEquals importEquals -> importEquals.Value :> Ts.Node
                | DeclarationKind.NamespaceExportDeclaration namespaceExportDeclaration -> namespaceExportDeclaration.Value :> Ts.Node
                | DeclarationKind.Type typeKind -> Type.getTypeNode typeKind :> Ts.Node
                | DeclarationKind.Signature signatureKind -> SignatureKind.toDeclaration signatureKind
                | DeclarationKind.Constructor parentInlinedProgram -> parentInlinedProgram.Value
        module ImportExportControl =
            let createExportDeclaration program node = ParentInlinedProgram.wrap program Node.ImportExportControl.ExportDeclaration node
            let createNamedExports program node = ParentInlinedProgram.wrap program Node.ImportExportControl.NamedExports node
            let createNamedImports program node = ParentInlinedProgram.wrap program Node.ImportExportControl.NamedImports node
            let createImportDeclaration program node = ParentInlinedProgram.wrap program Node.ImportExportControl.ImportDeclaration node
            let createExternalModuleReference program node = ParentInlinedProgram.wrap program Node.ImportExportControl.ExternalModuleReference node
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.ExportDeclaration ==> createExportDeclaration
                Ts.SyntaxKind.NamedExports ==> createNamedExports
                Ts.SyntaxKind.NamedImports ==> createNamedImports
                Ts.SyntaxKind.ImportDeclaration ==> createImportDeclaration
                Ts.SyntaxKind.ExternalModuleReference ==> createExternalModuleReference
            ]
            let kindSet = kindMap.Keys |> Set
            let isImportExportControl (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate program node =
                if not <| isImportExportControl node then None else
                kindMap[node.kind] program node
                |> Some
            let toNode = function
                | ImportExportControl.ExportDeclaration exportDeclaration -> exportDeclaration.Value :> Ts.Node
                | ImportExportControl.NamedExports namedExports -> namedExports.Value :> Ts.Node
                | ImportExportControl.ImportDeclaration parentInlinedProgram -> parentInlinedProgram.Value
                | ImportExportControl.NamedImports namedImports -> namedImports.Value :> Ts.Node
                | ImportExportControl.ExternalModuleReference externalModuleReference -> externalModuleReference.Value :> Ts.Node

        module TypeKeyword =
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.StringKeyword ==>! Node.TypeKeyword.String 
                Ts.SyntaxKind.NumberKeyword ==>! Node.TypeKeyword.Number 
                Ts.SyntaxKind.BooleanKeyword ==>! Node.TypeKeyword.Boolean 
                Ts.SyntaxKind.AnyKeyword ==>! Node.TypeKeyword.Any 
                Ts.SyntaxKind.VoidKeyword ==>! Node.TypeKeyword.Void 
                Ts.SyntaxKind.UndefinedKeyword ==>! Node.TypeKeyword.Undefined 
                Ts.SyntaxKind.NullKeyword ==>! Node.TypeKeyword.Null 
                Ts.SyntaxKind.NeverKeyword ==>! Node.TypeKeyword.Never 
                Ts.SyntaxKind.UnknownKeyword ==>! Node.TypeKeyword.Unknown 
                Ts.SyntaxKind.ObjectKeyword ==>! Node.TypeKeyword.Object 
                Ts.SyntaxKind.SymbolKeyword ==>! Node.TypeKeyword.Symbol 
                Ts.SyntaxKind.IntrinsicKeyword ==>! Node.TypeKeyword.Intrinsic 
                Ts.SyntaxKind.BigIntKeyword ==>! Node.TypeKeyword.BigInt 
            ]
            let kindSet = kindMap.Keys |> Set
            let isTypeKeyword (node: Ts.Node) = kindSet.Contains node.kind
            let tryCreate (node: Ts.Node) = kindMap[node.kind] JS.undefined node |> Some
            let unsafeCreate node =
                tryCreate node
                |> Option.defaultWith(fun () -> failwithf $"Unknown TypeKeyword %A{node.kind.Name}")
        
        module UnionType =
            let create program (node: Ts.UnionTypeNode) =
                InlinedProgram.create program node
                |> UnionType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.UnionTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown UnionTypeNode %A{node.kind.Name}")
        
        module IntersectionType =
            let create program (node: Ts.IntersectionTypeNode) =
                InlinedProgram.create program node
                |> IntersectionType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.IntersectionTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown IntersectionTypeNode %A{node.kind.Name}")
        
        module ArrayType =
            let create program (node: Ts.ArrayTypeNode) =
                InlinedProgram.create program node
                |> ArrayType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.ArrayTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ArrayTypeNode %A{node.kind.Name}")
        
        module TupleType =
            let create program (node: Ts.TupleTypeNode) =
                InlinedProgram.create program node
                |> TupleType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.TupleTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TupleTypeNode %A{node.kind.Name}")
            
        module NamedTupleMember =
            let create program (node: Ts.NamedTupleMember) =
                InlinedProgram.create program node
                |> NamedTupleMember
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.NamedTupleMember node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown NamedTupleMember %A{node.kind.Name}")
        
        module RestType =
            let create program (node: Ts.RestTypeNode) =
                InlinedProgram.create program node
                |> RestType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.RestTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown RestTypeNode %A{node.kind.Name}")
            
        module OptionalType =
            let create program (node: Ts.OptionalTypeNode) =
                InlinedProgram.create program node
                |> OptionalType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.OptionalTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown OptionalTypeNode %A{node.kind.Name}")
        module ParenthesizedType =
            let create program (node: Ts.ParenthesizedTypeNode) =
                InlinedProgram.create program node
                |> ParenthesizedType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.ParenthesizedTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ParenthesizedTypeNode %A{node.kind.Name}")
        
        module TypeParameterDeclaration =
            let create program (node: Ts.TypeParameterDeclaration) =
                InlinedProgram.create program node
                |> TypeParameterDeclaration
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TypeParameterDeclaration node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TypeParameterDeclaration %A{node.kind.Name}")
        module InferType =
            let create program (node: Ts.InferTypeNode) =
                InlinedProgram.create program node
                |> InferType
            let tryCreate program (node: Ts.TypeNode) =
                match node with
                | Patterns.Node.InferTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown InferTypeNode %A{node.kind.Name}")
        module TypePredicate =
            let create program (node: Ts.TypePredicateNode) =
                InlinedProgram.create program node
                |> TypePredicate
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TypePredicateNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TypePredicateNode %A{node.kind.Name}")
        module TypeQuery =
            let create program (node: Ts.TypeQueryNode) =
                InlinedProgram.create program node
                |> TypeQuery
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TypeQueryNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TypeQueryNode %A{node.kind.Name}")
        module IndexedAccessType =
            let create program (node: Ts.IndexedAccessTypeNode) =
                InlinedProgram.create program node
                |> IndexedAccessType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.IndexedAccessTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown IndexedAccessTypeNode %A{node.kind.Name}")
        module MappedType =
            let create program (node: Ts.MappedTypeNode) =
                InlinedProgram.create program node
                |> MappedType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.MappedTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown MappedTypeNode %A{node.kind.Name}")
        
        module ConditionalType =
            let create program (node: Ts.ConditionalTypeNode) =
                InlinedProgram.create program node
                |> ConditionalType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.ConditionalTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ConditionalTypeNode %A{node.kind.Name}")
        module TemplateLiteralType =
            let create program (node: Ts.TemplateLiteralTypeNode) =
                InlinedProgram.create program node
                |> TemplateLiteralType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TemplateLiteralTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TemplateLiteralTypeNode %A{node.kind.Name}")
        module TemplateLiteralTypeSpan =
            let create program (node: Ts.TemplateLiteralTypeSpan) =
                InlinedProgram.create program node
                |> TemplateLiteralTypeSpan
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TemplateLiteralTypeSpan node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TemplateLiteralTypeSpan %A{node.kind.Name}")
        module ImportType =
            let create program (node: Ts.ImportTypeNode) =
                InlinedProgram.create program node
                |> ImportType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.ImportTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ImportTypeNode %A{node.kind.Name}")
        module FunctionType =
            let create program (node: Ts.FunctionTypeNode) =
                ParentInlinedProgram.wrap program FunctionType node
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.FunctionTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown FunctionTypeNode %A{node.kind.Name}")
        module ConstructorType =
            let create program (node: Ts.ConstructorTypeNode) =
                ParentInlinedProgram.wrap program ConstructorType node
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.ConstructorTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ConstructorTypeNode %A{node.kind.Name}")
        module TypeLiteralType =
            let create program (node: Ts.TypeLiteralNode) =
                InlinedProgram.create program node
                |> TypeLiteralType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.TypeLiteralNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown TypeLiteralNode %A{node.kind.Name}")
        module LiteralType =
            let create program (node: Ts.LiteralTypeNode) =
                InlinedProgram.create program node
                |> LiteralType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.LiteralTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown LiteralTypeNode %A{node.kind.Name}")
        module ThisType =
            let create program (node: Ts.ThisTypeNode) =
                InlinedProgram.create program node
                |> ThisType
            let tryCreate program (node: Ts.Node) =
                match node with
                | Patterns.Node.ThisTypeNode node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ThisTypeNode %A{node.kind.Name}")
            
        module KeyOf =
            let inline map (mapFn: Type -> 'T) (keyof: Node.KeyOf) =
                match keyof with
                | KeyOf.Bounded inlinedProgram 
                | KeyOf.Unbounded inlinedProgram 
                | KeyOf.Generic inlinedProgram 
                | KeyOf.ConstrainedGeneric inlinedProgram -> mapFn inlinedProgram

            /// Retrieves the type node that this keyof operator is applied to.
            let getTypeNode node: Ts.TypeNode = map Type.getTypeNode node
            /// Retrieves the type the keyof operator is applied to.
            let getType keyOf = map Type.getType keyOf
            let create program (typeNode: Ts.TypeNode) =
                // ??
                Type.create program typeNode
                |> KeyOf.Generic

        module TypeOperator =
            let inline map (mapFn: Type -> 'T) (typeOperator: Node.TypeOperator) =
                match typeOperator with
                | TypeOperator.KeyOf inlinedProgram -> KeyOf.map mapFn inlinedProgram
                | TypeOperator.Readonly inlinedProgram -> mapFn inlinedProgram
                | TypeOperator.Unique inlinedProgram -> mapFn inlinedProgram
            let isUniqueESSymbol =
                (function TypeOperator.Unique (Type.Keyword (TypeKeyword.Symbol, _)) -> true | _ -> false)
            let getTypeNode typeOperator = map Type.getTypeNode typeOperator
            let getType typeOperator = map Type.getType typeOperator
            let create program (typeOperatorNode: Ts.TypeOperatorNode) =
                match typeOperatorNode.operator with
                | Ts.SyntaxKind.KeyOfKeyword -> KeyOf.create program typeOperatorNode.``type`` |> Node.TypeOperator.KeyOf
                | Ts.SyntaxKind.ReadonlyKeyword -> Type.create program typeOperatorNode.``type`` |> Node.TypeOperator.Readonly
                | Ts.SyntaxKind.UniqueKeyword -> Type.create program typeOperatorNode.``type`` |> Node.TypeOperator.Unique
                | _ -> failwithf "Unknown TypeOperatorNode operator %A" typeOperatorNode.operator.Name
            let tryCreate program (typeOperatorNode: Ts.Node) =
                match typeOperatorNode with
                | Patterns.Node.TypeOperatorNode node -> create program node |> Some
                | _ -> None

        module TypeReference =
            let create program (typeReferenceNode: Ts.TypeReferenceNode) = ParentInlinedProgram.wrap program TypeReference typeReferenceNode
            let tryCreate program (typeReferenceNode: Ts.Node) =
                match typeReferenceNode with
                | Patterns.Node.TypeReferenceNode node -> create program node |> Some
                | _ -> None
            let inline getTypeNode (typeReference: TypeReference): Ts.TypeReferenceNode = emitJsExpr typeReference "$0.fields[0]"
            let targetSymbol (typeReference: TypeReference) =
                typeReference.checker.getSymbolAtLocation (getTypeNode typeReference |> _.typeName |> unbox<Ts.Node>)
                |> Option.defaultWith(fun () -> failwith "Could not find symbol for type reference node")
        
        module ExpressionWithTypeArguments =
            let create program (expressionWithTypeArgumentsNode: Ts.ExpressionWithTypeArguments) = ParentInlinedProgram.wrap program ExpressionWithTypeArguments expressionWithTypeArgumentsNode
            let tryCreate program (expressionWithTypeArgumentsNode: Ts.Node) =
                match expressionWithTypeArgumentsNode with
                | Patterns.Node.ExpressionWithTypeArguments node -> create program node |> Some
                | _ -> None
            let unsafeCreate program node = tryCreate program node |> Option.defaultWith(fun () -> failwithf $"Unknown ExpressionWithTypeArgumentsNode %A{node.kind.Name}")
            let getType (expressionWithTypeArguments: ExpressionWithTypeArguments) =
                expressionWithTypeArguments.checker.getTypeFromTypeNode(expressionWithTypeArguments.Value)
        
        module Type =
            /// Optimisation
            let inline getTypeNode (type_: Type) = emitJsExpr type_ "$0.fields[1]"
            let inline getType (type_: Type) =
                getTypeNode type_
                |> type_.checker.getTypeFromTypeNode
            let inline program (node: Type) = node.program
            let inline checker (node: Type) = node.checker
            let createTypeReference program node =
                let value = TypeReference.create program node
                ParentInlinedProgram.wrap program (fun typeNode -> Type.Reference(value, typeNode)) node
            let createTypeKeyword program node =
                let value = TypeKeyword.unsafeCreate node
                ParentInlinedProgram.wrap program (fun typeNode -> Type.Keyword(value, typeNode)) node
            let createLiteralType program node =
                let value = LiteralType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Literal(value, node)) node
            let createUnionType program node =
                let value = UnionType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Union(value, node)) node
            let createFunctionType program node =
                let value = FunctionType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Function(value, node)) node
            let createTypeLiteral program node =
                let value = TypeLiteralType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TypeLiteral(value, node)) node
            let createExpressionWithTypeArguments program node =
                let value = ExpressionWithTypeArguments.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.ExpressionWithTypeArguments(value, node)) node
            let createArrayType program node =
                let value = ArrayType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Array(value, node)) node
            let createTypeOperator program node =
                let value = TypeOperator.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TypeOperator(value, node)) node
            let createIndexedAccess program node =
                let value = IndexedAccessType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.IndexedAccess(value, node)) node
            let createParenthesizedType program node =
                let value = ParenthesizedType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Parenthesized(value, node)) node
            let createThisType program node =
                let value = ThisType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.This(value, node)) node
            let createTupleType program node =
                let value = TupleType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Tuple(value, node)) node
            let createConditionalType program node =
                let value = ConditionalType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Conditional(value, node)) node
            let createIntersectionType program node =
                let value = IntersectionType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Intersection(value, node)) node
            let createTypeQuery program node =
                let value = TypeQuery.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TypeQuery(value, node)) node
            let createInferType program node =
                let value = InferType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Infer(value, node)) node
            let createTypePredicate program node =
                let value = TypePredicate.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TypePredicate(value, node)) node
            let createTemplateLiteralSpan program node =
                let value = TemplateLiteralTypeSpan.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TemplateLiteralSpan(value, node)) node
            let createMapped program node =
                let value = MappedType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Mapped(value, node)) node
            let createNamedTuple program node =
                let value = NamedTupleMember.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.NamedTuple(value, node)) node
            let createTemplateLiteral program node =
                let value = TemplateLiteralType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.TemplateLiteral(value, node)) node
            let createRestType program node =
                let value = RestType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Rest(value, node)) node
            let createImportType program node =
                let value = ImportType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Import(value, node)) node
            let createConstructorType program node =
                let value = ConstructorType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Constructor(value, node)) node
            let createOptionalType program node =
                let value = OptionalType.create program node
                ParentInlinedProgram.wrap program (fun node -> Node.Type.Optional(value, node)) node
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.TypeReference ==> createTypeReference // ~39% of cases
                for key in TypeKeyword.kindSet do // ~29% of cases
                    key ==> createTypeKeyword
                Ts.SyntaxKind.LiteralType ==> createLiteralType // ~10% of type nodes
                Ts.SyntaxKind.UnionType ==> createUnionType // ~7% of type nodes
                Ts.SyntaxKind.FunctionType ==> createFunctionType // ~3% of type nodes
                Ts.SyntaxKind.TypeLiteral ==> createTypeLiteral // ~2% of type nodes
                Ts.SyntaxKind.ExpressionWithTypeArguments ==> createExpressionWithTypeArguments // ~2% of type nodes
                Ts.SyntaxKind.ArrayType ==> createArrayType // ~1.5% of type nodes
                Ts.SyntaxKind.TypeOperator ==> createTypeOperator // ~1% of type nodes
                Ts.SyntaxKind.IndexedAccessType ==> createIndexedAccess // ~1% of type nodes
                Ts.SyntaxKind.ParenthesizedType ==> createParenthesizedType // ~1% of type nodes
                Ts.SyntaxKind.ThisType ==> createThisType // ~1% of type nodes
                Ts.SyntaxKind.TupleType ==> createTupleType // ~0.5% of type nodes
                Ts.SyntaxKind.ConditionalType ==> createConditionalType // ~0.25% of type nodes
                Ts.SyntaxKind.IntersectionType ==> createIntersectionType // ~0.25% of type nodes
                Ts.SyntaxKind.TypeQuery ==> createTypeQuery // ~0.25% of type nodes
                Ts.SyntaxKind.InferType ==> createInferType // ~0.1% of type nodes
                Ts.SyntaxKind.TypePredicate ==> createTypePredicate // ~0.1% of type nodes
                Ts.SyntaxKind.TemplateLiteralTypeSpan ==> createTemplateLiteralSpan // ~0.1% of type nodes
                Ts.SyntaxKind.MappedType ==> createMapped // ~0.1% of type nodes
                Ts.SyntaxKind.NamedTupleMember ==> createNamedTuple // ~0.05% of type nodes
                Ts.SyntaxKind.TemplateLiteralType ==> createTemplateLiteral // ~0.05% of type nodes
                Ts.SyntaxKind.RestType ==> createRestType // ~0.05% of type nodes
                Ts.SyntaxKind.ImportType ==> createImportType // ~0.03% of type nodes
                Ts.SyntaxKind.ConstructorType ==> createConstructorType // ~0.01% of type nodes
                Ts.SyntaxKind.OptionalType ==> createOptionalType // ~0.01% of type nodes
            ]
                
            let create program (typeNode: Ts.TypeNode) =
                kindMap[typeNode.kind] program typeNode
            let tryCreate program (typeNode: Ts.Node): Type option =
                try create program (typeNode :?> Ts.TypeNode) |> Some with _ -> None
        module ModifierKeyword =
            let internal kindMap = Dictionary [
                Ts.SyntaxKind.ExportKeyword ==>! Node.ModifierKeyword.Export
                Ts.SyntaxKind.DeclareKeyword ==>! Node.ModifierKeyword.Declare
                Ts.SyntaxKind.DefaultKeyword ==>! Node.ModifierKeyword.Default
                Ts.SyntaxKind.AbstractKeyword ==>! Node.ModifierKeyword.Abstract
                Ts.SyntaxKind.StaticKeyword ==>! Node.ModifierKeyword.Static
                Ts.SyntaxKind.PublicKeyword ==>! Node.ModifierKeyword.Public
                Ts.SyntaxKind.ProtectedKeyword ==>! Node.ModifierKeyword.Protected
                Ts.SyntaxKind.PrivateKeyword ==>! Node.ModifierKeyword.Private
                Ts.SyntaxKind.OverrideKeyword ==>! Node.ModifierKeyword.Override
                Ts.SyntaxKind.ReadonlyKeyword ==>! Node.ModifierKeyword.ReadOnly
                Ts.SyntaxKind.ConstKeyword ==>! Node.ModifierKeyword.Const
                Ts.SyntaxKind.InKeyword ==>! Node.ModifierKeyword.In
                Ts.SyntaxKind.OutKeyword ==>! Node.ModifierKeyword.Out
            ]
            let kindSet = kindMap.Keys |> Set.ofSeq
            let isModifierKeyword (node: Ts.Node) = kindMap.ContainsKey node.kind
            let create (modifier: Ts.Modifier) =
                match unbox<Ts.Node> modifier with
                | Patterns.SyntaxKind.ExportKeyword _ -> ModifierKeyword.Export
                | Patterns.SyntaxKind.DeclareKeyword _ -> ModifierKeyword.Declare
                | Patterns.SyntaxKind.DefaultKeyword _ -> ModifierKeyword.Default
                | Patterns.SyntaxKind.AbstractKeyword _ -> ModifierKeyword.Abstract
                | Patterns.SyntaxKind.StaticKeyword _ -> ModifierKeyword.Static
                | Patterns.SyntaxKind.PublicKeyword _ -> ModifierKeyword.Public
                | Patterns.SyntaxKind.ProtectedKeyword _ -> ModifierKeyword.Protected
                | Patterns.SyntaxKind.PrivateKeyword _ -> ModifierKeyword.Private
                | Patterns.SyntaxKind.OverrideKeyword _ -> ModifierKeyword.Override
                | Patterns.SyntaxKind.ReadOnlyKeyword _ -> ModifierKeyword.ReadOnly
                | Patterns.SyntaxKind.ConstKeyword _ -> ModifierKeyword.Const
                | Patterns.SyntaxKind.InKeyword _ -> ModifierKeyword.In
                | Patterns.SyntaxKind.OutKeyword _ -> ModifierKeyword.Out
                | _ -> failwithf "Unknown ModifierKeyword %A" modifier.kind.Name
            let tryCreate (program: Ts.Program) (modifier: Ts.Node) =
                if not <| isModifierKeyword modifier then None else
                Some <| kindMap[modifier.kind] program modifier
            
    [<RequireQualifiedAccess>]
    module Symbol =
        /// <summary>
        /// Maps the name of the type, to the transient and nontransient symbol kind constructors.
        /// </summary>
        let private kindConstructor: Dictionary<string, (obj -> Symbol.Kind) * (obj -> Transient.Kind)> = Dictionary [
            let inline (==>) a b = KeyValuePair(a, b)
            let inline (</>) (a: 'T -> Symbol.Kind) (b: 'U -> Transient.Kind): (obj -> Symbol.Kind) * (obj -> Transient.Kind) = (unbox a, unbox b)
            nameof Interface ==> (Symbol.Kind.Interface </> Transient.Kind.Interface)
            nameof Variable ==> (Symbol.Kind.Variable </> Transient.Kind.Variable)
            nameof Function ==> (Symbol.Kind.Function </> Transient.Kind.Function)
            nameof Parameter ==> (Symbol.Kind.Parameter </> Transient.Kind.Parameter)
            nameof Method ==> (Symbol.Kind.Method </> Transient.Kind.Method)
            nameof TypeAlias ==> (Symbol.Kind.TypeAlias </> Transient.Kind.TypeAlias)
            nameof TypeParameter ==> (Symbol.Kind.TypeParameter </> Transient.Kind.TypeParameter)
            nameof ValueModule ==> (Symbol.Kind.ValueModule </> Transient.Kind.ValueModule)
            nameof NamespaceModule ==> (Symbol.Kind.NamespaceModule </> Transient.Kind.NamespaceModule)
            nameof GetAccessor ==> (Symbol.Kind.GetAccessor </> Transient.Kind.GetAccessor)
            nameof SetAccessor ==> (Symbol.Kind.SetAccessor </> Transient.Kind.SetAccessor)
            nameof Class ==> (Symbol.Kind.Class </> Transient.Kind.Class)
            nameof Property ==> (Symbol.Kind.Property </> Transient.Kind.Property)
            nameof TypeEnum ==> (Symbol.Kind.TypeEnum </> Transient.Kind.TypeEnum)
            nameof ConstEnum ==> (Symbol.Kind.ConstEnum </> Transient.Kind.ConstEnum)
            nameof EnumMember ==> (Symbol.Kind.EnumMember </> Transient.Kind.EnumMember)
            nameof TypeLiteral ==> (Symbol.Kind.TypeLiteral </> Transient.Kind.TypeLiteral)
        ]
        /// <summary>
        /// Uses the type variable to look up the constructor for the kind.
        /// If the symbol is a transient symbol, it will apply the transient constructor.
        /// The transient kind is wrapped in the kind transient case.
        /// </summary>
        /// <param name="symbol"></param>
        let inline private constructKind<'T> (symbol: ISymbol) =
            if ISymbol.hasFlag Ts.SymbolFlags.Transient symbol then
                (kindConstructor[typeof<'T>.Name] |> snd)
                >> Kind.Transient
                >> Some
            else
                (kindConstructor[typeof<'T>.Name] |> fst)
                >> Some
            |> funApply symbol
            
        let inline private returnNone _ = None
        /// <summary>
        /// We determine the type of our symbol based on the canonical declaration.
        /// Otherwise, it is arbitrary.
        /// Either way, we can have full access to any API we create for compatible symbol types
        /// through the API defined on the specific symbol type interfaces.
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="declaration">The first 'canonical' declaration for a symbol</param>
        let private makeKind (symbol: ISymbol) (declaration: Ts.Declaration) =
            symbol
            |> match Node.DeclarationKind.create (ISymbol.program symbol) declaration with
                | DeclarationKind.Variable _ -> constructKind<Variable> 
                | DeclarationKind.TypeAlias _ -> constructKind<TypeAlias>
                | DeclarationKind.Function _ -> constructKind<Function>
                | DeclarationKind.Parameter _ -> constructKind<Parameter>
                | DeclarationKind.Interface _ -> constructKind<Interface> 
                | DeclarationKind.Property _ -> constructKind<Property>
                | DeclarationKind.Method _ -> constructKind<Method>
                | DeclarationKind.Signature _ -> constructKind<Signature>
                | DeclarationKind.TypeParameter _ -> constructKind<TypeParameter>
                | DeclarationKind.Module _ ->
                    if symbol |> ISymbol.hasFlag Ts.SymbolFlags.ValueModule
                    then constructKind<ValueModule>
                    else constructKind<NamespaceModule>
                | DeclarationKind.GetAccessor _ -> constructKind<GetAccessor>
                | DeclarationKind.SetAccessor _ -> constructKind<SetAccessor>
                | DeclarationKind.Class _ -> constructKind<Class>
                | DeclarationKind.EnumMember _ -> constructKind<EnumMember>
                | DeclarationKind.Enum _ ->
                    if symbol |> ISymbol.hasFlag Ts.SymbolFlags.ConstEnum
                    then constructKind<ConstEnum>
                    else constructKind<TypeEnum>
                // Not implemented, looking for fails to investigate shape
                | DeclarationKind.Type _ -> constructKind<TypeLiteral>
                // Not implemented, looking for fails to investigate shape
                | DeclarationKind.Constructor _ -> constructKind<Constructor> 
                | DeclarationKind.ImportSpecifier _
                | DeclarationKind.ExportSpecifier _ 
                | DeclarationKind.NamespaceImport _ 
                | DeclarationKind.ExportAssignment _ 
                | DeclarationKind.ImportClause _ 
                | DeclarationKind.ImportEquals _ 
                | DeclarationKind.NamespaceExportDeclaration _
                | DeclarationKind.NamespaceExport _ -> returnNone
        /// <summary>
        /// If we are unable to find the canonical declaration on the symbol, we will attempt to
        /// find the root symbol and use that to find the first declaration.
        /// On failing that, we will make a general assumption via the symbol flags.
        /// If we can still not disambiguate further, we return None.
        /// </summary>
        let private createFallback (program: Ts.Program) (symbol: ISymbol) =
            symbol
            |> program.getTypeChecker().getRootSymbols
            |> _.AsArray
            |> Array.tryPick (_.getDeclarations() >> Option.bind (_.AsArray >> Array.tryHead))
            |> Option.bind (makeKind symbol)
            |> Option.defaultValue (Transient.Kind.Unknown symbol |> Kind.Transient)
            
        /// <summary>
        /// Creates a Symbol.Kind DU. Do not be fooled by the DU name, it only serves as a generalisation
        /// of what the symbol contains. Merging declarations means you can have an enum declaration and a
        /// value module declaration on the symbol. But you will be able to assume the canonical referenced
        /// node would be the enum declaration based on the symbol kind being Symbol.Kind.Enum.
        /// </summary>
        let createKind (program: Ts.Program) (symbol: Ts.Symbol) =
            let iSymbol = ISymbol.create program symbol 
            symbol
            |> _.getDeclarations()
            |> Option.bind (_.AsArray >> Array.tryHead)
            |> Option.bind (makeKind iSymbol)
            |> Option.defaultWith (fun () -> createFallback program iSymbol)
        
        /// <summary>
        /// Use this to simplify your pattern matching.
        /// The motivation for this is the material difference between transient
        /// and non transient symbols only being the guarantee of the value declaration node.
        /// If this difference is not a deterrent for you, then you can half the number of pattern matches
        /// using this pattern. You can disambiguate true transients still via the _.isTransient property
        /// </summary>
        let foldToTransientKind = function
            | Kind.Class kind -> kind :> Transient.Class |> Transient.Kind.Class
            | Kind.Parameter kind -> kind :> Transient.Parameter |> Transient.Kind.Parameter 
            | Kind.Variable kind -> kind :> Transient.Variable |> Transient.Kind.Variable 
            | Kind.Property kind -> kind :> Transient.Property |> Transient.Kind.Property 
            | Kind.EnumMember kind -> kind :> Transient.EnumMember |> Transient.Kind.EnumMember 
            | Kind.Function kind -> kind :> Transient.Function |> Transient.Kind.Function 
            | Kind.Interface kind -> kind :> Transient.Interface |> Transient.Kind.Interface 
            | Kind.ConstEnum kind -> kind :> Transient.ConstEnum |> Transient.Kind.ConstEnum 
            | Kind.TypeEnum kind -> kind :> Transient.TypeEnum |> Transient.Kind.TypeEnum 
            | Kind.ValueModule kind -> kind :> Transient.ValueModule |> Transient.Kind.ValueModule 
            | Kind.NamespaceModule kind -> kind :> Transient.NamespaceModule |> Transient.Kind.NamespaceModule 
            | Kind.TypeLiteral kind -> kind :> Transient.TypeLiteral |> Transient.Kind.TypeLiteral 
            | Kind.ObjectLiteral kind -> kind :> Transient.ObjectLiteral |> Transient.Kind.ObjectLiteral 
            | Kind.Method kind -> kind :> Transient.Method |> Transient.Kind.Method 
            | Kind.Constructor kind -> kind :> Transient.Constructor |> Transient.Kind.Constructor 
            | Kind.GetAccessor kind -> kind :> Transient.GetAccessor |> Transient.Kind.GetAccessor 
            | Kind.SetAccessor kind -> kind :> Transient.SetAccessor |> Transient.Kind.SetAccessor 
            | Kind.Signature kind -> kind :> Transient.Signature |> Transient.Kind.Signature 
            | Kind.TypeParameter kind -> kind :> Transient.TypeParameter |> Transient.Kind.TypeParameter 
            | Kind.TypeAlias kind -> kind :> Transient.TypeAlias |> Transient.Kind.TypeAlias 
            | Kind.Transient kind -> kind

        let inline failIfNone<'T> (symbol: ISymbol) (value: 'T option) =
            value
            |> Option.defaultWith(fun () ->
                let genArgs =
                    typeof<'T>.GenericTypeArguments
                    |> NonEmptyArray.create
                let name =
                    genArgs
                    |> Option.map _.Value.Name
                    |> Option.defaultValue typeof<'T>.Name
                let declarations =
                    ISymbol.declarations symbol
                    |> Option.map (_.Values >> Array.map _.kind.Name)
                    |> Option.defaultValue [||]
                Logging.Log.Default.logfe "Symbol.Kind: Failed to retrieve a canonical declaration of kind %s. Found declarations: %A" name declarations
                failwithf "Symbol.Kind: Failed to retrieve a canonical declaration of kind %s" name
                )
        let inline private pairWithType (symbol: ISymbol) node =
            node
            |> Option.map (fun node ->
                node, (ISymbol.checker symbol).getTypeOfSymbolAtLocation(symbol, node))
        let inline private pairWithTypes (symbol: ISymbol) nodes =
            nodes
            |> Option.map (NonEmptyArray.map (Some >> pairWithType symbol >> Option.get))
        let inline private pairWithTypeByMap (symbol: ISymbol) map node =
            node |> Option.map (fun node ->
                node, (ISymbol.checker symbol).getTypeOfSymbolAtLocation(symbol, map node))
        let inline private pairWithTypesByMap (symbol: ISymbol) map nodes =
            nodes |> Option.map (NonEmptyArray.map (Some >> pairWithTypeByMap symbol map >> Option.get))
            
        let tryValueDeclaration (symbol: #Transient.IValue) =
            symbol |> ISymbol.toSymbol |> _.valueDeclaration
            
        let tryParameterDeclaration (symbol: #Transient.IParameter) =
            ISymbol.tryPickDeclaration (Node.ParameterKind.create (ISymbol.program symbol)) symbol
        let tryParameterDeclarationAndType (symbol: #Transient.IParameter) =
            tryParameterDeclaration symbol
            |> pairWithTypeByMap symbol Node.ParameterKind.toDeclaration
            
        let tryVariableDeclaration (variable: #Transient.IVariable) =
            ISymbol.tryPickDeclaration Patterns.Node.(|VariableDeclaration|_|) variable
        let tryVariableDeclarationAndType (variable: #Transient.IVariable) =
            tryVariableDeclaration variable
            |> pairWithType variable
            
        let tryPropertyDeclarations (property: #Transient.IProperty) =
            ISymbol.chooseDeclarations (Node.PropertyKind.create (ISymbol.program property)) property
        let tryPropertyDeclarationsAndTypes (property: #Transient.IProperty) =
            tryPropertyDeclarations property
            |> pairWithTypesByMap property Node.PropertyKind.toDeclaration
        
        let tryEnumMemberDeclaration (enumMember: #Transient.IEnumMember) =
            ISymbol.tryPickDeclaration (Node.EnumMember.tryCreate (ISymbol.program enumMember)) enumMember
        let tryEnumMemberDeclarationAndType (enumMember: #Transient.IEnumMember)  =
            tryEnumMemberDeclaration enumMember
            |> pairWithTypeByMap enumMember Node.EnumMember.toDeclaration
        
        let tryFunctionDeclarations (symbol: #Transient.IFunction) =
            ISymbol.chooseDeclarations Patterns.Node.(|FunctionDeclaration|_|) symbol
        let tryFunctionDeclarationAndTypes (symbol: #Transient.IFunction) =
            tryFunctionDeclarations symbol
            |> pairWithTypes symbol 
        
        let tryClassDeclaration (symbol: #Transient.IClass) =
            ISymbol.tryPickDeclaration Patterns.Node.(|ClassDeclaration|_|) symbol
        let tryClassDeclarationAndType (symbol: #Transient.IClass) =
            tryClassDeclaration symbol
            |> pairWithType symbol
        
        let tryMethodDeclarations (symbol: #Transient.IMethod) =
            ISymbol.chooseDeclarations (Node.MethodKind.create (ISymbol.program symbol)) symbol
        let tryMethodDeclarationsAndTypes (symbol: #Transient.IMethod) =
            tryMethodDeclarations symbol
            |> pairWithTypesByMap symbol Node.MethodKind.toDeclaration
        
        let tryConstructorDeclaration (symbol: #Transient.IConstructor) =
            ISymbol.tryPickDeclaration Patterns.Node.(|ConstructorDeclaration|_|) symbol
        let tryConstructorDeclarationAndType (symbol: #Transient.IConstructor) =
            tryConstructorDeclaration symbol
            |> pairWithType symbol
            
        let trySignatureDeclarations (symbol: #Transient.ISignature) =
            ISymbol.tryPickDeclaration (Node.SignatureKind.create  (ISymbol.program symbol)) symbol
        let trySignatureDeclarationsAndTypes (symbol: #Transient.ISignature) =
            trySignatureDeclarations symbol
            |> pairWithTypeByMap symbol Node.SignatureKind.toDeclaration
        
        let tryEnumDeclaration (symbol: #Transient.IEnum) =
            ISymbol.chooseDeclarations Patterns.Node.(|EnumDeclaration|_|) symbol
        let tryEnumDeclarationAndType (symbol: #Transient.IEnum) =
            tryEnumDeclaration symbol
            |> pairWithTypes symbol
        
        let tryNamespaceDeclarations (symbol: #Transient.INamespace) =
            ISymbol.chooseDeclarations Patterns.Node.(|ModuleDeclaration|_|) symbol
        let tryNamespaceDeclarationsAndTypes (symbol: #Transient.INamespace) =
            tryNamespaceDeclarations symbol
            |> pairWithTypes symbol
        
        let tryModuleDeclarations (symbol: #Transient.IValueModule) =
            ISymbol.chooseDeclarations (Node.ModuleKind.tryCreate (ISymbol.program symbol)) symbol
        let tryModuleDeclarationsAndTypes (symbol: #Transient.IValueModule) =
            tryModuleDeclarations symbol
            |> pairWithTypesByMap symbol Node.ModuleKind.toDeclaration
        
        let tryTypeParameterDeclarations (symbol: #Transient.ITypeParameter) =
            ISymbol.chooseDeclarations Patterns.Node.(|TypeParameterDeclaration|_|) symbol
        let tryTypeParameterDeclarationsAndTypes (symbol: #Transient.ITypeParameter) =
            tryTypeParameterDeclarations symbol
            |> pairWithTypes symbol
        
        let tryTypeAliasDeclaration (symbol: #Transient.ITypeAlias) =
            ISymbol.tryPickDeclaration Patterns.Node.(|TypeAliasDeclaration|_|) symbol
        let tryTypeAliasDeclarationAndType (symbol: #Transient.ITypeAlias) =
            tryTypeAliasDeclaration symbol
            |> pairWithType symbol
        
        let tryInterfaceDeclarations (symbol: #Transient.IInterface) =
            ISymbol.chooseDeclarations Patterns.Node.(|InterfaceDeclaration|_|) symbol
        let tryInterfaceDeclarationsAndTypes (symbol: #Transient.IInterface) =
            tryInterfaceDeclarations symbol
            |> pairWithTypes symbol
        
        let tryGetAccessorDeclaration (symbol: #Transient.IGetAccessor) =
            ISymbol.tryPickDeclaration Patterns.Node.(|GetAccessorDeclaration|_|) symbol
        let tryGetAccessorDeclarationAndType (symbol: #Transient.IGetAccessor) =
            tryGetAccessorDeclaration symbol
            |> pairWithType symbol
        
        let trySetAccessorDeclaration (symbol: #Transient.ISetAccessor) =
            ISymbol.tryPickDeclaration Patterns.Node.(|SetAccessorDeclaration|_|) symbol
        let trySetAccessorDeclarationAndType (symbol: #Transient.ISetAccessor) =
            trySetAccessorDeclaration symbol
            |> pairWithType symbol
        
        let tryClassMemberDeclarations (symbol: #Transient.IClassMember) =
            ISymbol.chooseDeclarations (Node.ClassMemberKind.create (ISymbol.program symbol)) symbol
        let tryClassMemberDeclarationsAndTypes (symbol: #Transient.IClassMember) =
            tryClassMemberDeclarations symbol
            |> pairWithTypesByMap symbol Node.ClassMemberKind.toDeclaration
        
        let parameterDeclarations (symbol: #Transient.Parameter) =
            tryParameterDeclaration symbol |> failIfNone symbol
        let parameterDeclarationAndType (symbol: #Transient.Parameter) =
            tryParameterDeclarationAndType symbol |> failIfNone symbol
        
        let methodDeclarations (symbol: #Transient.Method) =
            tryMethodDeclarations symbol |> failIfNone symbol
        let methodDeclarationsAndTypes (symbol: #Transient.Method) =
            tryMethodDeclarationsAndTypes symbol |> failIfNone symbol
        
        let propertyDeclarations (symbol: #Transient.Property) =
            tryPropertyDeclarations symbol |> failIfNone symbol
        let propertyDeclarationsAndTypes (symbol: #Transient.Property) =
            tryPropertyDeclarationsAndTypes symbol |> failIfNone symbol
        
        let variableDeclaration (symbol: #Transient.Variable) =
            tryVariableDeclaration symbol |> failIfNone symbol
        let variableDeclarationAndType (symbol: #Transient.Variable) =
            tryVariableDeclarationAndType symbol |> failIfNone symbol
        
        let enumMemberDeclaration (symbol: #Transient.EnumMember) =
            tryEnumMemberDeclaration symbol |> failIfNone symbol
        let enumMemberDeclarationAndType (symbol: #Transient.EnumMember) =
            tryEnumMemberDeclarationAndType symbol |> failIfNone symbol
        
        let functionDeclarations (symbol: #Transient.Function) =
            tryFunctionDeclarations symbol |> failIfNone symbol
        let functionDeclarationAndTypes (symbol: #Transient.Function) =
            tryFunctionDeclarationAndTypes symbol |> failIfNone symbol
        
        let classDeclaration (symbol: #Transient.Class) =
            tryClassDeclaration symbol |> failIfNone symbol
        let classDeclarationAndType (symbol: #Transient.Class) =
            tryClassDeclarationAndType symbol |> failIfNone symbol
        
        let interfaceDeclarations (symbol: #Transient.Interface) =
            tryInterfaceDeclarations symbol |> failIfNone symbol
        let interfaceDeclarationsAndTypes (symbol: #Transient.Interface) =
            tryInterfaceDeclarationsAndTypes symbol |> failIfNone symbol
    
        let typeAliasDeclarations (symbol: #Transient.TypeAlias) =
            tryTypeAliasDeclaration symbol |> failIfNone symbol
        let typeAliasDeclarationAndType (symbol: #Transient.TypeAlias) =
            tryTypeAliasDeclarationAndType symbol |> failIfNone symbol
        
        let constructorDeclaration (symbol: #Transient.Constructor) =
            tryConstructorDeclaration symbol |> failIfNone symbol
        let constructorDeclarationAndType (symbol: #Transient.Constructor) =
            tryConstructorDeclarationAndType symbol |> failIfNone symbol
        
        let signatureDeclarations (symbol: #Transient.Signature) =
            trySignatureDeclarations symbol |> failIfNone symbol
        let signatureDeclarationAndType (symbol: #Transient.Signature) =
            trySignatureDeclarationsAndTypes symbol |> failIfNone symbol
        
        let constEnumDeclarations (symbol: #Transient.ConstEnum) =
            tryEnumDeclaration symbol |> failIfNone symbol
        let constEnumDeclarationAndTypes (symbol: #Transient.ConstEnum) =
            tryEnumDeclarationAndType symbol |> failIfNone symbol
        
        let typeEnumDeclarations (symbol: #Transient.TypeEnum) =
            tryEnumDeclaration symbol |> failIfNone symbol
        let typeEnumDeclarationAndTypes (symbol: #Transient.TypeEnum) =
            tryEnumDeclarationAndType symbol |> failIfNone symbol
        
        let moduleDeclarations (symbol: #Transient.ValueModule) =
            tryModuleDeclarations symbol |> failIfNone symbol
        let moduleDeclarationAndTypes (symbol: #Transient.ValueModule) =
            tryModuleDeclarationsAndTypes symbol |> failIfNone symbol
        
        let namespaceDeclarations (symbol: #Transient.NamespaceModule) =
            tryNamespaceDeclarations symbol |> failIfNone symbol
        let namespaceDeclarationsAndTypes (symbol: #Transient.NamespaceModule) =
            tryNamespaceDeclarationsAndTypes symbol |> failIfNone symbol
        
        let getAccessorDeclaration (symbol: #Transient.GetAccessor) =
            tryGetAccessorDeclaration symbol |> failIfNone symbol
        let getAccessorDeclarationAndType (symbol: #Transient.GetAccessor) =
            tryGetAccessorDeclarationAndType symbol |> failIfNone symbol
        
        let setAccessorDeclaration (symbol: #Transient.SetAccessor) =
            trySetAccessorDeclaration symbol |> failIfNone symbol
        let setAccessorDeclarationAndType (symbol: #Transient.SetAccessor) =
            trySetAccessorDeclarationAndType symbol |> failIfNone symbol
        
        let typeParameterDeclarations (symbol: #Transient.TypeParameter) =
            tryTypeParameterDeclarations symbol |> failIfNone symbol
        let typeParameterDeclarationsAndTypes (symbol: #Transient.TypeParameter) =
            tryTypeParameterDeclarationsAndTypes symbol |> failIfNone symbol
        
        type CanonicalDeclarationSRTPHelper =
            static member inline GetCanonicalDeclaration (symbol: Transient.TypeParameter) = typeParameterDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.TypeAlias) = typeAliasDeclarations symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.Interface) = interfaceDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.Class) = classDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.Method) = methodDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.Property) = propertyDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.Variable) = variableDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.Function) = functionDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.Constructor) = constructorDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.Signature) = signatureDeclarations symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.GetAccessor) = getAccessorDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.SetAccessor) = setAccessorDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.EnumMember) = enumMemberDeclaration symbol
            static member inline GetCanonicalDeclaration (symbol: Transient.ConstEnum) = constEnumDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.TypeEnum) = typeEnumDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.ValueModule) = moduleDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.NamespaceModule) = namespaceDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclaration (symbol: Transient.Parameter) = parameterDeclarations symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Parameter) = parameterDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.TypeParameter) = typeParameterDeclarationsAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.TypeAlias) = typeAliasDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Interface) = interfaceDeclarationsAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Class) = classDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Method) = methodDeclarationsAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Property) = propertyDeclarationsAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Variable) = variableDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Function) = functionDeclarationAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Constructor) = constructorDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.Signature) = signatureDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.GetAccessor) = getAccessorDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.SetAccessor) = setAccessorDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.EnumMember) = enumMemberDeclarationAndType symbol
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.ConstEnum) =
                constEnumDeclarationAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.TypeEnum) =
                typeEnumDeclarationAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.ValueModule) =
                moduleDeclarationAndTypes symbol |> _.Value
            static member inline GetCanonicalDeclarationAndType (symbol: Transient.NamespaceModule) = namespaceDeclarationsAndTypes symbol |> _.Value
        
        let inline canonicalDeclaration symbol =
            ((^T or CanonicalDeclarationSRTPHelper):(static member GetCanonicalDeclaration: ^T -> ^U) symbol)
        let inline canonicalDeclarationAndType symbol =
            ((^T or CanonicalDeclarationSRTPHelper):(static member GetCanonicalDeclarationAndType: ^T -> ^U) symbol)
        
        let parameterValueDeclaration (symbol: Parameter) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Node.ParameterKind.create (ISymbol.program symbol)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Parameter was an unexpected kind")
        let methodValueDeclaration (symbol: Method) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Node.MethodKind.create (ISymbol.program symbol)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Method was an unexpected kind")
        let propertyValueDeclaration (symbol: Property) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Node.PropertyKind.create (ISymbol.program symbol)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Property was an unexpected kind")
        let classValueDeclaration (symbol: Class) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|ClassDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Class was an unexpected kind")
        let variableValueDeclaration (symbol: Symbol.Variable) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|VariableDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Variable was an unexpected kind")
        let enumMemberValueDeclaration (symbol: Symbol.EnumMember) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|EnumMember|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of EnumMember was an unexpected kind")
        let functionValueDeclaration (symbol: Function) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|FunctionDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Function was an unexpected kind")
        let constEnumValueDeclaration (symbol: ConstEnum) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|EnumDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of ConstEnum was an unexpected kind")
        let valueModuleValueDeclaration (symbol: ValueModule) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Node.ModuleKind.tryCreate (ISymbol.program symbol)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of ValueModule was an unexpected kind")
        let getAccessorValueDeclaration (symbol: GetAccessor) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|GetAccessorDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of GetAccessor was an unexpected kind")
        let setAccessorValueDeclaration (symbol: SetAccessor) =
            tryValueDeclaration symbol
            |> failIfNone symbol
            |> Patterns.Node.(|SetAccessorDeclaration|_|)
            |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of SetAccessor was an unexpected kind")
        
        type ValueDeclarationSRTPHelper =
            static member inline GetValueDeclaration symbol = parameterValueDeclaration symbol
            static member inline GetValueDeclaration symbol = methodValueDeclaration symbol
            static member inline GetValueDeclaration symbol = propertyValueDeclaration symbol
            static member inline GetValueDeclaration symbol = classValueDeclaration symbol
            static member inline GetValueDeclaration symbol = variableValueDeclaration symbol
            static member inline GetValueDeclaration symbol = enumMemberValueDeclaration symbol
            static member inline GetValueDeclaration symbol = functionValueDeclaration symbol
            static member inline GetValueDeclaration symbol = constEnumValueDeclaration symbol
            static member inline GetValueDeclaration symbol = valueModuleValueDeclaration symbol
            static member inline GetValueDeclaration symbol = getAccessorValueDeclaration symbol
            static member inline GetValueDeclaration symbol = setAccessorValueDeclaration symbol
        
        let inline valueDeclaration symbol =
            ((^T or ValueDeclarationSRTPHelper):(static member GetValueDeclaration: ^T -> ^U) symbol)
        
        let isTransient (symbol: #ITransient) = ISymbol.hasFlag Ts.SymbolFlags.Transient symbol
        let isOptional (symbol: #IOptional) = ISymbol.hasFlag Ts.SymbolFlags.Optional symbol
        
        module Kind =
            let create = createKind
    
    module Type =
        type private TF = Ts.TypeFlags
        let private primitiveSingletonFlags =
            TF.Number
            ||| TF.String
            ||| TF.Boolean
            ||| TF.ESSymbol
            ||| TF.NonPrimitive
            ||| TF.BigInt
            ||| TF.Undefined
            ||| TF.Null
            ||| TF.Void
            ||| TF.Never
            ||| TF.Any
            ||| TF.Unknown
        let private primitiveInstantiableFlags =
            TF.Index
            ||| TF.TemplateLiteral
            ||| TF.StringMapping
        let private nonPrimitiveInstantiableFlags =
            TF.TypeParameter
            ||| TF.IndexedAccess
            ||| TF.Conditional
            ||| TF.Substitution
        let private literalFlags =
            TF.StringLiteral
            ||| TF.NumberLiteral
            ||| TF.BigIntLiteral
            ||| TF.BooleanLiteral
            ||| TF.EnumLiteral
            ||| TF.UniqueESSymbol
        let private enumFlags = TF.Enum 
        let private structuralFlags =
            TF.Object
            ||| TF.Union
            ||| TF.Intersection
        let inline private masked (mask: TF) (flags: TF) = flags &&& mask <> enum 0
        module PrimitiveSingleton =
            let isPrimitiveSingleton (typ: Ts.Type) = masked primitiveSingletonFlags typ.flags
                
            let unsafeCreate (typ: Ts.Type) : Type.PrimitiveSingleton =
                let inline hasFlag flag = typ.flags.HasFlag flag
                if hasFlag TF.Any then
                    Type.PrimitiveSingleton.Any typ
                elif hasFlag TF.Unknown then
                    Type.PrimitiveSingleton.Unknown typ
                elif hasFlag TF.Never then
                    Type.PrimitiveSingleton.Never typ
                elif hasFlag TF.Void then
                    Type.PrimitiveSingleton.Void typ
                elif hasFlag TF.Undefined then
                    Type.PrimitiveSingleton.Undefined typ
                elif hasFlag TF.Null then
                    Type.PrimitiveSingleton.Null typ
                elif hasFlag TF.String then
                    Type.PrimitiveSingleton.String typ
                elif hasFlag TF.Number then
                    Type.PrimitiveSingleton.Number typ
                elif hasFlag TF.Boolean then
                    Type.PrimitiveSingleton.Boolean typ
                elif hasFlag TF.BigInt then
                    Type.PrimitiveSingleton.BigInt typ
                elif hasFlag TF.ESSymbol then
                    Type.PrimitiveSingleton.ESSymbol typ
                elif hasFlag TF.NonPrimitive then
                    Type.PrimitiveSingleton.NonPrimitive typ
                else failwith "Unknown primitive singleton"
            let tryCreate _ (typ: Ts.Type) =
                if not <| isPrimitiveSingleton typ then None else
                unsafeCreate typ
                |> Some
            let toType: Type.PrimitiveSingleton -> Ts.Type = _.Value
            let getType = toType
        module PrimitiveLiteral =
            open Type
            let isPrimitiveLiteral (typ: Ts.Type) = masked literalFlags typ.flags
            let unsafeCreate(typ: Ts.Type): Type.PrimitiveLiteral =
                if typ.flags.HasFlag TF.StringLiteral then
                    typ :?> Ts.StringLiteralType
                    |> Type.PrimitiveLiteral.String
                elif typ.flags.HasFlag TF.NumberLiteral then
                    typ :?> Ts.NumberLiteralType
                    |> Type.PrimitiveLiteral.Number
                elif typ.flags.HasFlag TF.BigIntLiteral then
                    typ :?> Ts.BigIntLiteralType
                    |> Type.PrimitiveLiteral.BigInt
                elif typ.flags.HasFlag TF.BooleanLiteral then
                    typ :?> Ts.LiteralType
                    |> Type.PrimitiveLiteral.Boolean
                else failwithf "Unknown primitive literal: %A" (typ.flags.ToStringArray())
            let tryCreate _ (typ: Ts.Type) =
                if not <| isPrimitiveLiteral typ then None else
                unsafeCreate typ
                |> Some
            let getType = function
                | PrimitiveLiteral.BigInt typ -> typ :> Ts.LiteralType
                | PrimitiveLiteral.Boolean typ -> typ
                | PrimitiveLiteral.Number typ -> typ
                | PrimitiveLiteral.String typ -> typ
            let toType = getType
            
        module EnumMember =
            let isEnumMember (typ: Ts.Type) =
                typ.flags.HasFlag TF.EnumLiteral
                && (typ.flags &&& (TF.Enum ||| TF.Union) = enum 0)
            let unsafeCreate program (typ: Ts.Type) =
                PrimitiveLiteral.unsafeCreate typ
                |> ParentInlinedProgram.wrap program Type.EnumMember
            let tryCreate program typ =
                if not <| isEnumMember typ then None else
                unsafeCreate program typ
                |> Some
            let getType (typ: Type.EnumMember) =
                match typ with Type.EnumMember typ -> PrimitiveLiteral.getType typ.Value
            let toType = getType
            let getSymbolKind (typ: Type.EnumMember) =
                getType typ
                |> _.unsafeGetCanonicalSymbol()
                |> Symbol.createKind typ.program
                |> function
                    | Symbol.Kind.EnumMember _ as symbol -> symbol
                    | symbolKind ->
                        Logging.Log.Default.logfw "Unexpected symbol kind for EnumMember type: %A" symbolKind
                        symbolKind
        module UniqueESSymbol =
            let isUniqueESSymbol (typ: Ts.Type) = typ.flags.HasFlag TF.UniqueESSymbol
            let create program (typ: Ts.UniqueESSymbolType) =
                Type.UniqueESSymbol.UniqueESSymbol typ
                |> InlinedProgram.inject program

        module Literal =
            open Type
            let unsafeCreate program (typ: Ts.Type): Type.Literal =
                if typ.flags.HasFlag TF.EnumLiteral then
                    EnumMember.unsafeCreate program typ
                    |> Type.Literal.EnumMember
                elif typ.flags.HasFlag TF.UniqueESSymbol then
                    typ :?> Ts.UniqueESSymbolType
                    |> Type.UniqueESSymbol.UniqueESSymbol
                    |> Type.Literal.UniqueESSymbol
                elif typ.flags |> masked literalFlags then
                    PrimitiveLiteral.unsafeCreate typ
                    |> Type.Literal.PrimitiveLiteral
                else failwith "Unknown literal"
            let isLiteral (typ: Ts.Type) =
                let literalLike = typ.flags |> masked (literalFlags ||| TF.UniqueESSymbol)
                literalLike || EnumMember.isEnumMember typ
            let tryCreate program (typ: Ts.Type) =
                if not(isLiteral typ) then None else
                unsafeCreate program typ |> Some
            let toType: Type.Literal -> Ts.Type = function
                | Literal.UniqueESSymbol typ -> typ.Value
                | Literal.PrimitiveLiteral typ -> PrimitiveLiteral.getType typ
                | Literal.EnumMember typ -> EnumMember.getType typ
            let getType = toType
        [<RequireQualifiedAccess>]
        module Primitive =
            open Type
            let toType: Type.Primitive -> Ts.Type = function
                | Primitive.Singleton typ -> PrimitiveSingleton.toType typ
                | Primitive.Literal typ -> Literal.toType typ
            let getType = toType
            let unsafeCreate program (typ: Ts.Type) =
                if typ.flags |> masked primitiveSingletonFlags then
                    PrimitiveSingleton.unsafeCreate typ
                    |> Type.Primitive.Singleton
                elif typ.flags |> masked literalFlags then
                    Literal.unsafeCreate program typ
                    |> Type.Primitive.Literal
                else failwith "Unknown primitive"
            let isPrimitive (typ: Ts.Type) =
                typ.flags |> masked (primitiveSingletonFlags ||| literalFlags)
            let tryCreate program (typ: Ts.Type) =
                if not(isPrimitive typ) then None else
                unsafeCreate program typ |> Some
        module TypeParameter =
            let inline create _ (typ: Ts.TypeParameter) = Type.TypeParameter.TypeParameter typ
            let inline toType (this: Type.TypeParameter) = this.Value
            let getType = toType
            let symbol typ = toType typ |> _.unsafeGetCanonicalSymbol()
            /// <summary>
            /// If a type parameter has a symbol without the type parameter flag, then it is a 'this' typar.
            /// </summary>
            /// <param name="typ"></param>
            let isThisTypeParameter typ =
                symbol typ
                |> _.flags.HasFlag(Ts.TypeFlags.TypeParameter)
                |> not
        module InstantiableNonPrimitive =
            open Type
            let unsafeCreate (typ: Ts.Type) =
                if typ.flags.HasFlag TF.Conditional then
                    typ :?> Ts.ConditionalType
                    |> Type.Conditional.Conditional
                    |> Type.InstantiableNonPrimitive.Conditional
                elif typ.flags.HasFlag TF.IndexedAccess then
                    typ :?> Ts.IndexedAccessType
                    |> Type.IndexedAccess.IndexedAccess
                    |> Type.InstantiableNonPrimitive.IndexedAccess
                elif typ.flags.HasFlag TF.Substitution then
                    typ :?> Ts.SubstitutionType
                    |> Type.Substitution.Substitution
                    |> Type.InstantiableNonPrimitive.Substitution
                elif typ.flags.HasFlag TF.TypeParameter then
                    typ :?> Ts.TypeParameter
                    |> Type.TypeParameter.TypeParameter
                    |> Type.InstantiableNonPrimitive.TypeParameter
                else failwith "Unknown instantiable non-primitive"
            let isInstantiableNonPrimitive (typ: Ts.Type) =
                typ.flags |> masked nonPrimitiveInstantiableFlags
            let tryCreate(typ: Ts.Type): Type.InstantiableNonPrimitive option =
                if not(isInstantiableNonPrimitive typ) then None else
                Some (unsafeCreate typ)
            let toType: InstantiableNonPrimitive -> Ts.InstantiableType = _.Value
            let getType = toType
        module StringMapping =
            let unsafeCreate (typ: Ts.Type) =
                let typ = typ :?> Ts.StringMappingType
                match typ.unsafeGetCanonicalSymbol().name with
                | "Capitalize" -> Type.StringMapping.Capitalize typ
                | "Lowercase" -> Type.StringMapping.Lowercase typ
                | "Uppercase" -> Type.StringMapping.Uppercase typ
                | "Uncapitalize" -> Type.StringMapping.Uncapitalize typ
                | v -> failwithf "Unknown string mapping: %s" v
            let isStringMapping (typ: Ts.Type) =
                typ.flags.HasFlag TF.StringMapping
            let tryCreate (typ: Ts.Type) =
                if not(isStringMapping typ) then None else
                Some (unsafeCreate typ)
            let toType: Type.StringMapping -> Ts.StringMappingType = _.Value
            let getType = toType
        module InstantiablePrimitive =
            open Type
            let unsafeCreate (typ: Ts.Type) =
                if typ.flags.HasFlag TF.Index then
                    typ :?> Ts.IndexType
                    |> Type.Index.Index
                    |> Type.InstantiablePrimitive.Index
                elif typ.flags.HasFlag TF.TemplateLiteral then
                    typ :?> Ts.TemplateLiteralType
                    |> Type.TemplateLiteral.TemplateLiteral
                    |> Type.InstantiablePrimitive.TemplateLiteral
                elif typ.flags.HasFlag TF.StringMapping then
                    StringMapping.unsafeCreate typ 
                    |> Type.InstantiablePrimitive.StringMapping
                else failwith "Unknown instantiable primitive"
            let isInstantiablePrimitive (typ: Ts.Type) =
                typ.flags |> masked primitiveInstantiableFlags
            let tryCreate (typ: Ts.Type) =
                if not(isInstantiablePrimitive typ) then None else
                Some (unsafeCreate typ)
            let toType: InstantiablePrimitive -> Ts.Type = function
                | InstantiablePrimitive.Index typ -> typ.Value
                | InstantiablePrimitive.TemplateLiteral typ -> typ.Value
                | InstantiablePrimitive.StringMapping typ -> StringMapping.toType typ
            let getType = toType
        module Instantiable =
            open Type
            let unsafeCreate (typ: Ts.Type) =
                if typ.flags |> masked primitiveInstantiableFlags then
                    InstantiablePrimitive.unsafeCreate typ
                    |> Type.Instantiable.Primitive
                elif typ.flags |> masked nonPrimitiveInstantiableFlags then
                    InstantiableNonPrimitive.unsafeCreate typ
                    |> Type.Instantiable.NonPrimitive
                else failwith "Unknown instantiable"
            let isInstantiable (typ: Ts.Type) =
                typ.flags |> masked (primitiveInstantiableFlags ||| nonPrimitiveInstantiableFlags)
            let tryCreate (typ: Ts.Type) =
                if not(isInstantiable typ) then None else
                unsafeCreate typ |> Some
            let toType: Instantiable -> Ts.Type = function
                | Instantiable.Primitive typ -> InstantiablePrimitive.toType typ
                | Instantiable.NonPrimitive typ -> InstantiableNonPrimitive.toType typ
        type private OF = Ts.ObjectFlags
        
        module Anonymous =
            let toType: Type.Anonymous -> Ts.Type = _.Value
            let getType = toType
            let unsafeCreate (typ: Ts.ObjectType) =
                let objectFlags = typ.objectFlags
                let hasFlag flag = objectFlags.HasFlag flag
                if hasFlag OF.ObjectRestType then
                    Type.Anonymous.ObjectRest typ
                elif hasFlag OF.InstantiationExpressionType then
                    Type.Anonymous.InstantiationExpression typ
                elif hasFlag OF.Instantiated then
                    Type.Anonymous.Instantiated typ
                elif hasFlag OF.Anonymous then
                    Type.Anonymous.Anonymous typ
                else failwith "Unknown anonymous"
            let isAnonymous (typ: Ts.Type) =
                Structural.isObject typ
                && typ :?> Ts.ObjectType |> _.objectFlags.HasFlag(OF.Anonymous)
            let tryCreate (typ: Ts.Type) =
                if not(isAnonymous typ) then None else
                typ :?> Ts.ObjectType |> unsafeCreate |> Some
        module Mapped =
            let toType: Type.Mapped -> Ts.Type = _.Value
            let getType = toType
            let unsafeCreate (typ: Ts.ObjectType) =
                let objectFlags = typ.objectFlags
                let hasFlag flag = objectFlags.HasFlag flag
                if hasFlag OF.Instantiated then
                    Type.Mapped.Instantiated typ
                elif hasFlag OF.Mapped then
                    Type.Mapped.Mapped typ
                else failwith "Unknown mapped"
            let isMapped (typ: Ts.Type) =
                Structural.isObject typ
                && typ :?> Ts.ObjectType |> _.objectFlags.HasFlag(OF.Mapped)
            let tryCreate (typ: Ts.Type) =
                if not(isMapped typ) then None else
                typ :?> Ts.ObjectType |> unsafeCreate |> Some
        module TypeReference =
            let toType: Type.TypeReference -> Ts.Type = _.Value
            let getType = toType
            let isTypeReference (typ: Ts.Type) =
                // must be object
                Structural.isObject typ
                // has ref flag
                && typ :?> Ts.ObjectType |> _.objectFlags.HasFlag(OF.Reference)
                // and is not just a class/interface with a 'this' ref (we don't count that)
                && (
                    if typ :?> Ts.ObjectType |> _.objectFlags |> (&&&) OF.ClassOrInterface |> (=) (enum 0)
                    then typ :?> Ts.TypeReference |> _.typeArguments.IsNone |> not
                    else true
                )
            let unsafeCreate (typ: Ts.TypeReference) =
                let objectFlags = typ.objectFlags
                let hasFlag flag = objectFlags.HasFlag flag
                if hasFlag OF.Tuple then
                    Type.TupleReference.TupleReference typ
                    |> Type.TypeReference.Tuple
                elif hasFlag OF.Class then
                    Type.ClassReference.ClassReference typ
                    |> Type.TypeReference.Class
                elif hasFlag OF.Interface then
                    Type.InterfaceReference.InterfaceReference typ
                    |> Type.TypeReference.Interface
                elif typ.node |> Option.exists (unbox >> Patterns.Node.(|ArrayTypeNode|_|) >> Option.isSome) then
                    Type.ArrayReference.ArrayReference typ
                    |> Type.TypeReference.Array
                elif hasFlag OF.Reference then
                    Type.PureTypeReference.TypeReference typ
                    |> Type.TypeReference.Pure
                else failwith "Unknown type reference"
            let tryCreate (typ: Ts.Type) =
                if not(isTypeReference typ) then None else
                unsafeCreate (typ :?> Ts.TypeReference) |> Some
        module Intersection =
            let toType: Type.Intersection -> Ts.IntersectionType = _.Value
            let inline create (typ: Ts.IntersectionType) = Type.Intersection.Intersection typ
            let types intersection =
                toType intersection
                |> _.types
                |> NonEmptyArray.create
                |> Option.defaultWith (fun () -> failwith "Intersection type has no types")
                |> NonEmptyArray.map (Kind.create intersection.Value.program)
        module Union =
            let toType: Type.Union -> Ts.UnionType = _.Value
            let inline create (typ: Ts.UnionType) =
                Type.Union.Union typ
            let types union =
                toType union
                |> _.types
                |> NonEmptyArray.create
                |> Option.defaultWith (fun () -> failwith "Union type has no types")
                |> NonEmptyArray.map (Kind.create union.Value.program)
                
        module Structural =
            open Type
            let toType = function
                | Structural.Anonymous typ -> typ.Value :> Ts.Type
                | Structural.Mapped typ -> typ.Value 
                | Structural.TypeReference typ -> typ.Value 
                | Structural.Union typ -> typ.Value 
                | Structural.Intersection typ -> typ.Value 
                | Structural.Class typ -> typ.Value 
                | Structural.Interface typ -> typ.Value 
            let getType = toType
            let isObject (typ: Ts.Type) = typ.flags.HasFlag TF.Object
            let isStructural (typ: Ts.Type) = typ.flags |> masked structuralFlags
            let unsafeCreateObjectType (typ: Ts.ObjectType) =
                let objectFlags = typ.objectFlags
                let hasFlag flag = objectFlags.HasFlag flag
                if hasFlag OF.Anonymous then
                    Anonymous.unsafeCreate typ
                    |> Type.Structural.Anonymous
                elif hasFlag OF.Mapped then
                    Mapped.unsafeCreate typ
                    |> Type.Structural.Mapped
                elif
                    hasFlag OF.Interface
                    && not (hasFlag OF.Reference && typ :?> Ts.TypeReference |> typ.checker.getTypeArguments |> Seq.isEmpty |> not)
                then
                    typ :?> Ts.InterfaceType
                    |> Type.Interface.Interface
                    |> Type.Structural.Interface
                elif
                    hasFlag OF.Class
                    && not (hasFlag OF.Reference && typ :?> Ts.TypeReference |> typ.checker.getTypeArguments |> Seq.isEmpty |> not)
                then
                    typ :?> Ts.InterfaceType
                    |> Type.Class.Class
                    |> Type.Structural.Class
                elif hasFlag OF.Reference then
                    typ :?> Ts.TypeReference
                    |> TypeReference.unsafeCreate
                    |> Type.Structural.TypeReference
                else failwith "Unknown structural of ObjectType"
            let unsafeCreate (typ: Ts.Type) =
                if typ.flags.HasFlag TF.Union then
                    typ :?> Ts.UnionType
                    |> Type.Union.Union
                    |> Type.Structural.Union
                elif typ.flags.HasFlag TF.Intersection then
                    typ :?> Ts.IntersectionType
                    |> Type.Intersection.Intersection
                    |> Type.Structural.Intersection
                elif typ.flags.HasFlag TF.Object then
                    typ :?> Ts.ObjectType
                    |> unsafeCreateObjectType
                else failwith "Unknown structural"
            let tryCreate (typ: Ts.Type) =
                if not(typ.flags |> masked structuralFlags) then None else
                unsafeCreate typ
                |> Some
        module Enum =
            let inline private isStandardEnumType (typ: Ts.Type) = typ.flags.HasFlag TF.Enum
            let inline private isAlternateEnumComposite (typ: Ts.Type) =
                let altSet = TF.Union ||| TF.EnumLiteral
                typ.flags &&& altSet = altSet
            /// Proved by EN - ENUM RESOLUTION
            let isEnum (typ: Ts.Type) =
                isStandardEnumType typ
                || isAlternateEnumComposite typ
            let getSymbol (typ: Type.Enum) =
                typ.Value.unsafeGetCanonicalSymbol()
                |> Symbol.createKind typ.program
                |> function
                    | Symbol.Kind.TypeEnum _ as symbol -> symbol
                    | symbolKind ->
                        Logging.Log.Default.logfw "Unexpected symbol kind: %A" symbolKind
                        symbolKind
            let toType: Type.Enum -> Ts.Type = _.Value
            let getType = toType
                
        module Kind =
            open Type
            let isErrorStub (typ: Ts.Type) =
                typ.flags.HasFlag TF.Any
                && typ?intrinsicName = "error"
            /// <summary>
            /// If we fail to retrieve a type from the node, then a None error is returned.
            /// If we get a type, but it is a error type with/without a symbol, then that information
            /// is returned as part of the error.
            /// This would indicate it is a good idea to handle the node declaration rather than the type.
            /// </summary>
            /// <param name="program"></param>
            /// <param name="node"></param>
            let tryCreateFromNode (program: Ts.Program) (node: Ts.Node) =
                try
                    if ts.isTypeNode node
                    then program.getTypeChecker().getTypeFromTypeNode (node :?> Ts.TypeNode)
                    else program.getTypeChecker().getTypeAtLocation(node)
                    |> Ok
                with _ -> Result.Error(None)
                |> Result.bind (function
                    | typ when typ.getCanonicalSymbol().IsNone ->
                        Some Type.TypeValidationError.SymbolessErrorType
                        |> Result.Error
                    | typ when isErrorStub typ ->
                        Some Type.TypeValidationError.ErrorType
                        |> Result.Error
                    | typ ->
                        create program typ
                        |> Ok
                    )
            let tryCreateFromSymbol (program: Ts.Program) (symbol: Ts.Symbol) =
                ISymbol.create program symbol
                |> ISymbol.declaredType
                |> function
                    | typ when isErrorStub typ ->
                        Type.TypeValidationError.ErrorType |> Result.Error
                    | typ ->
                        create program typ
                        |> Ok
            let create (program: Ts.Program) (typ: Ts.Type) =
                let primitiveFlags =
                    primitiveSingletonFlags
                    ||| literalFlags
                let instantiableFlags =
                    primitiveInstantiableFlags
                    ||| nonPrimitiveInstantiableFlags
                let flags = typ.flags
                if Enum.isEnum typ then
                    typ :?> Ts.EnumType
                    |> InlinedProgram.create program
                    |> Type.Enum.Enum 
                    |> Type.Kind.Enum
                elif flags |> masked primitiveFlags then
                    // primitive
                    Primitive.unsafeCreate program typ
                    |> Type.Kind.Primitive
                elif flags |> masked instantiableFlags then
                    Instantiable.unsafeCreate typ
                    |> Type.Kind.Instantiable
                elif flags |> masked structuralFlags then
                    Structural.unsafeCreate typ
                    |> Type.Kind.Structural
                else failwith "Unknown type kind"
            let toType = function
                | Type.Kind.Primitive typ -> Primitive.toType typ
                | Type.Kind.Enum typ -> Enum.toType typ
                | Type.Kind.Instantiable typ -> Instantiable.toType typ
                | Type.Kind.Structural typ -> Structural.toType typ
            let getType = toType
    module InternalTracer =
        open InternalTracer
        let inline get (program: Ts.Program) =
            match Tracer.get<Ts.Program> program with
            | ValueSome tracer ->
                tracer.Imprint
                tracer :?> InternalTracer.ProgramTracerMap
            | ValueNone ->
                let tracer = Tracer.unsafeCreate program program :?> InternalTracer.ProgramTracerMap
                tracer.Imprint
                tracer
        let inline private getSymbolMap program =
            get program
            |> _.SymbolMap
        let inline private getNodeMap program =
            get program
            |> _.NodeMap
        let inline private getTypeMap program =
            get program
            |> _.TypeMap
        let getOrAddSymbol (symbolTracerFn: Ts.Program -> Ts.Symbol -> SymbolTracer) (program: Ts.Program) (symbol: Ts.Symbol) =
            let map = getSymbolMap program
            let symbolKey = SymbolKey.fromSymbol symbol
            match map.TryGetValue(symbolKey) with
            | true, symbolTracer -> symbolTracer :?> SymbolTracer
            | _ ->
                let symbolTracer = symbolTracerFn program symbol
                map[symbolKey] <- symbolTracer
                symbolTracer
        let getOrAddNode (nodeTracerFn: Ts.Program -> Ts.Node -> NodeTracer) (program: Ts.Program) (node: Ts.Node) =
            let map = getNodeMap program
            let nodeKey = NodeKey.fromNode node
            match map.TryGetValue(nodeKey) with
            | true, nodeTracer -> nodeTracer :?> NodeTracer
            | _ ->
                let nodeTracer = nodeTracerFn program node
                map[nodeKey] <- nodeTracer
                nodeTracer
        let getOrAddType (typeTracerFn: Ts.Program -> Ts.Type -> TypeTracer) (program: Ts.Program) (typ: Ts.Type) =
            let map = getTypeMap program
            let typeKey = TypeKey.fromType typ
            match map.TryGetValue(typeKey) with
            | true, typeTracer -> typeTracer :?> TypeTracer
            | _ ->
                let typeTracer = typeTracerFn program typ
                map[typeKey] <- typeTracer
                typeTracer
            
    module NarrowedTracer =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let narrowedTraceSymbol = SymbolTypeKey.create<Dictionary<string, obj>> "NarrowedTracer"
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let hasNarrowedTracer (tracer: 'T when 'T :> Tracer<'MainKind>) =
            SymbolTypeKey.has narrowedTraceSymbol tracer
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline hasNarrowedTracerKey (tracer: NarrowedTracer<'MainKind, 'NarrowedType>) =
            SymbolTypeKey.accessOrInit narrowedTraceSymbol Dictionary tracer
            |> _.ContainsKey(typeof<'NarrowedType>.FullName)
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline getNarrowedTracerValue (tracer: NarrowedTracer<'MainKind, 'NarrowedType>) =
            if not <| hasNarrowedTracerKey tracer then
                failwithf "Narrowed tracer for type %s not found" typeof<'NarrowedType>.FullName
            SymbolTypeKey.unsafeAccess narrowedTraceSymbol tracer
            |> _.Item(typeof<'NarrowedType>.FullName) :?> 'NarrowedType
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline setNarrowedTracerValue (tracer: NarrowedTracer<'MainKind, 'NarrowedType>) (value: 'NarrowedType) =
            let map = SymbolTypeKey.accessOrInit narrowedTraceSymbol Dictionary tracer
            map[typeof<'NarrowedType>.FullName] <- value
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let inline setNarrowedTracerValueIfAbsent (tracer: NarrowedTracer<'MainKind, 'NarrowedType>) (value: 'NarrowedType) =
            if not <| hasNarrowedTracerKey tracer then
                setNarrowedTracerValue tracer value
        let inline createFunctionSet<'T, 'O, 'MainType, 'NarrowedType when 'O :> Tracer<'MainType> and 'T :> NarrowedTracer<'MainType, 'NarrowedType>> (mapToNarrowedValue: 'MainType -> 'NarrowedType option) =
            let isValid = fun (tracer: 'O) ->
                let tracer = unbox<'T> tracer
                (tracer |> hasNarrowedTracerKey ||
                    let value = mapToNarrowedValue tracer.Value
                    value
                    |> Option.map (setNarrowedTracerValue tracer >> fun _ -> true)
                    |> Option.defaultValue false)
                |> function
                    | true -> unbox<NarrowedTracer<'MainType, 'NarrowedType>> tracer |> Some
                    | _ -> None
            let getValue = fun (narrowedTracer: NarrowedTracer<'MainType, 'NarrowedType>) ->
                narrowedTracer
                |> SymbolTypeKey.unsafeAccess narrowedTraceSymbol
                |> _.Item(typeof<'NarrowedType>.FullName)
                |> unbox<'NarrowedType>
            {
                NarrowedTracer.tryGet = isValid
                NarrowedTracer.value = getValue
            }
        let inline value<^T, ^MainType, ^NarrowedType when ^T :> NarrowedTracer<^MainType, ^NarrowedType>> (tracer: ^T) =
            tracer
            |> SymbolTypeKey.unsafeAccess narrowedTraceSymbol
            |> _.Item(typeof<'NarrowedType>.FullName)
            |> unbox<'NarrowedType>
    module TypeTracer =
        open Type
        let private createImpl (program: Ts.Program) (typ: Ts.Type): TypeTracer =
            match Tracer.get<Type.Kind> typ with
            | ValueSome tracer ->
                tracer.Imprint
                tracer :?> TypeTracer
            | ValueNone ->
                let tracerValue = Type.Kind.create program typ
                let tracer = Tracer.unsafeCreate tracerValue typ :?> TypeTracer
                tracer.Imprint
                tracer.TypeKey <- TypeKey.fromType typ
                tracer.program <- program
                tracer.checker <- program.getTypeChecker()
                tracer
        let create program typ = InternalTracer.getOrAddType createImpl program typ
        let TypeParameter = NarrowedTracer.createFunctionSet<TypeTypeParameterTracer, TypeTracer, _, _> <| function
            // If the canonical symbol doesnt have the TypeParameter flag, then this type is a `this` type parameter.
            | Kind.Instantiable (Instantiable.NonPrimitive (InstantiableNonPrimitive.TypeParameter typar)) when Type.TypeParameter.isThisTypeParameter typar |> not -> Some typar
            | _ -> None
        let Class = NarrowedTracer.createFunctionSet<TypeClassTracer, TypeTracer, _, _> <| function
            | Kind.Structural (Structural.Class typ) ->
                Choice1Of2 typ
                |> Some
            | Kind.Structural(Structural.TypeReference (TypeReference.Class typ)) ->
                Choice2Of2 typ
                |> Some
            | _ -> None
        let UniqueESSymbol = NarrowedTracer.createFunctionSet<TypeUniqueESSymbolTracer, TypeTracer, _, _> <| function
            | Kind.Primitive (Primitive.Literal (Literal.UniqueESSymbol typ)) -> Some typ
            | _ -> None
    module SymbolTracer =
        let private createImpl (program: Ts.Program) (symbol: Ts.Symbol) =
            let symbol =
                ISymbol.create program symbol
                |> ISymbol.toSymbol
            match Tracer.get<Kind> symbol with
            | ValueSome tracer ->
                tracer.Imprint
                tracer :?> SymbolTracer
            | ValueNone ->
                let tracerValue = Symbol.createKind program symbol
                let tracer = Tracer.unsafeCreate tracerValue symbol :?> SymbolTracer
                tracer.Imprint
                tracer.SymbolKey <- SymbolKey.fromSymbol symbol
                tracer.program <- program
                tracer.checker <- program.getTypeChecker()
                tracer
        let create program symbol = InternalTracer.getOrAddSymbol createImpl program symbol
        let TypeParameter = NarrowedTracer.createFunctionSet<SymbolTypeParameterTracer, SymbolTracer, _, _> <| function
            | Kind.TypeParameter typ -> typ |> Choice1Of2 |> Some
            | Kind.Transient (Transient.Kind.TypeParameter typ) -> typ |> Choice2Of2 |> Some
            | _ -> None
        let ClassLike = NarrowedTracer.createFunctionSet<SymbolClassLikeTracer, SymbolTracer, _, _> <| function
            | Kind.Class symbol -> Some symbol
            | Kind.Interface symbol -> Some symbol
            | Kind.Function symbol -> Some symbol
            | Kind.Property symbol -> Some symbol
            | Kind.ValueModule symbol -> Some symbol
            | Kind.NamespaceModule symbol -> Some symbol
            | Kind.Transient transient ->
                match transient with
                | Transient.Kind.Class symbol -> Some symbol
                | Transient.Kind.Interface symbol -> Some symbol
                | Transient.Kind.Function symbol -> Some symbol
                | Transient.Kind.Property symbol -> Some symbol
                | Transient.Kind.ValueModule symbol -> Some symbol
                | Transient.Kind.NamespaceModule symbol -> Some symbol
                | _ -> None
            | _ -> None
        let Parameter = NarrowedTracer.createFunctionSet<SymbolParameterTracer, SymbolTracer, _, _> <| function
            | Kind.Parameter symbol -> Some symbol
            | Kind.Property symbol -> Some symbol
            | Kind.Interface symbol -> Some symbol
            | Kind.NamespaceModule symbol -> Some symbol
            | Kind.TypeParameter symbol -> Some symbol
            | Kind.TypeAlias symbol -> Some symbol
            // | Kind.Transient symbol ->
            //     match symbol with
            //     | Transient.Kind.Parameter symbol -> Some symbol
            //     | Transient.Kind.Property symbol -> Some symbol
            //     | Transient.Kind.Interface symbol -> Some symbol
            //     | Transient.Kind.NamespaceModule symbol -> Some symbol
            //     | Transient.Kind.TypeParameter symbol -> Some symbol
            //     | Transient.Kind.TypeAlias symbol -> Some symbol
            //     | _ -> None
            | _ -> None
        let UniqueESSymbol = NarrowedTracer.createFunctionSet<SymbolUniqueESSymbolTracer, SymbolTracer, _, _> <| function
            | Kind.Parameter symbol -> Choice1Of2 symbol |> Some
            | Kind.Property symbol -> Choice2Of2 symbol |> Some
            | _ -> None


    module NodeTracer =
        let private createImpl (program: Ts.Program) (node: Ts.Node) =
            match Tracer.get<Node.Kind> node with
            | ValueSome tracer ->
                tracer.Imprint
                tracer :?> NodeTracer
            | ValueNone ->
                let tracerValue = Node.Kind.create program node
                let tracer = Tracer.unsafeCreate tracerValue node :?> NodeTracer
                tracer.Imprint
                tracer.NodeKey <- NodeKey.fromNode node
                tracer.program <- program
                tracer.checker <- program.getTypeChecker()
                tracer
        let create program node = InternalTracer.getOrAddNode createImpl program node
        let DeclarationKind = NarrowedTracer.createFunctionSet<NodeDeclarationKindTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType kind -> Some kind
            | _ -> None
        let TypeParameter = NarrowedTracer.createFunctionSet<NodeTypeParameterTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType (Node.DeclarationKind.TypeParameter typ) -> Some typ
            | _ -> None
        let Type = NarrowedTracer.createFunctionSet<NodeTypeTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType (Node.DeclarationKind.Type typ) -> Some typ
            | _ -> None
        let ClassLike = NarrowedTracer.createFunctionSet<NodeClassTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType (Node.DeclarationKind.Class node) -> Some node.Value
            | _ -> None
        let Parameter = NarrowedTracer.createFunctionSet<NodeParameterTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType (Node.DeclarationKind.Parameter (Node.ParameterKind.Simple decl)) -> Some decl.Value
            | _ -> None
        let UniqueESSymbol = NarrowedTracer.createFunctionSet<NodeUniqueESSymbolTracer, NodeTracer, _, _> <| function
            | Node.Kind.DeclarationOrType declKind ->
                match declKind with
                | Node.DeclarationKind.Property node -> Choice1Of2 node |> Some
                | Node.DeclarationKind.Variable node -> Choice2Of2 node.Value |> Some
                | _ -> None
            | _ -> None
    module NodeTypeTracer =
        let create program (node: Ts.TypeNode) =
            NodeTracer.create program node
            |> NodeTracer.Type.tryGet
            |> Option.defaultWith (fun () ->
                let str = Utils.inspectTo 2 node
                let iso = Node.Type.create program node
                failwith $"Cannot create type tracer for non-type node: {str}\nIsolatedAttempt gave: %A{iso.ToString()}"
                )
            :?> NodeTypeTracer
        let tryFromNodeTracer =
            NodeTracer.Type.tryGet
            >> Option.map (fun tracer -> tracer :?> NodeTypeTracer)
        let type_ (tracer: NodeTypeTracer) =
            NarrowedTracer.value tracer
            |> Node.Type.getTypeNode
            |> tracer.checker.getTypeFromTypeNode
            |> TypeTracer.create tracer.program
            
    module SymbolTypeParameterTracer =
        let tryCreate program symbol =
            SymbolTracer.create program symbol
            |> SymbolTracer.TypeParameter.tryGet
            |> Option.map (fun t -> t :?> SymbolTypeParameterTracer)
        let tryFromSymbolTracer =
            SymbolTracer.TypeParameter.tryGet
            >> Option.map (fun t -> t :?> SymbolTypeParameterTracer)
        let unsafeCreate program symbol =
            tryCreate program symbol
            |> Option.defaultWith (fun () ->
                let flags = symbol.getFlags().ToStringArray()
                failwithf "Cannot create a SymbolTypeParameterTracer from a SymbolTracer for symbol with flags: %A" flags
                )
        let inline narrowedValue (tracer: SymbolTypeParameterTracer) = tracer |> NarrowedTracer.value
        let inline narrowedFoldedValue tracer = narrowedValue tracer |> function
            | Choice1Of2 value -> value :> Transient.TypeParameter
            | Choice2Of2 value -> value
        let type_ (tracer: SymbolTypeParameterTracer) =
            narrowedFoldedValue tracer
            |> ISymbol.declaredType
            |> TypeTypeParameterTracer.unsafeCreate tracer.program
        let node tracer =
            narrowedFoldedValue tracer 
            |> Symbol.typeParameterDeclarations
            |> _.Value
            |> NodeTypeParameterTracer.create tracer.program
        let nodes tracer =
            narrowedFoldedValue tracer
            |> Symbol.typeParameterDeclarations
            |> NonEmptyArray.map (NodeTypeParameterTracer.create tracer.program)
            |> NonEmptyArray.distinctBy (fun (tracer: NodeTypeParameterTracer) -> tracer.NodeKey.Value)
        let name tracer =
            narrowedFoldedValue tracer
            |> ISymbol.name
    module TypeTypeParameterTracer =
        let tryFromTypeTracer =
            TypeTracer.TypeParameter.tryGet
            >> Option.map (fun t -> t :?> TypeTypeParameterTracer)
        let tryCreate program typ =
            TypeTracer.create program typ
            |> tryFromTypeTracer
        let unsafeCreate program typ =
            tryCreate program typ
            |> Option.defaultWith (fun () -> failwith "Cannot create TypeTypeParameterTracer")
        /// <summary>
        /// Alias for unsafeCreate, but typed with the TypeParameter type. This presumes no 'unsafe'
        /// nature, as we should be assured of the type.
        /// </summary>
        let create program (typ: Ts.TypeParameter) =
            unsafeCreate program typ
        let inline narrowedValue (tracer: TypeTypeParameterTracer) = tracer |> NarrowedTracer.value
        /// <summary>
        /// Retrieves the canonical symbol for the type parameter.
        /// </summary>
        let symbol (tracer: TypeTypeParameterTracer) =
            let typ = narrowedValue tracer
            typ.Value.unsafeGetCanonicalSymbol()
            |> SymbolTypeParameterTracer.unsafeCreate tracer.program
        /// <summary>
        /// Retrieves the canonical type parameter declaration.
        /// </summary>
        let node tracer =
            symbol tracer
            |> SymbolTypeParameterTracer.node
        /// <summary>
        /// Retrieves the constraint type of the type parameter using the type checker.
        /// </summary>
        let constraint tracer =
            narrowedValue tracer
            |> _.Value
            |> _.getConstraint()
            |> Option.map (TypeTracer.create tracer.program)
        /// <summary>
        /// Retrieves the default type of the type parameter using the type checker.
        /// </summary>
        let defaultType tracer =
            narrowedValue tracer
            |> _.Value
            |> _.getDefault()
            |> Option.map (TypeTracer.create tracer.program)
    module NodeTypeParameterTracer =
        /// <summary>
        /// Maps the given node tracer to a NodeTypeParameterTracer if it can be narrowed to a
        /// TypeParameterDeclaration (ie, has been narrowed already, or is a type parameter declaration).
        /// </summary>
        let tryFromNodeTracer =
            NodeTracer.TypeParameter.tryGet
            >> Option.map (fun t -> t :?> NodeTypeParameterTracer)
        /// <summary>
        /// Creates a NodeTypeParameterTracer from a TypeParameterDeclaration
        /// </summary>
        /// <param name="program"></param>
        /// <param name="node"></param>
        let create program (node: Ts.TypeParameterDeclaration) =
            NodeTracer.create program node
            |> NodeTracer.TypeParameter.tryGet
            |> Option.defaultWith(fun () -> failwith "Cannot narrow a type parameter tracer if it doesn't exist")
            :?> NodeTypeParameterTracer
        let inline narrowedValue (tracer: NodeTypeParameterTracer) = tracer |> NarrowedTracer.value 
        /// <summary>
        /// Retrieves the node name.
        /// </summary>
        let name (tracer: NodeTypeParameterTracer) =
            tracer
            |> narrowedValue
            |> _.Value.name.text
        let inline toTypeParameterNode (tracer: NodeTypeParameterTracer) =
            tracer |> narrowedValue |> _.Value
        /// <summary>
        /// Retrieves the constraint type from the type parameter node.
        /// </summary>
        let constraint (tracer: NodeTypeParameterTracer) =
            tracer
            |> narrowedValue |> _.Value.constraint
            |> Option.map (NodeTypeTracer.create tracer.program)
        /// <summary>
        /// Retrieves the default type from the type parameter node.
        /// </summary>
        let defaultType (tracer: NodeTypeParameterTracer) =
            tracer |> narrowedValue |> _.Value.``default``
            |> Option.map (NodeTypeTracer.create tracer.program)
        /// Only a subset of modifiers are expected for type parameters.
        let modifiers (tracer: NodeTypeParameterTracer) =
            tracer |> narrowedValue |> _.Value.modifiers
            |> Option.bind (_.AsArray >> Array.map Node.ModifierKeyword.create >> NonEmptyArray.create)
        /// <summary>
        /// Retrieves the canonical symbol tracer for the type parameter.
        /// </summary>
        let symbol tracer =
            let node = toTypeParameterNode tracer
            node.name
            |> tracer.checker.getSymbolAtLocation
            |> Option.defaultWith (fun () -> failwith "TypeParameter did not have a symbol on its name")
            |> SymbolTypeParameterTracer.unsafeCreate tracer.program
        /// <summary>
        /// Retrieves the CANONICAL type of the type parameter (from the symbol).
        /// </summary>
        let type_ tracer =
            symbol tracer
            |> SymbolTypeParameterTracer.type_
        /// <summary>
        /// This can be a 'Any' type if the type parameter is part of a MappedType or a InferType.
        /// To get the 'type parameter' type, use the 'type_' function instead.
        /// </summary>
        let unsafeType tracer =
            toTypeParameterNode tracer
            |> tracer.checker.getTypeAtLocation
            |> TypeTracer.create tracer.program
        /// <summary>
        /// Retrieves the canonical type parameter tracer from the symbol.
        /// </summary>
        let toCanonicalNode: NodeTypeParameterTracer -> NodeTypeParameterTracer =
            symbol >> SymbolTypeParameterTracer.node
        /// <summary>
        /// Checks whether the canonical type parameter tracer is the same as the given type parameter tracer.
        /// </summary>
        let isCanonicalNode tracer =
            symbol tracer
            |> SymbolTypeParameterTracer.node
            |> _.NodeKey
            |> (=) tracer.NodeKey
        /// <summary>
        /// Retrieves the parent node tracer.
        /// </summary>
        let parent tracer =
            toTypeParameterNode tracer
            |> _.parent
            |> unbox<Ts.Node>
            |> NodeTracer.create tracer.program
    module SymbolClassLikeTracer =
        let inline tryFromSymbolTracer (symbolTracer: SymbolTracer) =
            SymbolTracer.ClassLike.tryGet symbolTracer
            |> Option.map (fun t -> t :?> SymbolClassLikeTracer)
        let inline unsafeFromSymbolTracer (symbolTracer: SymbolTracer) =
            tryFromSymbolTracer symbolTracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a symbol tracer if it doesn't exist")
        let tryCreate program (symbol: Ts.Symbol) =
            SymbolTracer.create program symbol
            |> tryFromSymbolTracer
        let create program symbol =
            SymbolTracer.create program symbol
            |> unsafeFromSymbolTracer
    module NodeParameterTracer =
        let inline tryFromNodeTracer (nodeTracer: NodeTracer) =
            NodeTracer.Parameter.tryGet nodeTracer
            |> Option.map (fun t -> t :?> NodeParameterTracer)
        let inline unsafeFromNodeTracer (nodeTracer: NodeTracer) =
            tryFromNodeTracer nodeTracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a node tracer if it doesn't exist")
        let tryCreate program (node: Ts.Node) =
            NodeTracer.create program node
            |> tryFromNodeTracer
        let create program (node: Ts.ParameterDeclaration) =
            NodeTracer.create program node
            |> unsafeFromNodeTracer
        let unsafeCreate program node =
            NodeTracer.create program node
            |> unsafeFromNodeTracer
        let inline narrowedValue (tracer: NodeParameterTracer) = tracer |> NarrowedTracer.value
        let inline toParameterNode (tracer: NodeParameterTracer) =
            narrowedValue tracer
            |> _.Value
        let symbol tracer =
            let node = toParameterNode tracer
            let backupSymbol = node?symbol : Ts.Symbol
            match node.name with
            | Patterns.Node.BindingNamePatterns.Identifier identifier ->
                tracer.checker.getSymbolAtLocation identifier
                |> Option.defaultValue backupSymbol
            | _ -> backupSymbol
            |> SymbolParameterTracer.unsafeCreate tracer.program
        let typeNode tracer =
            toParameterNode tracer
            |> _.``type``
            |> Option.defaultWith(fun () -> failwith "Parameter did not have a type which goes against the corpus of tests")
            |> NodeTypeTracer.create tracer.program
        let inline type_ tracer =
            typeNode tracer
            |> NodeTypeTracer.type_
        let isRestParameter tracer =
            toParameterNode tracer
            |> _.dotDotDotToken.IsSome
        let nameOrPattern tracer =
            toParameterNode tracer
            |> _.name
            |> function
                | Patterns.Node.BindingNamePatterns.Identifier identifier -> identifier.text |> Choice1Of3
                | Patterns.Node.BindingNamePatterns.ObjectBindingPattern objectBindingPattern -> Choice2Of3 objectBindingPattern
                | Patterns.Node.BindingNamePatterns.ArrayBindingPattern arrayBindingPattern -> Choice3Of3 arrayBindingPattern
        let nameOrNone tracer =
            match nameOrPattern tracer with
            | Choice1Of3 name -> Some name
            | _ -> None
        let nameOrPosition tracer =
            nameOrNone tracer
            |> Option.map Choice1Of2
            |> Option.defaultWith (fun () ->
                symbol tracer
                |> NarrowedTracer.value
                |> ISymbol.name
                |> function
                    | SymbolName.String txt ->
                        txt[2..]
                        |> JS.Constructors.Number.parseFloat
                        |> int
                        |> Choice2Of2
                    | _ -> failwith "Cannot get the position of a parameter if it doesn't have the correct nametype"
                )
        let nameOrSymbolName tracer =
            nameOrPosition tracer
            |> function
                | Choice2Of2 i -> sprintf "__%i" i
                | Choice1Of2 name -> name
        let inline isBindingPattern tracer =
            nameOrPattern tracer |> _.IsChoice1Of3 |> not
        let private parentTracer = NarrowedTracer.createFunctionSet<NarrowedTracer<Node.Kind, _>, NodeTracer, _, _> <| function
            | Kind.DeclarationOrType declOrType ->
                match declOrType with
                | DeclarationKind.Type (Type.Function (node, _)) -> Choice1Of7 node |> Some
                | DeclarationKind.Signature signature -> Choice2Of7 signature |> Some
                | DeclarationKind.Constructor constructor -> Choice3Of7 constructor.Value |> Some
                | DeclarationKind.Function function_ -> Choice4Of7 function_.Value |> Some
                | DeclarationKind.Method method -> Choice5Of7 method |> Some
                | DeclarationKind.GetAccessor getAccessor -> Choice1Of2 getAccessor.Value |> Choice6Of7 |> Some
                | DeclarationKind.SetAccessor setAccessor -> Choice2Of2 setAccessor.Value |> Choice6Of7 |> Some
                | DeclarationKind.Type (Type.Constructor (node, _)) -> Choice7Of7 node |> Some
                | _ -> None
            | _ -> None
        let parent tracer =
            toParameterNode tracer
            |> _.parent
            |> unbox<Ts.Node>
            |> NodeTracer.create tracer.program
            |> parentTracer.tryGet
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a parameter tracer for the parent if it doesn't exist or is unknown")
    module SymbolParameterTracer =
        let inline tryFromSymbolTracer tracer =
            SymbolTracer.Parameter.tryGet tracer
            |> Option.map (fun t -> t :?> SymbolParameterTracer)
        let inline unsafeFromSymbolTracer tracer =
            tryFromSymbolTracer tracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a symbol tracer if it doesn't exist")
        let tryCreate program (symbol: Ts.Symbol) =
            SymbolTracer.create program symbol
            |> tryFromSymbolTracer
        let unsafeCreate program symbol =
            SymbolTracer.create program symbol
            |> unsafeFromSymbolTracer
    
    module SymbolUniqueESSymbolTracer =
        let inline tryFromSymbolTracer tracer =
            SymbolTracer.UniqueESSymbol.tryGet tracer
            |> Option.map (fun t -> t :?> SymbolUniqueESSymbolTracer)
        let inline unsafeFromSymbolTracer tracer =
            tryFromSymbolTracer tracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a symbol tracer if it doesn't exist")
        let tryCreate program (symbol: Ts.Symbol) =
            SymbolTracer.create program symbol
            |> tryFromSymbolTracer
        let unsafeCreate program symbol =
            SymbolTracer.create program symbol
            |> unsafeFromSymbolTracer
        let node (tracer: SymbolUniqueESSymbolTracer) =
            match SymbolTracer.UniqueESSymbol.value tracer with
            | Choice1Of2 symbol ->
                Symbol.canonicalDeclaration symbol
                |> NodeTracer.create tracer.program
                |> NodeUniqueESSymbolTracer.unsafeFromNodeTracer
            | Choice2Of2 symbol ->
                Symbol.canonicalDeclaration symbol
                |> _.Value
                |> NodeTracer.create tracer.program
                |> NodeUniqueESSymbolTracer.unsafeFromNodeTracer
            
    // This should only be created from the UniqueESSymbolType tracer
    module NodeUniqueESSymbolTracer =
        let inline tryFromNodeTracer tracer =
            NodeTracer.UniqueESSymbol.tryGet tracer
            |> Option.map (fun t -> t :?> NodeUniqueESSymbolTracer)
        let inline unsafeFromNodeTracer tracer =
            tryFromNodeTracer tracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a node tracer if it doesn't exist")
        let node tracer =
            NodeTracer.UniqueESSymbol.value tracer
        // let tryCreate program (node: Ts.Node) =
        //     NodeTracer.create program node
        //     |> tryFromNodeTracer
    module TypeUniqueESSymbolTracer =
        let inline tryFromTypeTracer tracer =
            TypeTracer.UniqueESSymbol.tryGet tracer
            |> Option.map (fun t -> t :?> TypeUniqueESSymbolTracer)
        let inline unsafeFromTypeTracer tracer =
            tryFromTypeTracer tracer
            |> Option.defaultWith (fun () -> failwith "Cannot narrow a type tracer if it doesn't exist")
        let tryCreate program (typ: Ts.Type) =
            TypeTracer.create program typ
            |> tryFromTypeTracer
        let create program (typ: Ts.UniqueESSymbolType) =
            TypeTracer.create program typ
            |> unsafeFromTypeTracer
        let unsafeCreate program (typ: Ts.Type) =
            TypeTracer.create program typ
            |> unsafeFromTypeTracer
        let inline narrowedValue (tracer: TypeUniqueESSymbolTracer) = TypeTracer.UniqueESSymbol.value tracer
        let symbol tracer =
            narrowedValue tracer
            |> _.Value.unsafeGetCanonicalSymbol()
            |> SymbolUniqueESSymbolTracer.unsafeCreate tracer.program
        let node tracer =
            symbol tracer
            |> SymbolUniqueESSymbolTracer.node
        
    
    
    module Wrapped =
        module Identifier =
            module Builder =
                type Builder = {
                    Symbol: UniqueIdentifiers.SymbolKey option
                    Node: UniqueIdentifiers.NodeKey option
                    Type: UniqueIdentifiers.TypeKey option
                }
                let private empty = {
                    Symbol = None
                    Node = None
                    Type = None
                }
                let withSymbol symbol builder = { builder with Builder.Symbol = Some symbol }
                let withNode node builder = { builder with Builder.Node = Some node }
                let withType typeKey builder = { builder with Builder.Type = Some typeKey }
                let initWithSymbol symbol = withSymbol symbol empty
                let initWithNode node = withNode node empty
                let initWithType typeKey = withType typeKey empty
                let build builder =
                    if builder = empty then failwith "Cannot build empty Identifier."
                    UniqueIdentifiers.CompositeKey.Create(?symbol = builder.Symbol, ?node = builder.Node, ?typ = builder.Type)
                
            let createSymbol symbol = UniqueIdentifiers.CompositeKey.Create(symbol = symbol)
            let createNode node = UniqueIdentifiers.CompositeKey.Create(node = node)
            let createType typeKey = UniqueIdentifiers.CompositeKey.Create(typ = typeKey)
            let createSymbolAndNode symbol node =
                UniqueIdentifiers.CompositeKey.Create(symbol = symbol, node = node)
            let createSymbolAndType symbol typeKey =
                UniqueIdentifiers.CompositeKey.Create(symbol = symbol, typ = typeKey)
            let createNodeAndType node typeKey =
                UniqueIdentifiers.CompositeKey.Create(node = node, typ = typeKey)
            let createSymbolAndNodeAndType symbol node typeKey =
                UniqueIdentifiers.CompositeKey.Create(symbol = symbol, node = node, typ = typeKey)
            let symbol (identifier: UniqueIdentifiers.CompositeKey) = identifier.symbolKey
            let node (identifier: UniqueIdentifiers.CompositeKey) = identifier.nodeKey
            let type' (identifier: UniqueIdentifiers.CompositeKey) = identifier.typeKey
                
        module PrimitiveKind =
            let fromTypePrimitiveSingleton = function
                | Type.PrimitiveSingleton.Any _ -> Wrapped.PrimitiveKind.Any
                | Type.PrimitiveSingleton.Unknown _ -> Wrapped.PrimitiveKind.Unknown
                | Type.PrimitiveSingleton.Never _ -> Wrapped.PrimitiveKind.Never
                | Type.PrimitiveSingleton.Void _ -> Wrapped.PrimitiveKind.Void
                | Type.PrimitiveSingleton.Undefined _ -> Wrapped.PrimitiveKind.Undefined
                | Type.PrimitiveSingleton.Null _ -> Wrapped.PrimitiveKind.Null
                | Type.PrimitiveSingleton.String _ -> Wrapped.PrimitiveKind.String
                | Type.PrimitiveSingleton.Number _ -> Wrapped.PrimitiveKind.Number
                | Type.PrimitiveSingleton.Boolean _ -> Wrapped.PrimitiveKind.Boolean
                | Type.PrimitiveSingleton.BigInt _ -> Wrapped.PrimitiveKind.BigInt
                | Type.PrimitiveSingleton.ESSymbol _ -> Wrapped.PrimitiveKind.ESSymbol
                | Type.PrimitiveSingleton.NonPrimitive _ -> Wrapped.PrimitiveKind.NonPrimitive
            let fromNodeTypeKeyword = function
                | TypeKeyword.Any -> Wrapped.PrimitiveKind.Any
                | TypeKeyword.String ->  Wrapped.PrimitiveKind.String 
                | TypeKeyword.Number ->  Wrapped.PrimitiveKind.Number 
                | TypeKeyword.Boolean ->  Wrapped.PrimitiveKind.Boolean 
                | TypeKeyword.Null -> Wrapped.PrimitiveKind.Null 
                | TypeKeyword.Undefined ->  Wrapped.PrimitiveKind.Undefined 
                | TypeKeyword.Void -> Wrapped.PrimitiveKind.Void 
                | TypeKeyword.Never ->  Wrapped.PrimitiveKind.Never 
                | TypeKeyword.Unknown ->  Wrapped.PrimitiveKind.Unknown 
                | TypeKeyword.Object ->  Wrapped.PrimitiveKind.NonPrimitive 
                | TypeKeyword.Symbol ->  Wrapped.PrimitiveKind.ESSymbol
                | TypeKeyword.BigInt ->  Wrapped.PrimitiveKind.BigInt 
                | TypeKeyword.Intrinsic ->  Wrapped.PrimitiveKind.Intrinsic
                
            type SRTPHelper =
                static member inline tryCreate(typ: Ts.Type) =
                    Type.PrimitiveSingleton.tryCreate () typ
                    |> Option.map fromTypePrimitiveSingleton
                static member inline tryCreate(node: Ts.Node) =
                    Node.TypeKeyword.tryCreate node
                    |> Option.map fromNodeTypeKeyword
                static member inline tryCreate(typ) = fromTypePrimitiveSingleton typ
                static member inline tryCreate(node) = fromNodeTypeKeyword node
            let inline create input = ((^T or SRTPHelper):(static member tryCreate: ^T -> ^U) input)
                    
        module TypeParameter =
            let private create (nodeTracers: NonEmptyArray<NodeTypeParameterTracer>) (typeTracer: TypeTypeParameterTracer) (symbolTracer: SymbolTypeParameterTracer) =
                let nodeTracer,aliases = NonEmptyArray.popHead nodeTracers
                let symbolId = symbolTracer.SymbolKey
                let nodeId = nodeTracer.NodeKey
                let typeId = typeTracer.TypeKey
                let identifier = Identifier.createSymbolAndNodeAndType symbolId nodeId typeId
                // let name = NodeTypeParameterTracer.name nodeTracer
                // let constraint = NodeTypeParameterTracer.constraint nodeTracer
                // let defaultType = NodeTypeParameterTracer.defaultType nodeTracer
                {
                    Wrapped.TypeParameter.Symbol = symbolTracer
                    Wrapped.TypeParameter.Node = nodeTracer
                    Wrapped.TypeParameter.Type = typeTracer
                    Wrapped.TypeParameter.Key = identifier
                    Wrapped.TypeParameter.AliasNodes = aliases
                }
            let fromNodeTracer (tracer: NodeTypeParameterTracer) =
                let symbol = NodeTypeParameterTracer.symbol tracer
                let type_ = NodeTypeParameterTracer.type_ tracer
                // use the canonical declaration
                let tracer = SymbolTypeParameterTracer.nodes symbol
                create tracer type_ symbol
            let collectTypeParametersForNode (program: Ts.Program) (node: Ts.Node) =
                ts.getEffectiveTypeParameterDeclarations(!!node)
                |> NonEmptyArray.create
                |> Option.map (
                    NonEmptyArray.map (
                        NodeTypeParameterTracer.create program
                        >> fromNodeTracer
                        )
                    )
            let inline collectFor program (input: ^T when ^T:>ICanHaveTypeParameters) =
                input :> ICanHaveTypeParameters
                |> _.TypeParameters
                |> Option.map (NonEmptyArray.map (NodeTypeParameterTracer.create program  >> fromNodeTracer))
            let fromTypeTracer (tracer: TypeTypeParameterTracer) =
                let symbol = TypeTypeParameterTracer.symbol tracer
                let node = SymbolTypeParameterTracer.nodes symbol
                create node tracer symbol
            let fromSymbolTracer symbol =
                let type_ = SymbolTypeParameterTracer.type_ symbol
                let node = SymbolTypeParameterTracer.nodes symbol
                create node type_ symbol
            let symbolTracer (typar: Wrapped.TypeParameter) = typar.Symbol
            let typeTracer (typar: Wrapped.TypeParameter) = typar.Type
            let nodeTracer (typar: Wrapped.TypeParameter) = typar.Node
            let aliasNodeTracers (typar: Wrapped.TypeParameter) = typar.AliasNodes
            let allNodeTracers (typar: Wrapped.TypeParameter) =
                match aliasNodeTracers typar with
                | Some aliases ->
                    aliases
                    |> NonEmptyArray.appendOne (nodeTracer typar)
                | _ -> NonEmptyArray.singleton (nodeTracer typar)
            let key (typar: Wrapped.TypeParameter) = typar.Key
            let symbolKey = key >> _.symbolKey >> Option.get
            let nodeKey = key >> _.nodeKey >> Option.get
            let aliasNodeKeys = aliasNodeTracers >> Option.map (NonEmptyArray.map (fun (tracer: NodeTypeParameterTracer) -> tracer.NodeKey))
            let nodeKeys = allNodeTracers >> NonEmptyArray.map _.NodeKey
            let typeKey = key >> _.typeKey >> Option.get
            let aliasKeys typar =
                aliasNodeKeys typar
                |> Option.map (NonEmptyArray.map (fun (key: NodeKey) ->
                    UniqueIdentifiers.CompositeKey.Create(symbol = symbolKey typar, typ = typeKey typar, node = key))
                )
            let name = symbolTracer >> SymbolTypeParameterTracer.name
            let typeConstraint = typeTracer >> TypeTypeParameterTracer.constraint
            let typeDefault = typeTracer >> TypeTypeParameterTracer.defaultType
            let modifiers = nodeTracer >> NodeTypeParameterTracer.modifiers
            let nonCanonicalDeclarations typar =
                let symbol = symbolTracer typar
                symbol
                |> SymbolTypeParameterTracer.narrowedFoldedValue
                |> ISymbol.chooseDeclarations (Node.DeclarationKind.create symbol.program >> function
                    | Node.DeclarationKind.TypeParameter _ -> None
                    | node -> Some node
                    )
            type SRTPHelper =
                static member inline tryCreate(typar: Ts.Type, program: Ts.Program) =
                    TypeTracer.create program typar
                    |> TypeTypeParameterTracer.tryFromTypeTracer
                    |> Option.map fromTypeTracer
                static member inline tryCreate(node: Ts.Node, program) =
                    NodeTracer.create program node
                    |> NodeTypeParameterTracer.tryFromNodeTracer
                    |> Option.map fromNodeTracer
                static member inline tryCreate(symbol: Ts.Symbol, program) =
                    SymbolTracer.create program symbol
                    |> SymbolTypeParameterTracer.tryFromSymbolTracer
                    |> Option.map fromSymbolTracer
                static member inline tryCreate(node: Ts.TypeParameterDeclaration, program) =
                    NodeTypeParameterTracer.create program node
                    |> fromNodeTracer
                static member inline tryCreate(typar: Ts.TypeParameter, program) =
                    TypeTypeParameterTracer.create program typar
                    |> fromTypeTracer
                static member inline tryCreate(tracer, _) = fromNodeTracer tracer
                static member inline tryCreate(tracer, _) = fromTypeTracer tracer
                static member inline tryCreate(tracer, _) = fromSymbolTracer tracer
                static member inline tryCreate(tracer, _) =
                    TypeTypeParameterTracer.tryFromTypeTracer tracer
                    |> Option.map fromTypeTracer
                static member inline tryCreate(tracer, _) =
                    NodeTypeParameterTracer.tryFromNodeTracer tracer
                    |> Option.map fromNodeTracer
                static member inline tryCreate(tracer, _) =
                    SymbolTypeParameterTracer.tryFromSymbolTracer tracer
                    |> Option.map fromSymbolTracer
            let inline srtpCreate program input =
                ((^T or SRTPHelper):(static member tryCreate: ^T * Ts.Program -> ^U) (input, program))


[<AutoOpen>]
module CoreExtensions =
    type Ts.Symbol with
        member inline this.symbolName = this.escapedName |> SymbolName.Create
    type ISymbol with
        member inline this.program = ISymbol.program this
        member inline this.checker = ISymbol.checker this
        member inline this.toSymbol = ISymbol.toSymbol this
        
    type Type.PrimitiveSingleton with
        static member UnsafeCreate(typ: Ts.Type): Type.PrimitiveSingleton = Type.PrimitiveSingleton.unsafeCreate typ
    type Type.PrimitiveLiteral with
        static member UnsafeCreate(typ) = Type.PrimitiveLiteral.unsafeCreate typ
    type Type.EnumMember with
        static member UnsafeCreate(typ: Ts.Type, program): Type.EnumMember = Type.EnumMember.unsafeCreate program typ
    type Type.Literal with
        static member UnsafeCreate(typ: Ts.Type, program): Type.Literal = Type.Literal.unsafeCreate program typ
    type Type.Primitive with
        static member UnsafeCreate(typ: Ts.Type, program): Type.Primitive = Type.Primitive.unsafeCreate program typ
    type Type.InstantiableNonPrimitive with
        static member UnsafeCreate(typ: Ts.Type): Type.InstantiableNonPrimitive = Type.InstantiableNonPrimitive.unsafeCreate typ
    type Type.StringMapping with
        static member UnsafeCreate(typ: Ts.Type): Type.StringMapping = Type.StringMapping.unsafeCreate typ
    type Type.InstantiablePrimitive with
        static member UnsafeCreate(typ: Ts.Type): Type.InstantiablePrimitive = Type.InstantiablePrimitive.unsafeCreate typ
    type Type.Instantiable with
        static member UnsafeCreate(typ: Ts.Type): Type.Instantiable = Type.Instantiable.unsafeCreate typ
    type Type.Anonymous with
        static member UnsafeCreate(typ: Ts.ObjectType): Type.Anonymous = Type.Anonymous.unsafeCreate typ
    type Type.Mapped with
        static member UnsafeCreate(typ: Ts.ObjectType): Type.Mapped = Type.Mapped.unsafeCreate typ
    type Type.TypeReference with
        static member UnsafeCreate(typ: Ts.TypeReference): Type.TypeReference = Type.TypeReference.unsafeCreate typ
    type Type.Structural with
        static member UnsafeCreate(typ: Ts.ObjectType): Type.Structural = Type.Structural.unsafeCreateObjectType typ
        static member UnsafeCreate(typ: Ts.Type): Type.Structural = Type.Structural.unsafeCreate typ
    type Type.Kind with
        static member UnsafeCreate(typ: Ts.Type, program): Type.Kind = Type.Kind.create program typ
                
    type LocalTableSymbol with
        member inline this.Declarations =
            this.Value.getDeclarations().Value.AsArray
            |> Array.map TopLevelLocalSymbolDeclarations.Create
    type ExportTableSymbol with
        member inline this.Declarations =
            this.Value.getDeclarations().Value.AsArray
            |> Array.map TopLevelExportSymbolDeclarations.Create
            
    type SignatureKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type ParameterKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type PropertyKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type MethodKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type ClassMemberKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type ModuleMemberKind with
        [<Emit "$0.fields[0]">]
        member inline this.Value: Ts.Declaration = jsNative
    type ModuleKind with
        member inline this.Value: Ts.Declaration =
            match this with
            | ModuleKind.Declaration node -> node.Value
            | ModuleKind.Source kind ->
                match kind with
                | SourceKind.Script sf -> sf.Value
                | SourceKind.ExternalModule em -> em.Value
        
    // Transient extensions
    type Transient.Kind with
        /// <summary>Optimised access to the underlying symbol via emitting <c>this.fields[0]</c>.</summary>
        /// <remarks>Compatible with Fable 5</remarks>
        [<Emit "$0.fields[0]">]
        member inline this.Value: ISymbol = jsNative
    type ITransient with
        member this.isTransient = ISymbol.hasFlag Ts.SymbolFlags.Transient this
        member this.hasDeclaration = ISymbol.declarations this |> Option.isSome
    type Transient.IValue with
        member inline this.valueDeclaration: Ts.Declaration option = Symbol.tryValueDeclaration this
    type Transient.IParameter with
        member this.parameterDeclaration: ParameterKind option = Symbol.tryParameterDeclaration this
        member this.parameterDeclarationAndType = Symbol.tryParameterDeclarationAndType this
    type Transient.IVariable with
        member this.variableDeclaration: Ts.VariableDeclaration option = Symbol.tryVariableDeclaration this
        member this.variableDeclarationAndType = Symbol.tryVariableDeclarationAndType this
    type Transient.IProperty with
        member this.propertyDeclarations: NonEmptyArray<PropertyKind> option = Symbol.tryPropertyDeclarations this
        member this.propertyDeclarationAndTypes = Symbol.tryPropertyDeclarationsAndTypes this
    type Transient.IEnumMember with
        member this.enumMemberDeclaration = Symbol.tryEnumMemberDeclaration this
        member this.enumMemberDeclarationAndType = Symbol.tryEnumMemberDeclarationAndType this
    type Transient.IFunction with
        member this.functionDeclarations: NonEmptyArray<Ts.FunctionDeclaration> option = Symbol.tryFunctionDeclarations this
        member this.functionDeclarationAndTypes = Symbol.tryFunctionDeclarationAndTypes this
    type Transient.IClass with
        member this.classDeclaration: Ts.ClassDeclaration option = Symbol.tryClassDeclaration this
        member this.classDeclarationAndType = Symbol.tryClassDeclarationAndType this
    type Transient.IMethod with
        member this.methodDeclarations: NonEmptyArray<MethodKind> option = Symbol.tryMethodDeclarations this
        member this.methodDeclarationAndTypes = Symbol.tryMethodDeclarationsAndTypes this
    type Transient.IConstructor with
        member this.constructorDeclaration: Ts.ConstructorDeclaration option = Symbol.tryConstructorDeclaration this
        member this.constructorDeclarationAndType = Symbol.tryConstructorDeclarationAndType this
    type Transient.ISignature with
        member this.signatureDeclarations: SignatureKind option = Symbol.trySignatureDeclarations this
        member this.signatureDeclarationAndType = Symbol.trySignatureDeclarationsAndTypes this
    type Transient.IEnum with
        member this.enumDeclarations: NonEmptyArray<Ts.EnumDeclaration> option = Symbol.tryEnumDeclaration this
        member this.enumDeclarationAndTypes = Symbol.tryEnumDeclarationAndType this
    type Transient.INamespace with
        member this.namespaceDeclarations: NonEmptyArray<Ts.ModuleDeclaration> option = Symbol.tryNamespaceDeclarations this
        member this.namespaceDeclarationAndTypes = Symbol.tryNamespaceDeclarationsAndTypes this
    type Transient.IValueModule with
        member this.moduleDeclarations: NonEmptyArray<ModuleKind> option = Symbol.tryModuleDeclarations this
        member this.moduleDeclarationAndTypes = Symbol.tryModuleDeclarationsAndTypes this
    type Transient.ITypeParameter with
        member this.typeParameterDeclarations: NonEmptyArray<Ts.TypeParameterDeclaration> option = Symbol.tryTypeParameterDeclarations this
        member this.typeParameterDeclarationAndTypes = Symbol.tryTypeParameterDeclarationsAndTypes this
    type Transient.ITypeAlias with
        member this.typeAliasDeclaration: Ts.TypeAliasDeclaration option = Symbol.tryTypeAliasDeclaration this
        member this.typeAliasDeclarationAndType = Symbol.tryTypeAliasDeclarationAndType this
    type Transient.IInterface with
        member this.interfaceDeclarations: NonEmptyArray<Ts.InterfaceDeclaration> option = Symbol.tryInterfaceDeclarations this
        member this.interfaceDeclarationAndTypes = Symbol.tryInterfaceDeclarationsAndTypes this
    type Transient.IGetAccessor with
        member this.getAccessorDeclaration: Ts.GetAccessorDeclaration option = Symbol.tryGetAccessorDeclaration this
        member this.getAccessorDeclarationAndType = Symbol.tryGetAccessorDeclarationAndType this
    type Transient.ISetAccessor with
        member this.setAccessorDeclaration: Ts.SetAccessorDeclaration option = Symbol.trySetAccessorDeclaration this
        member this.setAccessorDeclarationAndType = Symbol.trySetAccessorDeclarationAndType this
    type Transient.IAccessor with
        member this.accessorDeclarations =
            ISymbol.tryPickDeclaration Patterns.Node.(|GetAccessorDeclaration|_|) this,
            ISymbol.tryPickDeclaration Patterns.Node.(|SetAccessorDeclaration|_|) this
        member this.accessorDeclarationAndTypes =
            this :?> Transient.IGetAccessor |> _.getAccessorDeclarationAndType,
            this :?> Transient.ISetAccessor |> _.setAccessorDeclarationAndType
    type Transient.IClassMember with
        member this.classMemberDeclarations: NonEmptyArray<ClassMemberKind> option = Symbol.tryClassMemberDeclarations this
        member this.classMemberDeclarationAndTypes = Symbol.tryClassMemberDeclarationsAndTypes this
            
    // ----------------------
    type IOptional with
        member inline this.isOptional = this |> ISymbol.hasFlag Ts.SymbolFlags.Optional
    // Ensure the overloads for the declaration of the canonical kind does not go onto
    // the inherited concrete I___ interface kind, as that is not the correct intention.
    // Because we can provide more guarantees with the 'concrete'/'canonical' symbol kinds,
    // we overload many of the transient members which provide generic options with non-option
    // return values, and concrete types.
    type Transient.Parameter with
        member this.parameterDeclaration = Symbol.parameterDeclarations this
        member this.parameterDeclarationAndType = Symbol.parameterDeclarationAndType this
        member inline this.canonical = this.parameterDeclaration
        member inline this.canonicalWithType = this.parameterDeclarationAndType
    type Parameter with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.Method with
        member this.methodDeclarations = Symbol.methodDeclarations this
        member this.methodDeclarationAndTypes = Symbol.methodDeclarationsAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Method with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.Property with
        member this.propertyDeclarations = Symbol.propertyDeclarations this
        member this.propertyDeclarationAndTypes = Symbol.propertyDeclarationsAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Property with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.TypeAlias with
        member this.typeAliasDeclaration = Symbol.typeAliasDeclarations this
        member this.typeAliasDeclarationAndType = Symbol.typeAliasDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.Interface with
        member this.interfaceDeclarations = Symbol.interfaceDeclarations this
        member this.interfaceDeclarationAndTypes = Symbol.interfaceDeclarationsAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.Class with
        member this.classDeclaration = Symbol.classDeclaration this
        member this.classDeclarationAndType = Symbol.classDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Class with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.Constructor with
        member this.constructorDeclaration = Symbol.constructorDeclaration this
        member this.constructorDeclarationAndType = Symbol.constructorDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.Signature with
        member this.signatureDeclarations = Symbol.signatureDeclarations this
        member this.signatureDeclarationAndType = Symbol.signatureDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.Variable with
        member this.variableDeclaration = Symbol.variableDeclaration this
        member this.variableDeclarationAndType = Symbol.variableDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Symbol.Variable with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.EnumMember with
        member this.enumMemberDeclaration = Symbol.enumMemberDeclaration this
        member this.enumMemberDeclarationAndType = Symbol.enumMemberDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Symbol.EnumMember with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.Function with
        member this.functionDeclarations = Symbol.functionDeclarations this
        member this.functionDeclarationAndTypes = Symbol.functionDeclarationAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Function with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.ConstEnum with
        member this.enumDeclarations = Symbol.constEnumDeclarations this
        member this.enumDeclarationAndTypes = Symbol.constEnumDeclarationAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type ConstEnum with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.TypeEnum with
        member this.enumDeclarations = Symbol.typeEnumDeclarations this 
        member this.enumDeclarationAndTypes = Symbol.typeEnumDeclarationAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.ValueModule with
        member this.moduleDeclarations = Symbol.moduleDeclarations this
        member this.moduleDeclarationAndTypes = Symbol.moduleDeclarationAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type ValueModule with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.NamespaceModule with
        member this.namespaceDeclarations = Symbol.namespaceDeclarations this
        member this.namespaceDeclarationAndTypes = Symbol.namespaceDeclarationsAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type Transient.GetAccessor with
        member this.getAccessorDeclaration = Symbol.getAccessorDeclaration this
        member this.getAccessorDeclarationAndType = Symbol.getAccessorDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type GetAccessor with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.SetAccessor with
        member this.setAccessorDeclaration = Symbol.setAccessorDeclaration this
        member this.setAccessorDeclarationAndType = Symbol.setAccessorDeclarationAndType this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type SetAccessor with
        member this.valueDeclaration = Symbol.valueDeclaration this
    type Transient.TypeParameter with
        member this.typeParameterDeclarations = Symbol.typeParameterDeclarations this 
        member this.typeParameterDeclarationAndTypes = Symbol.typeParameterDeclarationsAndTypes this
        member inline this.canonical = Symbol.canonicalDeclaration this
        member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
    type NodeDeclarationKindTracer with
        member inline this.SubValue =
            match this.Value with
            | Node.Kind.DeclarationOrType kind -> kind
            | _ -> failwith "Expected DeclarationOrType"
module Patterns =
    module NodeTracer =
        let (|DeclarationKind|_|) (tracer: NodeTracer) =
            match tracer.Value with
            | Node.Kind.DeclarationOrType _ -> DeclarationKind (unbox<NodeDeclarationKindTracer> tracer |> Some)
            | _ -> None
            