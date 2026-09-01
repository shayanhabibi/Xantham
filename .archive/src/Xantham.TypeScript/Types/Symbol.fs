namespace Xantham.TypeScript

open Fable.Core
open TypeScript
open Xantham.Fable
open Fable.Core.JsInterop

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
    type LocalTableSymbol = LocalTableSymbol of Ts.Symbol interface IErasedWrapper<Ts.Symbol>
    [<Erase>]
    type ExportTableSymbol = ExportTableSymbol of Ts.Symbol interface IErasedWrapper<Ts.Symbol>

[<AutoOpen>]
module Maps =
    type SymbolTable = Map<SymbolName, Ts.Symbol>
    type LocalSymbolTable = Map<SymbolName, Symbol.LocalTableSymbol>
    type ExportSymbolTable = Map<SymbolName, Symbol.ExportTableSymbol>