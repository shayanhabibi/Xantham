---
title: Guarded Properties
category: Xantham.Fable
categoryindex: 1
index: 5
---

# Guarded Properties Architecture in Xantham.Fable

The guarded properties architecture in Xantham.Fable provides a sophisticated pattern for managing type-safe access to data associated with AST nodes. This system is fundamental to how Xantham.Fable manages information flow throughout the TypeScript processing pipeline.

## Overview

Guarded properties provide a type-safe, named access pattern for associating data with XanthamTag objects. They offer a clean separation between the core tag system and the specialized data management for different parts of the AST processing pipeline.

## Core Abstractions

### KeyedSymbolSlot<'T>
```fsharp
type KeyedSymbolSlot<'T> =
    abstract member get: XanthamTag -> 'T
    abstract member has: XanthamTag -> bool
    abstract member set: 'T -> XanthamTag -> XanthamTag
    abstract member clear: XanthamTag -> XanthamTag
    abstract member getOrSetWith: (unit -> 'T) -> XanthamTag -> 'T
    abstract member getOrMapSet: (XanTagKind -> 'T) -> XanthamTag -> 'T
```

### SymbolSlot<'T>
```fsharp
type SymbolSlot<'T> =
    inherit KeyedSymbolSlot<'T>
    abstract Keyed: KeyedSymbolSlot<'T>
```

### Pending Types
```fsharp
type KeyedPendingSymbolSlot<'T> =
    inherit KeyedSymbolSlotWithDefault<PendingSignal<'T>>

type PendingSymbolSlot<'T> =
    inherit SymbolSlotWithDefault<PendingSignal<'T>>
```

<div class="mermaid">
flowchart TD
    subgraph Guarded_Properties_System
        tag[XanthamTag] --> slots[Guarded Properties]
        slots --> slot1[SummaryContent]
        slots --> slot2[Documentation] 
        slots --> slot3[ParameterBuilder]
        slots --> slot4[TypeSignal]
        slots --> slot5[MemberBuilder]
        slots --> slot6[ExportBuilder]
    end
    subgraph Access_Patterns
        get[Get Access] --> slot1
        get --> slot2
        get --> slot3
        get --> slot4
        get --> slot5
        get --> slot6
        set[Set Access] --> slot1
        set --> slot2  
        set --> slot3
        set --> slot4
        set --> slot5
        set --> slot6
        clear[Clear Operations] --> slot1
        clear --> slot2
        clear --> slot3
        clear --> slot4
        clear --> slot5
        clear --> slot6
    end
    subgraph Internal_System
        symbol[Symbol Key] --> slots
        tags[Tag Storage] --> slots
        cache[Cache System] --> slots
        pending[Pending Signals] --> slots
    end
    tag --> internal[Internal Processing]
    internal --> get
    get --> results[Results]
    results --> cache
    cache --> set
    set --> tag
</div>

## Implementation Details

### Helpers Module
The `Helpers` module provides the core implementation functions:

```fsharp
let inline makeSlotWithDefault<'T> thunk name =
    // Creates a slot with default value initialization
    let symbol = SymbolTypeKey.create<'T> name
    { new SymbolSlotWithDefault<'T> with
        member _.get tag = tag.Get(symbol)
        member _.has tag = tag.Has(symbol)
        member _.set value tag = tag.Set(symbol, value); tag
        member _.clear tag = tag.Clear(symbol); tag
        member _.getOrSetWith thunk tag = tag.GetOrInit(symbol, thunk)
        member _.getOrMapSet thunk tag = tag.GetOrInit(symbol, fun () -> thunk tag.Value)
        member _.getOrSetDefault tag = tag.GetOrInit(symbol, thunk)
        member _.Keyed = {
            // Keyed implementations
        }
    }

let inline makeSlot<'T> name =
    makeSlotWithDefault<'T> (fun () -> Fable.Core.JS.undefined) name :> SymbolSlot<'T>

let inline makePendingSlot<'T> name =
    makeSlotWithDefault<PendingSignal<'T>> (fun () -> Signal.pending()) name :?> PendingSymbolSlot<'T>
```

## Predefined Slots

Xantham.Fable defines several specialized guarded properties slots:

```fsharp
let SummaryContent = Helpers.makeSlot<TsComment> "SummaryContent"
let Documentation = Helpers.makeSlot<TsComment array> "Documentation"
let ParameterBuilder = Helpers.makePendingSlot<SParameterBuilder> "ParameterBuilder"
let ConstructorBuilder = Helpers.makePendingSlot<SConstructorBuilder> "ConstructorBuilder"
let MemberBuilder = Helpers.makePendingSlot<SMemberBuilder> "MemberBuilder"
let AstNodeBuilder = Helpers.makePendingSlot<SType> "AstNodeBuilder"
let TypeSignal = Helpers.makeSlotWithDefault<TypeSignal> (fun () -> TypeSignal.pending()) "TypeSignal"
let Source = Helpers.makeSlot<Signal<ModuleName>> "Source"
let ExportBuilder = Helpers.makePendingSlot<STsExportDeclaration> "ExportBuilder"
```

## Usage Patterns

### Setting Data
```fsharp
// Set a value using the slot
let setSummaryContent (tag: XanthamTag) (content: TsComment) =
    SummaryContent.set content tag

// Get a value
let getDocumentation (tag: XanthamTag) : TsComment array option =
    if Documentation.has tag then
        Some (Documentation.get tag)
    else
        None
```

### Lazy Initialization
```fsharp
// Get or initialize with a thunk
let getOrSetParameterBuilder (tag: XanthamTag) (thunk: unit -> SParameterBuilder) : SParameterBuilder =
    ParameterBuilder.getOrSetWith thunk tag
```

### Pending Signal Usage
```fsharp
// Create a pending signal
let pendingSignal = Signal.pending<SParameterBuilder>()
// Later fulfill it
Signal.fill myBuilder pendingSignal
```

## Integration with XanthamTag System

### Tag Data Management
The guarded properties work seamlessly with XanthamTag's underlying data store:

```fsharp
// Keyed storage pattern
member _.Keyed = {
    new KeyedSymbolSlotWithDefault<'T> with
        member _.get tag = tag.KeyedGet(symbol)
        member _.has tag = tag.KeyedHas(symbol)
        member _.set value tag = tag.KeyedSet(symbol, value); tag
        member _.clear tag = tag.KeyedClear(symbol); tag
        member _.getOrSetWith thunk tag = tag.KeyedGetOrInit(symbol, thunk)
        member _.getOrMapSet thunk tag = tag.KeyedGetOrInit(symbol, fun () -> thunk tag.Value)
        member _.getOrSetDefault tag = tag.KeyedGetOrInit(symbol, thunk)
}
```

## Benefits of This Architecture

### 1. Type Safety
- Compile-time guarantees that data access is properly typed
- Prevents accidental misuse of data slots
- Clear separation between different data types stored on tags

### 2. Clean Interface
- Named accessors provide intuitive API
- Clear semantics about when data is accessed vs modified
- Separation of concerns between raw tag operations and convenience methods

### 3. Memory Management
- Automatic cleanup and clearing of unused data
- Efficient storage through symbol-based keys
- Support for both regular and pending data slots

### 4. Extensibility
- Easy to add new data slots without modifying core tag system
- Consistent patterns across all data access
- Flexible initialization and default value handling

## In The Processing Pipeline

The guarded properties play a crucial role in the processing pipeline:

1. **Type Construction**: `TypeSignal` tracks the computed type for each node
2. **Documentation**: `Documentation` stores JSDoc comments associated with declarations  
3. **Builders**: `ParameterBuilder`, `MemberBuilder` hold intermediate processing state
4. **AstNode**: `AstNodeBuilder` links processed F# constructs back to original AST nodes
5. **Exports**: `ExportBuilder` manages the export declaration construction

## Practical Example

Here's how guarded properties are used in the actual processing:

```fsharp
// In MemberDeclaration.fs
let processMember (ctx: TypeScriptReader) (tag: XanthamTag) (memberDeclaration: MemberDeclaration) =
    // Get or create the member builder
    let builder = MemberBuilder.getOrSetWith (fun () -> SMemberBuilder()) tag
    
    // Set information on the builder
    builder.MemberName <- memberDeclaration.Name
    builder.IsStatic <- memberDeclaration.IsStatic
    
    // Store the builder back on the tag
    MemberBuilder.set builder tag
```

This architecture provides Xantham.Fable with a robust, extensible system for managing the complex data relationships that naturally arise in TypeScript AST processing, ensuring both performance and type safety throughout the extraction process.