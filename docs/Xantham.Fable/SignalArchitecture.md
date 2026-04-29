---
title: Signals
category: Xantham.Fable
index: 4
---

# Signal Architecture in Xantham.Fable

The signal architecture in Xantham.Fable is a core component that enables reactive data flow throughout the TypeScript AST processing pipeline. This architecture is crucial for managing the complex dependencies and computations that occur during type extraction and transformation.

## Overview

Xantham.Fable uses a sophisticated signal-based reactive system that provides automatic dependency tracking, lazy evaluation, and efficient invalidation propagation. The system is designed to handle the complex data dependencies that naturally arise when processing large TypeScript ASTs.

## Signal Type Definition

The core `Signal<'a>` type is defined in `Types/Signal.fs` and provides:

<div class="mermaid">
flowchart TD
    subgraph Signal_System
        signal[Signal<'a>] --> source[Source Signal]
        signal --> computed[Computed Signal]
        signal --> auto[Auto Signal]
    end
    subgraph Signal_Operations
        source --> set[Set Value]
        source --> get[Get Value]
        computed --> deps[Dependency Tracking]
        computed --> recalc[Recompute]
        auto --> track[Automatic Tracking]
        auto --> invalidation[Invalidation]
        get --> cache[Value Caching]
    end
    subgraph Dependency_Management
        deps --> collector[Dependency Collector]
        collector --> deps2[Dependency Discovery]
        deps2 --> invalidation
        invalidation --> propagation[Propagation]
    end
    subgraph Reactive_Flow
        input[Input Data] --> source
        source --> cache
        cache --> computed
        computed --> output[Output Data]
    end
</div>

### Key Features

1. **Dual Nature**:
   - Source signals: hold plain mutable values, updated imperatively  
   - Computed signals: values derived from other signals via thunks

2. **Dependency Management**:
   - Explicit dependency tracking (computed signals)
   - Automatic dependency tracking (auto signals)
   - Transitive dirty propagation

3. **Lazy Evaluation**:
   - Computed values are cached and only re-computed when needed
   - Dirty state determines when recomputation is required

## Signal Constructors

### Source Signals
```fsharp
static member Source(value: 'a) : Signal<'a>
```
Creates a source signal with an initial value. These are the "roots" of the reactive system.

### Computed Signals
```fsharp
static member Computed(compute: unit -> 'a, deps: IEvent<unit> list) : Signal<'a>
```
Creates a computed signal that re-evaluates when any provided dependencies fire.

### Auto Signals
```fsharp
static member Auto(thunk: unit -> 'a) : Signal<'a>
```
Creates a computed signal with automatic dependency discovery through a tracking scope.

## Core Operations

### Value Access
```fsharp
member this.Value : 'a
```
Returns the current value, re-computing and caching it if dirty. This operation also registers dependencies in auto-mode.

### Setting Values  
```fsharp
member _.Set(v: 'a)
```
Sets the value of a source signal and fires invalidation events.

### Fulfillment
```fsharp
member this.FulfillWith(thunk: unit -> 'a) : unit
```
Retrofits a source signal with a reactive thunk, converting it into a self-updating signal.

## The Tracking Scope System

The key to automatic dependency tracking is the module-level `collector` that allows signals to register as dependencies:

```fsharp
let mutable internal collector: (IEvent<unit> -> unit) option = None
```

### Auto Dependency Tracking Workflow

1. **Prepare scope**: `collector` is set to a tracking function
2. **Execute thunk**: Thunk runs within the tracking scope
3. **Collect dependencies**: Any `.Value` access is recorded
4. **Restore scope**: Original collector value is restored
5. **Create signal**: Computed signal created with discovered dependencies

## Practical Usage in Xantham

### In Type Processing
The signal system is extensively used during type declaration processing where complex type relationships need to be tracked:

```fsharp
// Example from Read.fs
let rec healthCheckType (typKey: TypeKey) (node: TsType) =
    // Various type checking logic that might access other signals
    match node with
    | TsType.TypeReference tsTypeReference ->
        if typKey = tsTypeReference.Type then Log.healthCheckError typKey tsTypeReference tsTypeReference
```

### In Duplicate Handling
Signals are used to manage and resolve type duplicates during processing:

```fsharp
let private finaliseAssembly (results: IRResult<'T> array) =
    // Uses signal-based tracking for duplicate handling
    let duplicates, nonDuplicates =
        results
        |> Array.groupBy _.Key
        |> Array.map (fun (key, values) -> key, values |> Array.distinctBy _.Node)
        |> Array.partition (snd >> _.Length >> (<) 1)
```

## Benefits of This Approach

### 1. Efficient Memory Management
- Only recomputes values when dependencies change
- Automatic garbage collection of unused signal dependencies
- Lazy evaluation prevents unnecessary computations

### 2. Clean Separation of Concerns
- Signal management is separated from node processing logic
- Type safety is maintained through the typed signal system
- Easy to test isolated components

### 3. Complex Dependency Resolution
- Handles recursive type references gracefully
- Manages cross-referenced types without circular dependencies
- Provides clear invalidation semantics for change propagation

### 4. Extensibility
- Easy to add new signal types and operations
- Composable transformations (`map`, `map2`)
- Supports both manual (explicit) and automatic (implicit) dependency tracking

## Advanced Patterns

### Pending Signals
```fsharp
let pending<'a>(): Signal<'a voption>
```
Used for signals that are initialized with `ValueNone` and later filled with actual values.

### Signal Effects
```fsharp
let effect (action: unit -> unit) (deps: IEvent<unit> list) : System.IDisposable
```
Allows running side-effect operations when signals change, useful for logging or debugging.

### Signal Mapping
```fsharp
let map (f: 'a -> 'b) (s: Signal<'a>) : Signal<'b>
let map2 (f: 'a -> 'b -> 'c) (a: Signal<'a>) (b: Signal<'b>) : Signal<'c>
```
Transformation functions that create new computed signals from existing ones.

## Integration with AST Processing

The signal architecture integrates seamlessly with the AST processing pipeline:

1. **Tag creation**: Each AST node gets associated with a XanthamTag that can hold signal data
2. **Type evaluation**: Complex type resolution happens through computed signals
3. **Cross-referencing**: Type dependencies are tracked through signal relationships
4. **Duplication handling**: Duplicate type resolution uses signal dependency tracking
5. **Final output**: All processed types are assembled through signal-computed values

This signal architecture provides the foundation for Xantham.Fable's robust handling of complex TypeScript constructs and ensures that type relationships are correctly managed even in large, complex codebases.