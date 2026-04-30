---
title: Architecture
category: Xantham.Fable
categoryindex: 1
index: 2
---

# Architecture of Xantham.Fable

The architecture of Xantham.Fable is designed to transform TypeScript `.d.ts` files into a standardized JSON schema that can be consumed by downstream F# generators. This multi-layered architecture is built for scalability, maintainability, and robustness.

## System Overview

Xantham.Fable operates as a TypeScript AST crawler and converter that processes complex TypeScript definitions into a common schema format. The architecture can be visualized as:

<div class="mermaid">
flowchart LR
    input[Input .d.ts files] --> tsc[TypeScript Compiler API]
    tsc --> stack[Stack-based AST traversal]
    stack --> signals[Reactive signal processing]
    signals --> output[Common Schema JSON Output]
</div>

## Core Architecture Layers

### 1. Input Layer
- **File Processing**: Reads input TypeScript files (.d.ts)
- **Module Resolution**: Handles complex module imports and exports
- **Compiler Setup**: Initializes TypeScript program and type checker

### 2. AST Processing Layer  
- **Stack-based Traversal**: Safe, memory-efficient AST walking
- **Type System**: Maps TypeScript constructs to internal representations
- **Signal Management**: Reactive computation and data flow

### 3. Data Flow Layer
- **Guarded Properties**: Type-safe data access patterns
- **Signal Architecture**: Reactive dependency tracking
- **Caching**: Optimized repeated processing

### 4. Output Layer
- **Schema Encoding**: Converts internal data to JSON format
- **Common Schema**: Standard representation for downstream consumers
- **Serialization**: Efficient JSON generation with Thoth.Json

<div class="mermaid">
flowchart TB
    subgraph Input
        input1[File Processing]
        input2[Module Resolution]
        input3[Compiler Setup]
    end
    subgraph AST_Processing
        ast1[Stack-based Traversal]
        ast2[Type System]
        ast3[Signal Management]
    end
    subgraph Data_Flow
        data1[Guarded Properties]
        data2[Signal Architecture]
        data3[Caching]
    end
    subgraph Output
        out1[Schema Encoding]
        out2[Common Schema]
        out3[Serialization]
    end
    input1 --> ast1
    input2 --> ast1
    input3 --> ast1
    ast1 --> data1
    ast2 --> data2
    ast3 --> data3
    data1 --> out1
    data2 --> out2
    data3 --> out3
    out1 --> out3
</div>

## Detailed Architecture Components

### TypeScript Reader and Program Management

The core infrastructure for reading and analyzing TypeScript files:

```fsharp
type TypeScriptReader = {
    Stack: Stack<XanthamTag>                // Processing stack
    EntryFiles: string array                // Input file paths
    Program: Ts.Program                     // TypeScript compilation program
    Checker: Ts.TypeChecker                 // Type checker for resolution
    Warnings: ResizeArray<string>           // Processing diagnostics
    ModuleMap: ModuleMap                   // Module resolution mapping
    SignalCache: Dictionary<IdentityKey, TypeStore>  // Type processing cache
    ExportCache: Dictionary<IdentityKey, ExportStore> // Export processing cache
    MemberCache: Dictionary<IdentityKey, MemberStore> // Member processing cache
    LibCache: HashSet<IdentityKey>         // Library type cache
}
```

<div class="mermaid">
flowchart LR
    subgraph TypeScript_Reader
        stack[XanthamTag Stack] --> program[TypeScript Program]
        program --> checker[TypeChecker]
        checker --> modulemap[ModuleMap]
        modulemap --> caches[Caches]
        caches --> signalcache[SignalCache]
        caches --> exportcache[ExportCache]
        caches --> membercache[MemberCache]
        caches --> libcache[LibCache]
    end
    subgraph Processing_Loop
        program --> tags[Tag Creation]
        tags --> dispatch[Dispatcher]
        dispatch --> processors[Node Processors]
        processors --> results[Processing Results]
        results --> caches
    end
</div>

### XanthamTag System

The fundamental unit of data association in the AST processing:

```fsharp
// XanthamTag is a guarded tracer attached to the underlying TS object via
// the well-known TRACER_TAG / TRACER_GUARD JS symbol keys. It carries the
// XanTagKind classification, an identity Guard, and two property bags
// (a tag-bag stored on the tag and a keyed-bag stored on the guard).
type XanthamTag =
    inherit GuardedTracer<XanTagKind, GuardTracer>
    member Value       : XanTagKind   // discriminated kind
    member Guard       : GuardTracer  // identity guard
    member IdentityKey : IdentityKey  // = this.Guard.Value
    member Debug       : bool with get, set
    member DebugId     : int

type XanTagKind =
    | Ignore
    | MemberDeclaration  of MemberDeclaration
    | TypeDeclaration    of TypeDeclaration
    | TypeNode           of TypeNode
    | JSDocTag           of JSDocTags
    | Type               of TypeFlagPrimary
    | ModulesAndExports  of ModulesAndExports
    | LiteralTokenNode   of LiteralTokenNode
```

Tags are obtained through the SRTP factory `XanthamTag.Create(node, checker)`,
which dispatches over `Ts.Node`, `Ts.Type`, and `Ts.Symbol`. The factory
returns a pair of `TagState` values describing whether the tag container and
its guard already existed on the underlying object &mdash; this is how
cycle-detection and idempotent re-entry are implemented (see also
[Debugging](Debugging.html) for the role of `Debug` and `DebugId`).

<div class="mermaid">
flowchart TD
    subgraph XanthamTag_System
        tag[XanthamTag]
        tag --> value[Value: XanTagKind]
        tag --> identity[IdentityKey]
        value --> kind1[MemberDeclaration]
        value --> kind2[TypeDeclaration]
        value --> kind3[TypeNode]
        value --> kind4[JSDocTag]
        value --> kind5[Type]
        value --> kind6[ModulesAndExports]
        value --> kind7[LiteralTokenNode]
    end
    subgraph Data_Association
        tag --> data[Associated Data]
        data --> member[Member Data]
        data --> type[Type Data]
        data --> node[Node Data]
        data --> doc[JSDoc Data]
        data --> typeflag[Type Flag Data]
        data --> module[Module Data]
        data --> literal[Literal Data]
    end
</div>

### Stack-based Processing Architecture

A critical component that prevents JavaScript stack overflows:

<div class="mermaid">
flowchart TD
    subgraph Stack_Processing
        stack[Processing Stack] --> tag1[XanthamTag]
        stack --> tag2[XanthamTag]  
        stack --> tag3[XanthamTag]
        tag1 --> process1[Processing]
        tag2 --> process2[Processing]
        tag3 --> process3[Processing]
    end
    subgraph Processing_Control
        dispatch[Dispatcher] --> processor1[Handler]
        processor1 --> results[Results]
        results --> cache[Caches]
        cache --> stack2[Update Stack]
        stack2 --> process1
    end
    subgraph Dependency_Management
        dependencies[Dependencies] --> signals[Signal System]
        signals --> processing_loop[Processing Loop]
        processing_loop --> stack3[Stack Updates]
        stack3 --> stack
    end
</div>

### Signal Architecture Integration

Core reactive data flow system:

<div class="mermaid">
flowchart LR
    subgraph Signal_System
        source[Source Signals] --> computed[Computed Signals]
        computed --> invalidation[Invalidation Events]
    end
    subgraph Dependency_Tracking
        collector[Dependency Collector] --> computed
        invalidation --> recalc[Recalculation]
        recalc --> cache2[Signal Cache]
        cache2 --> computed
    end
    subgraph Data_Flow
        source --> processing[Processing]
        processing --> results[Results]
        results --> signals
    end
</div>

### Guarded Properties Pattern

Type-safe data access with separation of concerns:

<div class="mermaid">
flowchart LR
    subgraph Guarded_Properties
        tag[XanthamTag] --> slots[Guarded Properties]
        slots --> slot1[Summary Content]
        slots --> slot2[Documentation]
        slots --> slot3[Parameter Builder]
        slots --> slot4[Type Signal]
        slots --> slot5[Member Builder]
    end
    subgraph Access_Patterns
        get[Get Access] --> slot1
        get --> slot2
        get --> slot3
        get --> slot4
        get --> slot5
        set[Set Access] --> slot1
        set --> slot2  
        set --> slot3
        set --> slot4
        set --> slot5
        clear[Clear Operations] --> slot1
        clear --> slot2
        clear --> slot3
        clear --> slot4
        clear --> slot5
    end
    subgraph Data_Flow
        tag --> process[Processing]
        process --> get
        get --> results[Results]
        results --> cache[Signal Cache]
        cache --> set
        set --> tag
    end
</div>

### Signal Architecture Integration

Core reactive data flow system:

```text
┌───────────────────────────────────────────────────────────────┐
│                     Signal System                             │
├─────────────────┬─────────────────┬───────────────────────────┤
│   Signals       │   Dependencies  │   Computation             │
├─────────────────┼─────────────────┼───────────────────────────┤
│   Source        │   Automatic     │   Lazy Evaluation         │
│   Signals       │   Tracking      │   Dirty Propagation       │
│                 │   (Collector)   │   Caching                 │
├─────────────────┼─────────────────┼───────────────────────────┤
│   Computed      │   Explicit      │   Transformation          │
│   Signals       │   Dependencies  │   Effects                 │
│                 │   (Tracked)     │   Fulfillment             │
└─────────────────┴─────────────────┴───────────────────────────┘
```

### Guarded Properties Pattern

Type-safe data access with separation of concerns:

```text
┌───────────────────────────────────────────────────────────────┐
│                  Guarded Properties                           │
├─────────────────┬─────────────────┬───────────────────────────┤
│   Named         │  Module-level   │   Keyed vs Non-Keyped     │
│   Accessors     │  Slot System    │   Storage                 │
├─────────────────┼─────────────────┼───────────────────────────┤
│   Summary       │   SymbolSlot    │   Tag Bag                 │
│   Content       │   Pending       │   Keyed Bag               │
│   Documentation │   SymbolSlot    │   Caching                 │
│                 │   WithDefault   │   Lazy Init               │
└─────────────────┴─────────────────┴───────────────────────────┘
```

## Data Flow Through the System

### 1. Input Processing
- Files are read and compiled into TypeScript program
- Entry points and module resolution are established
- Compiler options are set for strict type checking

### 2. AST Crawling 
- Stack-based traversal begins at entry points  
- Each node is wrapped in a XanthamTag for processing
- Processors determine appropriate handlers based on node type

### 3. Signal-based Processing  
- Complex type relationships are tracked through signals
- Dependencies are automatically discovered
- Computed values are cached and invalidated appropriately

### 4. Data Assembly
- Type and export information is collected
- Duplicates are resolved and merged
- Final data structures are prepared for encoding

### 5. JSON Output
- Final structure is encoded using Thoth.Json
- Results serialized to file in common schema format
- Output is ready for consumption by downstream generators

## Integration Points

### TypeScript Compiler API Integration
Xantham.Fable integrates directly with the TypeScript compiler API to:

- Parse `.d.ts` files accurately
- Resolve symbolic references  
- Perform type checking and inference
- Handle complex type system constructs

### Thoth.Json Encoding
The system uses Thoth.Json for robust JSON serialization with:

- Type-safe encoding
- Schema validation
- Performance optimization
- Cross-platform compatibility

### Memory and Performance Management
Key strategies include:

- Stack-based traversal to prevent recursion limits
- Comprehensive caching of processed elements
- Automatic dependency tracking
- Lazy evaluation of computed values
- Efficient data structures and memory management

## Scalability Features

### 1. Memory Efficiency
- No deep recursion that could overflow JS stack
- Caching reduces redundant computation
- Stack management controls memory usage

### 2. Processing Parallelism
- Independent processing tasks can be parallelized
- Work queuing and scheduling available
- Async operations supported where appropriate

### 3. Extensibility
- Easy to add new node types and processing rules
- Modular design allows focused enhancements
- Well-defined interfaces prevent breaking changes

## Error Handling and Diagnostics

### 1. Type Safety  
- Comprehensive type checking throughout processing
- Compile-time guarantees of correctness
- Safe access patterns prevent runtime errors

### 2. Warning Collection
- Warnings collected during processing
- Diagnostic information for problem identification
- Context-aware error messages

### 3. Graceful Degradation
- Partial processing on malformed inputs
- Fallback mechanisms for unsupported constructs
- Robust error recovery

## Benefits of This Architecture

### 1. Robustness
- Stack overflow prevention for complex ASTs
- Comprehensive error handling and recovery
- Memory-safe processing of large files

### 2. Maintainability
- Clear separation of concerns
- Modular design with well-defined interfaces
- Extensible processing pipeline

### 3. Performance
- Efficient cache usage
- Lazy evaluation of computed values
- Streaming processing capabilities

### 4. Flexibility
- Support for all TypeScript constructs
- Extensible type system
- Standardized output for downstream consumers

This architecture allows Xantham.Fable to reliably process even the most complex TypeScript definitions while providing a solid foundation that can evolve with new TypeScript features and requirements.