---
title: Architecture
category: Xantham.Fable
categoryindex: 2
index: 2
---

# Architecture of Xantham.Fable

The architecture of Xantham.Fable is designed to transform TypeScript `.d.ts` files into a standardized JSON schema that can be consumed by downstream F# generators. This multi-layered architecture is built for scalability, maintainability, and robustness.

## System Overview

Xantham.Fable operates as a TypeScript AST crawler and converter that processes complex TypeScript definitions into a common schema format. The architecture can be visualized as:

```text
Input .d.ts files 
    ↓
TypeScript Compiler API (ts-morph)
    ↓
Stack-based AST traversal
    ↓
Reactive signal processing with guarded properties
    ↓
Common Schema JSON Output
```

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

### XanthamTag System

The fundamental unit of data association in the AST processing:

```fsharp
type XanthamTag = {
    Value: XanTagKind                       // Tag type and data
    IdentityKey: IdentityKey                // Unique identifier
    // Additional metadata for tracking
}

type XanTagKind = 
    | Ignore
    | MemberDeclaration of MemberDeclaration
    | TypeDeclaration of TypeDeclaration
    | TypeNode of TypeNode
    | JSDocTag of JSDocTags
    | Type of TypeFlagPrimary
    | ModulesAndExports of ModulesAndExports
    | LiteralTokenNode of LiteralTokenNode
```

### Stack-based Processing Architecture

A critical component that prevents JavaScript stack overflows:

```text
┌─────────────────────────────────────────────────────────────┐
│                    Stack Management                         │
├─────────────────┬─────────────────┬─────────────────────────┤
│   XanthamTag    │   Processing    │       Work Queue        │
│   Stack         │   State         │       (LIFO Queue)      │
├─────────────────┼─────────────────┼─────────────────────────┤
│   ┌───────────┐ │   ┌───────────┐ │   ┌───────────────────┐ │
│   │  Member   │ │   │  Type     │ │   │  TypeDeclaration  │ │
│   │Declaration│ │   │  Node     │ │   │  Process          │ │
│   └───────────┘ │   └───────────┘ │   └───────────────────┘ │
│   ┌───────────┐ │   ┌───────────┐ │   ┌───────────────────┐ │
│   │  Type     │ │   │  Type     │ │   │  Module           │ │
│   │Declaration│ │   │  Node     │ │   │  Processing       │ │
│   └───────────┘ │   └───────────┘ │   └───────────────────┘ │
│                 │                 │                         │
│                 │                 │                         │
└─────────────────┴─────────────────┴─────────────────────────┘
         ↑                    ↑                     ↑
 Current Stack State       Processing Loop       Work Items
```

### Signal Architecture Integration

Core reactive data flow system:

```text
┌─────────────────────────────────────────────────────────────┐
│                     Signal System                           │
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
┌─────────────────────────────────────────────────────────────┐
│                  Guarded Properties                         │
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