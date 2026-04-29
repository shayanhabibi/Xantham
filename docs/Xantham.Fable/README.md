---
title: Introduction
category: Xantham.Fable
index: 0
---

# Xantham.Fable Project Documentation

The Xantham.Fable project is the TypeScript extractor component of the Xantham schema-driven TypeScript-to-F# bindings generator. It leverages Fable to compile to JavaScript and uses the TypeScript Compiler API (via ts-morph) to crawl `.d.ts` files and emit JSON conforming to the common schema.

## Architecture Overview

Xantham.Fable is designed specifically to work with Fable (an F# to JavaScript compiler) and is compiled to JavaScript via Fable. It serves as the **extraction phase** of the Xantham architecture:

<div class="mermaid">
flowchart LR
    input[.d.ts Input] --> tsc
    subgraph Fable / Xantham.Fable
        tsc[TypeScript Compiler API]
        crawler[AST Crawler]
        thoth_enc[Thoth encoder]
        tsc --> crawler --> thoth_enc
    end
    subgraph Common
        schema[Common Schema]
    end
    schema -.->|contract| thoth_enc
    schema -.->|contract| thoth_dec
    thoth_enc --> json[JSON]
    subgraph .NET / Xantham.Decoder
        json --> thoth_dec[Thoth decoder]
        thoth_dec --> util[Utility layer]
    end
    util --> types[F# Types]
    subgraph Userland Generators
        types --> gen1[Xantham.SimpleGenerator\nFabulous.AST]
        types --> gen2[Your Generator\nString Concatenation / SyntaxOak / ...]
    end
    gen1 & gen2 --> out[F# Bindings]
</div>

## Core Architecture Components

### Signal-based Reactive Architecture

Xantham.Fable uses a sophisticated signal-based reactive system for managing data flow through the AST processing:

- **Source signals**: Hold plain mutable values
- **Computed signals**: Values derived from other signals via thunks
- **Automatic dependency tracking**: Auto-tracking through a scoped collector
- **Dirty propagation**: Transitive and lazy invalidation of computed values

<div class="mermaid">
flowchart LR
    subgraph Signal_System
        Source[Source Signals] --> Auto[Auto Computed Signals]
        Source --> Computed[Explicit Computed Signals]
        Auto --> Value1[Computed Value]
        Computed --> Value2[Computed Value]
    end
    subgraph Dependency_Tracking
        Collector[Dependency Collector] --> Auto
        Collector --> Computed
        Auto --> Propagator[Propagation]
        Computed --> Propagator
        Propagator --> Invalidated[Invalidated Events]
    end
    Value1 --> Cache[Signal Cache]
    Value2 --> Cache
    Invalidated --> Recompute[Recompute Cached Values]
</div>

### Guarded Properties Pattern

Xantham.Fable employs a sophisticated guarded properties pattern to ensure type safety and clean data handling:

- Named accessors for data slots on tags
- Separation between tag bag (for data on the tag object) and keyed bag (for partitioned data)
- Automatic creation and management of pending signals

<div class="mermaid">
flowchart LR
    subgraph Guarded_Properties
        Tag[XanthamTag] --> Slot1[Named Accessors]
        Tag --> Slot2[Keyed Bag]
        Slot1 --> Data1[Data Access/Modification]
        Slot2 --> Data2[Partitioned Data]
        Slot1 --> Pending[Pending Signals]
        Slot2 --> Pending
    end
    subgraph Data_Flow
        Tag -.->|Data Associated| Processor[Processing Logic]
        Processor --> Tag
        Tag --> Cache[Caching System]
        Cache --> Processor
    end
</div>

### Stack-based AST Traversal

To handle deep or complex TypeScript AST structures and avoid JavaScript stack overflows, Xantham.Fable uses:

- **Stack-based iteration**: Instead of recursive descent  
- **Type-based processing**: Each AST node type is handled by specific dispatchers
- **Memory-aware design**: Caches and state management to prevent out-of-memory issues

<div class="mermaid">
flowchart TD
    A[Entry Files] --> B[TypeScript Program]
    B --> C[Module Resolution]
    C --> D[Stack Initialization]
    D --> E[Tag Stack]
    E --> F{Process Tag}
    F -->|XanthamTag| G[Dispatcher]
    G --> H[Handler]
    H --> I[Process Node]
    I --> J[Child Nodes]
    J --> K{Push to Stack}
    K -->|Yes| E
    K -->|No| F
    F -->|Complete| L[Cache & Assemble]
    L --> M[Duplicate Handling]
    M --> N[Final Encoding]
    N --> O[JSON Output]
</div>

## File Structure

The project structure is organized as follows:

- `Program.fs` - Entry point for command-line interface
- `Read.fs` - Main reading logic, including signal management and type processing
- `Types/` - Core type definitions and structures
  - `Signal.fs` - Reactive signal implementation for data flow
  - `GuardedData.fs` - Guarded properties pattern for type safety
  - `Reader.fs` - TypeScript reader and program management
  - `XanTag.fs` - Core tagging system to track and associate information with AST nodes
  - `XanTagKind.fs` - Discriminated union defining all tag types
- `Reading/` - AST traversal logic for processing TypeScript nodes
  - Core dispatcher logic for processing different node types
  - Specific handlers for different TypeScript constructs

## Key Concepts

### AST Node Processing

The system processes TypeScript AST nodes through specialized handlers that translate them into the common schema format:

- Type declarations (classes, interfaces, type aliases, enums)
- Function and method signatures
- Import/export declarations
- JSDoc comments and documentation
- Complex types (unions, intersections, generics, conditional types)

## Processing Pipeline

The typical workflow in Xantham.Fable is:

1. **Initialization**: Create TypeScript program, type checker, and caches
2. **AST Crawling**: Traverse AST nodes using stack-based recursion
3. **Type Processing**: Translate each TypeScript node to common schema format
4. **Signal Management**: Use signals to track dependencies and manage data flow
5. **Duplication Handling**: Merge duplicate types and exports
6. **Encoding**: Serialize final results to JSON using Thoth.Json

## Key Design Patterns

### Signal-based Computation
- Allows for lazy evaluation and automatic dependency tracking
- Enables seamless integration with reactive data flows throughout the AST traversal

### Tag-based Architecture
- Every AST node is associated with a XanthamTag storing related information
- Enables safe, typed access to data associated with each node
- Provides hooks for custom processing without modifying core types

### Caching and Memoization
- Type and export caches prevent duplicate processing
- Signal-based invalidation ensures data consistency
- Memory-aware design prevents stack overflows

### Extensibility
- Modular dispatcher system makes it easy to add new node types
- Clear separation between node processing and data management
- Well-defined interfaces for type extensions

This architecture allows Xantham.Fable to be robust, scalable, and maintainable while properly converting complex TypeScript constructs into a standardized schema that can be consumed by downstream F# generators.