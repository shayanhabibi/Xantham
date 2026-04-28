---
title: Introduction
category: Xantham.Fable
index: 0
---

# Xantham.Fable Project Documentation

The Xantham.Fable project is the TypeScript extractor component of the Xantham schema-driven TypeScript-to-F# bindings generator. It leverages Fable to compile to JavaScript and uses the TypeScript Compiler API (via ts-morph) to crawl `.d.ts` files and emit JSON conforming to the common schema.

## Architecture Overview

Xantham.Fable is designed specifically to work with Fable (an F# to JavaScript compiler) and is compiled to JavaScript via Fable. It serves as the **extraction phase** of the Xantham architecture:

```text
.d.ts Input → TypeScript Compiler API → AST Crawler → Thoth encoder → JSON (Common Schema)
```

## Core Architecture Components

### Signal-based Reactive Architecture

Xantham.Fable uses a sophisticated signal-based reactive system for managing data flow through the AST processing:

- **Source signals**: Hold plain mutable values
- **Computed signals**: Values derived from other signals via thunks
- **Automatic dependency tracking**: Auto-tracking through a scoped collector
- **Dirty propagation**: Transitive and lazy invalidation of computed values

### Guarded Properties Pattern

Xantham.Fable employs a sophisticated guarded properties pattern to ensure type safety and clean data handling:

- Named accessors for data slots on tags
- Separation between tag bag (for data on the tag object) and keyed bag (for partitioned data)
- Automatic creation and management of pending signals

### Stack-based AST Traversal

To handle deep or complex TypeScript AST structures and avoid JavaScript stack overflows, Xantham.Fable uses:

- **Stack-based iteration**: Instead of recursive descent  
- **Type-based processing**: Each AST node type is handled by specific dispatchers
- **Memory-aware design**: Caches and state management to prevent out-of-memory issues

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