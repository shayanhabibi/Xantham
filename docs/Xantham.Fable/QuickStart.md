---
title: Quick Start
category: Xantham.Fable
index: 1
---

# Quick Start Guide for Xantham.Fable

This guide will help you quickly understand and get started with working on the Xantham.Fable project.

## Project Overview

Xantham.Fable is the TypeScript extraction component of the Xantham schema-driven TypeScript-to-F# bindings generator. It uses Fable to compile F# code to JavaScript and leverages the TypeScript Compiler API to parse `.d.ts` files and emit JSON conforming to a common schema.

## Getting Started

### Prerequisites

1. **F# Development Environment**:
   - Visual Studio 2022 or later with F# tools
   - VS Code with Ionide F# extension
   - .NET SDK (version 6.0 or later)

2. **Building Tools**:
   - Node.js and npm (for package management)
   - Git for source control

### Setup Instructions

1. **Clone the Repository**:
```bash
git clone https://github.com/your-organization/xantham.git
cd xantham
```

2. **Restore Dependencies**:
```bash
dotnet restore
```

3. **Run Tests**:
```bash
npm run watch-test
```

## Understanding the Codebase Structure

### Core Directory Structure

```text
src/
├── Xantham.Fable/                    # Main project
│   ├── Program.fs                    # Entry point
│   ├── Read.fs                       # Main reading logic
│   ├── Types/                        # Core type definitions
│   │   ├── Signal.fs                 # Reactive signal system
│   │   ├── GuardedData.fs            # Guarded properties pattern
│   │   ├── Reader.fs                 # TypeScript reader
│   │   └── XanTag.fs                 # Tag system
│   └── Reading/                      # AST traversal logic
│       ├── Dispatcher.fs             # Node dispatching
│       ├── MemberDeclaration.fs      # Member processing
│       ├── TypeDeclaration.fs        # Type declaration processing
│       └── ... more modules
└── Xantham.Common/                   # Shared schema
```

### Key Concepts to Understand

#### 1. The XanthamTag System
The fundamental unit of data association in Xantham.Fable:

```fsharp
// A tag represents an AST node with associated metadata
type XanthamTag = {
    Value: XanTagKind        // Type of node being processed
    IdentityKey: IdentityKey // Unique identifier
    // ... other metadata
}
```

#### 2. Signal-based Architecture
Xantham.Fable uses a sophisticated signal system for reactive data flow:

```fsharp
// Source signal (mutable value)
let sourceSignal = Signal.source 42

// Computed signal (depends on others)
let computedSignal = Signal.auto (fun () -> sourceSignal.Value * 2)

// Pending signal (filled later)
let pendingSignal = Signal.pending<int>()
// Later...
Signal.fill 100 pendingSignal
```

#### 3. Stack-based Traversal
To prevent JavaScript stack overflows from deep AST structures:

```fsharp
// Processing uses explicit stack instead of recursion
let runReader (reader: TypeScriptReader) =
    let mutable stackEntry = Unchecked.defaultof<XanthamTag>
    while reader.stack.TryPop(&stackEntry) do
        Dispatcher.dispatch reader stackEntry
    reader
```

## Key Files to Explore

### 1. Main Entry Point
`Program.fs` - Contains the CLI interface and main execution flow:

```fsharp
// Entry point that handles command line arguments
let main argv =
    match argv with
    | [| inputFile |] -> 
        let reader = TypeScriptReader.create inputFile
        // Process and output results
    | _ -> 
        // Handle error / show help
```

### 2. Core Reading Logic  
`Read.fs` - Main processing logic for transforming AST to JSON:

```fsharp
// Core reading function
let read (reader: TypeScriptReader) : EncodedResult =
    // Setup processing
    // Traverse AST using stack
    // Process type declarations
    // Resolve duplicates
    // Encode result
```

### 3. Signal System
`Types/Signal.fs` - The reactive system at the heart of data flow:

```fsharp
// Reactive signal with dependency tracking
type Signal<'T> = {
    Value: 'T                // Current value
    Invalidated: IEvent<unit> // Dependency changes
    // ... methods for set, compute, etc.
}
```

### 4. Tag Dispatching
`Reading/Dispatcher.fs` - Routes processing to appropriate handlers:

```fsharp
let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.Value with
    | XanTagKind.TypeDeclaration typeDecl ->
        TypeDeclaration.dispatch ctx tag typeDecl
    // ... other handlers
```

## Development Workflow

### 1. Adding New Node Type Processing

To add processing for a new TypeScript construct:

1. **Define tag kind** in `XanTagKind.fs`:
```fsharp
type XanTagKind =
    // ... existing kinds
    | CustomNode of CustomNodeData
```

2. **Create processing module** in `Reading/CustomNode.fs`:
```fsharp
module CustomNode =
    let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) (nodeData: CustomNodeData) =
        // Process the custom node
        // Return results through signals/property system
```

3. **Update dispatcher** in `Reading/Dispatcher.fs`:
```fsharp
| XanTagKind.CustomNode nodeData ->
    CustomNode.dispatch ctx tag nodeData
```

### 2. Working with Signals and Properties

```fsharp
// Getting or setting guarded properties
let processNode (ctx: TypeScriptReader) (tag: XanthamTag) =
    // Get or create a builder
    let builder = AstNodeBuilder.getOrSetWith (fun () -> SType()) tag
    
    // Set properties on the builder
    builder.NodeType <- "CustomType"
    
    // Store back on tag
    AstNodeBuilder.set builder tag
```

### 3. Debugging Tips

- Use the `debugLocationAndForget` helper for logging
- Check `XanthamTag.debugLocationAndCommentAndForget` for detailed debugging
- Monitor the stack for processing flow
- Use signal `.Value` access to track dependency tracking

## Testing

The project implements unit tests using `Fable.Mocha`, these can be run using the following commands:

```bash
# Run tests
npm run test

# Run tests in watch mode
npm run watch-test

# Run tests for signals
npm run test:signal
```

## Common Patterns for Extending

### 1. Adding New Type Processing
```fsharp
// In Reading/MyNewType.fs
module MyNewType =
    let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) (typeNode: MyNewTypeNode) =
        // Process with computed signals
        let computedResult = Signal.auto (fun () -> 
            // Complex logic here
            processNewType typeNode
        )
        
        // Store result using guarded properties
        TypeSignal.fill computedResult typeSignal
```

### 2. Handling Complex Types
```fsharp
// For recursive or complex types
let handleComplexType (ctx: TypeScriptReader) (tag: XanthamTag) =
    if not (isAlreadyProcessed ctx tag.IdentityKey) then
        // Process complex type
        // Push children to stack
        typeChildren 
        |> Array.iter (fun child -> 
            pushToStack ctx (createTagFor child)
        )
    else
        // Skip already processed
        ()
```

This quick start guide should help you understand the main architecture, key components, and development patterns used in Xantham.Fable. The system is designed to be extensible and follows F# best practices for reactive programming and functional design.