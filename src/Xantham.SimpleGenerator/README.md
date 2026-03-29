# SimpleGenerator

An example generator which utilises the Xantham
decoded TS type AST to generate bindings
for Fable.

## Strategy

1. Decode TS AST
2. Generate F# Bindings for Top Level Exports
3. Recursively generate bindings for nested types and exports
4. Write to disk

## Special Cases

Derived types with literal fields.

```ts
type Foo<T extends "A" | "B" = "A"> = {
    bar: T
}
type A = Foo<'A'>
type B = Foo<'B'>

interface Ax extends Foo<'A'> {
    boo: string
}
```

### Foo

Type: TypeAlias

Internal Type: TypeLiteral

Type Parameters: T

#### TypeParameter T

Default is a literal: 'A'

Constraint is a Union of literals 'A' and 'B'

#### Internal TypeLiteral

A single property `bar` which references the type parameter.

### A

Type: TypeAlias

~ In xantham, this would have been resolved to:

Internal Type: TypeLiteral

#### Internal TypeLiteral

A single property `bar` which references the literal
`'A'`.

I suppose the only way that I can tell that the
type alias is actually an implementation of
the previous type alias is by observing that
the type of the field literal matches the types
utilised by the union in the constraint of the
type parameter.

Otherwise, we've reduced the type to the calculated
type, so we can't really use any single field 
to infer this.

Perhaps we can add this?


At the moment, with an example like

```ts
type Foo<T extends "A" | "B" = "A"> = {
    bar: T
}
type A = Foo<'A'>
```

Xantham will correctly resolve `A` to
the type literal `{ bar: 'A' }`. However,
we don't have explicit context that type `A` is
an implementation of `Foo<T>`.

How can this be corrected while maintaining end output?

# Notes

Pathing presents an issue. A type can be derived from multiple paths when it is
a transient type. We would like them all to share the same definition.

The most reasonable approach is to have a shared definition for each path, with
the first common path being the root for the definition.

Each type, and its type reference can therefore depend on the path of the
definition it is derived from.

To mitigate this, renders should be path aware; they should be able to
determine the path of the type they are rendering when given a path that
they are rendering.

In this respect, all render functions should take a path, to which they
construct their output. The computations that determine this can be cached
to avoid redundant calculations.

---

The types have been adjusted as above.

Now the rendering must be adjusted.

The TypeResolver will determine short circuiting.

Each of the render functions must provide their renders
when given a root path and a path.

---

# Typescript -> F# AST

Concrete path types are simple to render. The issue is the references to transient types such as
Conditional types or Intersections which need to be constructed into types on the F# side.

Imagine a scenario where we have a type like this:
```ts
type Foo = {
    bar: { a: string } & { b: int }
}

type Bar = {
    foo: { a: string } & { b: int }
}
```

Lets begin with reading just one of the types.

The type information will result in an object that looks something like this:

```fsharp
TypeAlias {
    Name: "Foo"
    TypeParameters: []
    InternalType: TypeLiteral {
        Members: [
            Property {
                Name: "bar"
                Type: TypeIntersection [
                    TypeLiteral {
                        Members: [
                            Property {
                                Name: "a"
                                Type: TypePrimitive "string"
                            }
                            Property {
                                Name: "b"
                                Type: TypePrimitive "int"
                            }
                        ]
                    }
                ]
            }
        ]
    }
}
```

When we render the type, we want to achieve something like so:

```fsharp
module Foo =
    type Bar =
        abstract a: string
        abstract b: int
        
type Foo =
    abstract bar: Foo.Bar
```

We recognise the transient type to be constructed on the property `bar`, and we
construct the type on the F# side by creating a module with the type name and creating
the type using the properties name.

Now let's bring the two types together. If we follow the same approach, we would get the following:
```fsharp
module Foo =
    type Bar =
        abstract a: string
        abstract b: int
        
type Foo =
    abstract bar: Foo.Bar
    
module Bar =
    type Foo =
        abstract a:
        abstract b:
    
type Bar =
    abstract foo: Bar.Foo
```

This introduces the first problem: the type `Bar` and `Foo` are structurally identical, and would be
synonymous in TS. However, they are separate types in F# and are not interchangeable.

The following would instead be better:
```fsharp
[<EditorBrowsable(EditorBrowsableState.Never)>]
type TransientIntersection1 =
    abstract a: string
    abstract b: int

module Foo =
    type Bar = TransientIntersection1

module Bar =
    type Foo = TransientIntersection1

type Foo =
    abstract bar: Foo.Bar
type Bar =
    abstract foo: Bar.Foo
```

We now have a type `TransientIntersection1` which is not visible to the user editor,
but is used to construct the type `Foo.Bar` and `Bar.Foo` as a type alias.

The two types are now structurally and nominally identical. Of course, the naming is an issue.

The current approach would create nonsensical names.

Another approach would be to use the shortest path to the definition to be the primary definition (or
the first definition alphabetically when they are in the same hierarchy) and then have the subsequent
definitions reference this as the alias.

```fsharp
module Foo =
    type Bar =
        abstract a: string
        abstract b: int

type Foo =
    abstract bar: Foo.Bar

module Bar =
    type Foo = Foo.Bar

type Bar =
    abstract foo: Bar.Foo
```

What if the transient type, itself defines a transient type?

```ts
type Foo = {
    bar: { a: string } & { b: { c: string } }
}

type Bar = {
    foo: { a: string } & { b: { c: string } }
}
```

Constructing the type Foo in isolation would result in the following:

```fsharp
module Foo =
    module Bar =
        type B =
            abstract c: string
    type Bar =
        abstract a: string
        abstract b: Bar.B

type Foo =
    abstract bar: Foo.Bar
```

Now if we bring the two types together using our suggested approach from before:

```fsharp
module Foo =
    module Bar =
        type B =
            abstract c: string
    type Bar =
        abstract a: string
        abstract b: Bar.B

type Foo =
    abstract bar: Foo.Bar

module Bar =
    type Foo = Foo.Bar

type Bar =
    abstract foo: Foo.Bar
```

However, we have a problem. The type reference for the subsequent transient type `Bar.B` does not follow
the same path such that you can construct the type using `Bar.Foo.B`.

How can we resolve this?

```fsharp
module Foo =
    module Bar =
        type B =
            abstract c: string
    type Bar =
        abstract a: string
        abstract b: Bar.B

type Foo =
    abstract bar: Foo.Bar

module Bar =
    module Foo =
        type B = Foo.Bar.B
    type Foo = Foo.Bar

type Bar =
    abstract foo: Bar.Foo
```

In terms of the IDE experience, this is not the greatest.
The alternative is to create op_Implicit operators for the types.

```fsharp
module Foo =
    module Bar =
        [<Interface>]
        type B =
            abstract c: string
            static member inline op_Implicit (value: Bar.Foo.B): B = !!value
            static member inline op_ErasedCast (value: Bar.Foo.B): B = !!value
    [<Interface>]
    type Bar =
        abstract a: string
        abstract b: Bar.B
        static member inline op_Implicit (value: Foo.Bar): Bar = !!value
        static member inline op_ErasedCast (value: Foo.Bar): Bar = !!value

type Foo =
    abstract bar: Foo.Bar

module Bar =
    module Foo =
        [<Interface>]
        type B =
            abstract c: string
            static member inline op_Implicit (value: Bar.Foo.B): B = !!value
            static member inline op_ErasedCast (value: Bar.Foo.B): B = !!value
    [<Interface>]
    type Foo =
        abstract a: string
        abstract b: Foo.B
        static member inline op_Implicit (value: Bar.Foo): Foo = !!value
        static member inline op_ErasedCast (value: Bar.Foo): Foo = !!value

type Bar =
    abstract foo: Bar.Foo
```

> [!NOTE]
> The `Interface` attribute is required to explicitly declare the type as an interface, and prevent
> issues when using record constructors.

We also include the erased cast operator, because the `op_Implicit` is nuanced in practice.

```fsharp

let y (value: Foo.Bar): unit = ignore value
let x: Bar = Unchecked.defaultof<_>

// == COMPILES ==
y x.foo // works
!^x.foo |> y // works

// == DOES NOT COMPILE == possibly due to recursive module/namespaces
x.foo |> y // the type Bar.Foo is not compatible with the type Foo.Bar

```

This provides the most consistent experience, balancing IDE experience and practicality.

The consideration for this would be that this can quickly become a performance issue for large
projects. But it is a tradeoff that the user will have to make. They can decide whether to generate
the converters or not.

So what does this mean for Xantham? What do we have to track?

We need to track the path as we traverse the AST.
We need to construct the type reference relative to the path.

Transient types are never named, so it is harmless whether we add them to the key path or not.
All Transient types will be localised to the path of the definition they are derived from.

We will track what paths are defined for what master keys, and then create static members which translate
between them.

If we want to be able to cache base computations for the paths et al, then we have to allow the
type reference to be conditional on the passed path. For instance, in the previous example, we might
have the master key for the intersection type `{ a: ... } & ...` cached with a function as so:

```fsharp
let transientTypeKey = 1 // { a: { c: string } } & { b: int }
let transientTypeKey2 = 2 // { c: string }

// == Type Foo
let rootPath = [ ] // represents the root
let typePath = rootPath @ [ "Foo" ] // the type is located in the base namespace.
let propertyPath = typePath @ [ "Foo"; "bar" ] // the property is a member of the type.
let transientTypePath = propertyPath @ [] // [ "Foo"; "bar" ] - the transient type has no name
let transientTypeProperty = transientTypePath @ [ "a" ] // [ "Foo"; "bar"; "a" ] - the transient subtype property
let transientSubTypePath = transientTypePath @ [ "a" ] 
// [ "Foo"; "bar"; "a" ]


// == Type Bar
module Bar =
    let typePath = rootPath @ [ "Bar" ]
    let propertyPath = typePath @ [ "foo" ]
    let transientTypePath = propertyPath @ []
    let transientTypeProperty = transientTypePath @ [ "a" ]
    let transientSubTypePath = transientTypePath @ [ "a" ]
    // [ "Bar"; "foo"; "a" ]
```

Whenever the path appends a concrete pathed type, then the concrete path is used in its entirety.
