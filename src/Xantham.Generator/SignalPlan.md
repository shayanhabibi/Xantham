# Signal architecture plan

Aim - to simplify the signal architecture, and allow lazy evaluation of paths
and other parts of the generator.

To be more resilient to recursion, and reduce stack usage in large type graphs.

1. ResolvedTypes are added to the signal dictionary with a RenderScope value.
2. RenderScope is initialised with a bare minimum of information, including the resolved type.
3. Renders are generated immediately in place and assigned to the renderscope.
4. TypeRefRenders which detect transient types are lifted to the local renderscope.
5. As we traverse the graph, we add more resolved types to be worked through to the stack.
6. The stack is processed in a depth-first manner, ensuring that all dependencies are resolved before proceeding.