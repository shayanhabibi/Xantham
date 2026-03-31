// TEST TARGET: namespace (module) declaration emission
//
// Verifies that TsAstNode.Module is emitted for a namespace declaration with
// IsNamespace = true, that TsModule.Types lists the correct number of nested
// declarations, and that the nested interfaces are independently reachable in
// the result by name.

declare namespace Geometry {
    interface Point {
        x: number;
        y: number;
    }
    interface Circle {
        center: Point;
        radius: number;
    }
}
