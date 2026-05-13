// TEST TARGET: cross-file type reference (consumer)
//
// Imports Point2D from shapes.d.ts and uses it as a heritage type.
// Verifies that:
//   - cross-file heritage TypeKeys resolve correctly
//   - Vector2D.Heritage.Extends[0].Type resolves to Point2D
//   - Vector3D.Heritage.Extends[0].Type resolves to Vector2D

import { Point2D } from "./shapes";

export interface Vector2D extends Point2D {
    magnitude: number;
}

export interface Vector3D extends Vector2D {
    z: number;
}
