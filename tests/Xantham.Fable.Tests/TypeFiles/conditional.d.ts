// TEST TARGET: conditional type emission
//
// Verifies that TsAstNode.Conditional is emitted for T extends U ? X : Y
// type aliases, and that the Check, Extends, True, and False TypeKeys resolve
// to the expected nodes.  Boolean literal branches (true/false) let us assert
// exact TypeKey content without relying on runtime-generated IDs.

export type IsString<T> = T extends string ? true : false;
