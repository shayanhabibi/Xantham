// TEST TARGET: function overload merging
//
// Multiple exported declarations of the same function name must be merged by
// the reader pipeline into a single TsAstNode.FunctionDeclaration node that
// carries TsOverloadableConstruct.Overloaded with all variants in the Set.
//
// A singly-declared function must yield TsOverloadableConstruct.NoOverloads.
//
// Tests assert:
//   - 'process' → Overloaded (not NoOverloads), Values.Length = 2
//   - 'identity' → NoOverloads (not Overloaded)
//   - both names present in the result

// Two overloads — must be merged into Overloaded { string variant; number variant }
export function process(x: string): string;
export function process(x: number): number;

// Single declaration — must remain NoOverloads
export function identity(x: string): string;
