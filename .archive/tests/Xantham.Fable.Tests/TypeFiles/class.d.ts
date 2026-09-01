// TEST TARGET: class declaration emission
//
// Verifies that TsAstNode.Class is produced for a class declaration, that
// TsClass.Constructors carries the correct parameter list, and that instance
// members (method and readonly property) are emitted as TsMember cases.

export class Animal {
    constructor(name: string, age: number);
    speak(): string;
    readonly name: string;
}
