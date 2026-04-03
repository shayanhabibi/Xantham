// TEST TARGET: cross-package import — validators package
//
// package.json name: "@app/validators"
// Provides Validator interface and ValidationResult type alias.

export interface Validator {
    validate(input: unknown): boolean;
}

export type ValidationResult = {
    valid: boolean;
    errors: string[];
};
