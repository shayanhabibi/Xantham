// TEST TARGET: cross-package import — data-layer imports from validators
//
// package.json name: "@app/data-layer"
// Imports Validator from the sibling validators package.
// Entry file types should get Source = "@app/data-layer" (from fallback).
// Imported types from validators should get Source from the import specifier.

import { Validator } from "../validators/index";

export interface DataModel {
    id: string;
    name: string;
    validate: Validator;
}

export type ModelId = string;
