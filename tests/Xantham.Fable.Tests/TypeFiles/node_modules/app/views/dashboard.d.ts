// TEST TARGET: middle of transitive import chain
//
// Imported by index.d.ts as "./views/dashboard".
// Imports Widget from "../components/widget".

import { Widget } from "../components/widget";

export interface Dashboard {
    widgets: Widget[];
    columns: number;
}
