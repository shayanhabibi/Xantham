// A second entry reusing the root's generic tagged union through a declaration catalog.
import type { Job } from "./index.js";
export { Job } from "./index.js";

export interface Runner<T> {
    run(job: Job<T>): Job<T>;
    poll(): Job<T> | undefined;
}
