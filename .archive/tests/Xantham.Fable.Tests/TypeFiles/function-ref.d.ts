export declare function getPerson(name: string): object;

export interface Api {
    person: typeof getPerson
}