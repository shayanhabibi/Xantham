export interface Options {
    text: string;
}

export interface Model<T> {
    generate(options: Options): T;
}
