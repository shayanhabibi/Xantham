export function echo(value) { return value; }
export function session() {
    return {
        id: "session",
        submit(input, options) {
            if (this.id !== "session") throw new Error("receiver was lost");
            this.options = options;
            return input;
        },
        configure(change, context) {
            if (this.id !== "session" || context !== "context") throw new Error("call arguments changed");
            return change.thinkingLevel;
        },
        inspect(input) {
            return [Object.keys(input).sort().join(","), input.__proto__, input.constructor,
                Object.getPrototypeOf(input) === Object.prototype].join(";");
        }
    };
}
