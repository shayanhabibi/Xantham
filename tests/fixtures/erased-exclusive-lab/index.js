export const left = () => Object.assign(value => value + 10, { left: 1, right: undefined });
export const right = () => Object.assign(value => value * 2, { right: "twice", left: undefined });
export const incomingLeft = left;
export const incomingRight = right;
export const generic = () => Object.assign(value => value + "!", { right: "generic", left: undefined });
export const genericMethod = () => Object.assign(value => value, { right: "generic", left: undefined });
export const overloaded = () => Object.assign(value => value === "a" ? 1 : 2, { right: "overload", left: undefined });
export const constructor = () => Object.assign(class { constructor(value) { this.value = value; } }, { right: "constructor", left: undefined });
export const escaped = () => Object.assign(value => value + 1, { "left-$0": 1, right: undefined });
