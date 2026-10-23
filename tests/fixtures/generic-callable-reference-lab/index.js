export const callback = () => (left, right) => left + right;
export const pair = value => [value, callback()];
export const accept = value => value("left", "right");
