export const event = {
    eventPhase: 2,
    type: "click",
    NONE: 0,
    CAPTURING_PHASE: 1,
    AT_TARGET: 2,
    BUBBLING_PHASE: 3,
    stopPropagation() {},
};

export function inspect(value) {
    return JSON.stringify(value);
}

export const Level = { Low: 0, High: 1, 0: "Low", 1: "High" };
