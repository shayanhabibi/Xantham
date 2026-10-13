// The runtime behind tag-map-lab: each method records the key it was called with.
export const doc = {
  last: undefined,
  createElement(tagName) { this.last = tagName; return { tagName }; },
  createSvg(tagName) { this.last = tagName; return { tagName }; },
  getElementsByTagName(name) { this.last = name; return { length: 1, item: () => ({ tagName: name }) }; },
  hasTag(tag) { this.last = tag; return tag === "div"; },
  findLoose(tag) { this.last = tag; return { length: 0, item: () => null }; },
};
export function valuesOf(source) { return Object.values(source); }
