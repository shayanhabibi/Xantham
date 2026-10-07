import { test, expect } from "vitest";
import { render, isServer } from "@solidjs/web";
import { App_view } from "./partas/js/Consumer.fs.jsx";

test("generated Partas properties reach the input element", () => {
  expect(isServer).toBe(false);
  const root = document.createElement("div");
  const dispose = render(() => App_view(), root);
  try {
    const input = root.querySelector("input");
    expect(input).not.toBeNull();
    expect(input.value).toBe("custom value");
    expect(input.title).toBe("custom title");
  } finally { dispose(); }
});
