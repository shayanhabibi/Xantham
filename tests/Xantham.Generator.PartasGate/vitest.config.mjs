import { defineConfig } from "vitest/config";
import solid from "@solidjs/vite-plugin";
import { fileURLToPath } from "node:url";

const scratch = fileURLToPath(new URL("../.scratch/partas-gate/", import.meta.url));
export default defineConfig({
  root: scratch,
  plugins: [solid({ include: ["**/*.jsx"], exclude: ["**/fable_modules/**"], dev: true })],
  resolve: {
    conditions: ["browser", "development"]
  },
  test: {
    environment: "jsdom",
    include: ["runtime.test.jsx"],
    server: { deps: { inline: [/solid-js/, /@solidjs\//] } }
  }
});
