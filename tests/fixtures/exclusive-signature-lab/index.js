export function render(page) {
  if (typeof page.url === "string" && page.html === undefined) return `url:${page.url}`;
  if (typeof page.html === "string" && page.url === undefined) return `html:${page.html}`;
  throw new Error("Expected exactly one page input");
}
