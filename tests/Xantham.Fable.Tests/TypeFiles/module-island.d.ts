// A `declare module` island: named-module declarations inside an ambient file,
// the shape workers-types uses for "cloudflare:workers" et al.
declare module "test:island" {
  export class IslandClass {
    islandProp: string;
  }
  export interface IslandShape {
    width: number;
  }
}
