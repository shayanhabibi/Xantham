// Tag values that collide. `taggedUnionShape` requires every arm's tag value to be distinct,
// and the refusal used to be silent: the union fell through to the erased-union mapping and
// the manifest carried no `DT` finding for it under any name.
//
// Reduced from `@cloudflare/workers-types`' `TailStream.EventType`, where an intersection over
// a union distributes into two members both tagged `"log"`.

// The regression guard: uniformly tagged, every value distinct. Stays a discriminated union.
export type Distinct =
    | { readonly kind: "circle"; readonly radius: number }
    | { readonly kind: "square"; readonly side: number }
    | { readonly kind: "point" };

// The second candidate. `channel` is the first arm's first property and collides across the
// arms; `verb` is the second and discriminates cleanly, so the union is a discriminated union
// on `verb` and the collision on `channel` is reported nowhere.
export type Signal =
    | { readonly channel: "bus"; readonly verb: "start"; readonly at: number }
    | { readonly channel: "bus"; readonly verb: "stop"; readonly reason: string };

// The fold. Two arms share `state: "done"` and a third discriminates, so the shared pair folds
// into one case and the union survives with two. The folded arms agree on the tag alone, so
// `value` and `error` are what the fold costs.
export type Terminal =
    | { readonly state: "done"; readonly value: number }
    | { readonly state: "done"; readonly error: string }
    | { readonly state: "pending" };

// The refusal. Every arm carries `type: "log"`, so the fold leaves one case and there is
// nothing to discriminate. Written flat: this is the pair the checker produces from the
// `Log` intersection below.
export type Onset =
    | { readonly type: "log"; readonly level: string; readonly message: object; readonly truncated?: false }
    | { readonly type: "log"; readonly level: string; readonly message: string; readonly truncated: true };

// The `TailStream.EventType` shape verbatim. The checker distributes the intersection over the
// union, so the two arms arrive flagged `Intersection` rather than `Object` and the union is
// not considered a tagged one at all - a gap upstream of the fold, recorded here as a negative.
export type Log = { readonly type: "log"; readonly level: string } & (
    | { readonly message: object; readonly truncated?: false }
    | { readonly message: string; readonly truncated: true }
);

// A negative: `kind` is a bare string on one arm, so no candidate is uniform and the union was
// never a tagged one to refuse.
export type Loose =
    | { readonly kind: "one"; readonly a: number }
    | { readonly kind: string; readonly b: number };
