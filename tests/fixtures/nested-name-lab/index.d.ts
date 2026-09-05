// Anonymous inline shapes take the name of the position they are reached through, and that
// name is a module path rather than a concatenation: `Widget.Options` is `type Options` inside
// `module Widget`. See docs/plans/generator-type-mapping.md 4.4 and wave six lane AD.
//
// What this fixture pins is the *disambiguation*. Two owners with an `options` member are two
// declarations, not `WidgetOptions` and `WidgetOptions2`, because the path they are declared
// at already separates them. What is left over - a shape with no position of its own to take -
// keeps the numeric suffix, and the last case below says which those are.

// ---------------------------------------------------------------------------
// Two owners, one member name. The whole point.
// ---------------------------------------------------------------------------

/** The first owner of an `options` shape. */
export interface Widget {
    /** Nests as `Widget.Options`. */
    options: {
        /** Nests one level deeper, as `Widget.Options.Retry`. */
        retry: {
            attempts: number;
            /** A literal union under two nests: a StringEnum inside two modules. */
            backoff: "linear" | "exponential";
        };
        label: string;
    };
    /** A second position under the same owner: `Widget.Metrics`. */
    metrics: { hits: number };
}

/** The second owner of an `options` shape, distinct from the first without a suffix. */
export interface Gadget {
    /** Nests as `Gadget.Options`, and its members differ from `Widget.Options`. */
    options: { serial: string };
    /** A nested shape referring to another owner's nested shape, across two modules. */
    borrowed: { from: Widget["options"] };
}

// ---------------------------------------------------------------------------
// The run gate's subject: a value crosses the boundary as a nested shape and
// comes back as a nested StringEnum.
// ---------------------------------------------------------------------------

/** Reads the nested StringEnum off a nested shape. */
export declare function backoffOf(retry: Widget["options"]["retry"]): string;

/** Hands back a nested shape built on the JavaScript side. */
export declare function defaultRetry(): Widget["options"]["retry"];

// ---------------------------------------------------------------------------
// A function export nests under a module with no type beside it.
// ---------------------------------------------------------------------------

/** The parameter shape nests as `Configure.Settings`, under a module and no type. */
export declare function configure(settings: { verbose: boolean }): void;

// ---------------------------------------------------------------------------
// A key outside the plain identifier shape. Only an identifier opens a module,
// so this position concatenates; the concatenation is then reduced to what a
// declaration name admits, and `key-sanitise-lab` pins that reduction.
// ---------------------------------------------------------------------------

/** The owner of a member whose JavaScript key does not open a module. */
export interface Registry {
    "beta channel": { model: string };
}

// ---------------------------------------------------------------------------
// The residue. A union's arms share their owner's path, so two arms that both
// need a name reach the identical path and the later one takes a suffix.
// ---------------------------------------------------------------------------

/** Both arms are object shapes, and both are reached at `Choice.Either`. */
export interface Choice {
    either: { left: string; leftAt: number } | { right: string; rightAt: number };
}

// ---------------------------------------------------------------------------
// Two declarations of one name, separated by the namespace one of them is
// written in. TypeScript tells them apart that way and so does F#: the
// namespaced one nests, and neither takes a number.
// ---------------------------------------------------------------------------

/** The unqualified `Node`, reached by that name. */
export interface Node {
    id: string;
}

/** A namespace whose `Node` is a second declaration of the name, exported by nothing. */
declare namespace Cluster {
    /** Nests as `Cluster.Node`. */
    interface Node {
        peers: number;
    }
}

/** Reaches the namespaced declaration, so the reference position is pinned too. */
export declare function joinCluster(node: Cluster.Node): Node;
