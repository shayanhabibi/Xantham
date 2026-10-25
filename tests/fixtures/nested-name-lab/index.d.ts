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
    namespace Inner {
        interface PrivateItem { key: string }
    }
}

/** Reaches the namespaced declaration, so the reference position is pinned too. */
export declare function joinCluster(node: Cluster.Node): Node;
export declare function clusterItem(): Cluster.Inner.PrivateItem;

// A function abbreviation cannot close this cycle in F#, but a callable interface can.
export namespace Recursive {
    export interface Thunk { (): Value }
    export type Value = string | Thunk;
    export interface GenericThunk<T> { (): GenericValue<T> }
    export type GenericValue<T> = T | GenericThunk<T>;
    export interface Reader { (): Record }
    export interface Record { next: Reader }
    export interface Plain { (): string }
    export interface RestOverloads {
        apply(...values: string[]): void;
        apply(values: string[]): void;
    }
}

export type SharedCode = "ready" | "closed";
export namespace Reexports {
    export { SharedCode };
}

// ---------------------------------------------------------------------------
// An exported namespace's members are API whether or not a signature reaches
// them. Each is declared once, under its namespace, as TypeScript spells it.
// ---------------------------------------------------------------------------

/** A namespace with a value beside its types. */
export namespace Pool {
    /** Declared though no signature reads it. */
    export interface Idle { since: number }
    /** A class member: the namespace object reads its constructor. */
    export class Conn { close(): void }
    export function size(): number;
    /** A namespace inside a namespace. */
    export namespace Inner {
        /** Declared two namespaces deep. */
        export interface Deep { depth: number }
        /** Shares a name with the exported `Node` and with `Shapes.Node`. */
        export interface Node { inner: boolean }
    }
    /** A second name for a nested member: abbreviates the first. */
    export import Depth = Inner.Deep;
}

/** A namespace of types alone. */
export namespace Shapes {
    export interface Lonely { d: number }
    /** Read by `widen` as well as harvested: declared once. */
    export interface Reached { r: string }
    export interface Node { shape: string }
    /** Named like the value container, which keeps its name. */
    export interface Exports { e: number }
}

/** Reads a namespace member that is harvested too. */
export declare function widen(r: Shapes.Reached): void;

/** An alias merged with a namespace. */
export type Pick = { kind: "a" } | { kind: "b" };
export namespace Pick {
    export interface Extra { e: number }
}

/** A tagged union whose arms are its namespace's members. */
export type Signal = Signal.Start | Signal.Stop;
export namespace Signal {
    export interface Start { kind: "start"; at: number }
    export interface Stop { kind: "stop" }
}
export declare function send(signal: Signal): void;

/** Interfaces of one namespace that refer to each other, read by a signature and not. */
export namespace Markup {
    export interface Element { children: Element[]; attributes: Attributes }
    export interface Attributes { parent?: Element; ref?: (element: Element) => void }
    export interface Fragment { items: Fragment[]; slot?: Slot }
    export interface Slot { fill: Fragment }
}

/** Reads the DOM `Element` beside a namespace member of that name. */
export declare function mount(host: Element): Markup.Element;

export namespace Original {
    export interface Item<T> { value: T }
    export namespace Inner {
        export interface Leaf<T> { item: Item<T> }
    }
}
export import PublicAlias = Original;
export namespace AliasHolder {
    export import First = Original;
    export import Second = Original;
}
export declare function readAlias(): PublicAlias.Inner.Leaf<string>;

declare namespace HiddenAliasSource {
    namespace Inner {
        interface Item<T> { value: T }
        namespace Deep { interface Leaf<T> { item: Item<T> } }
    }
}
export import OnlyAlias = HiddenAliasSource.Inner;
export declare function readOnlyAlias(): OnlyAlias.Deep.Leaf<string>;

/** A class merged with a namespace of types. */
export declare class Queue {
    push(item: Queue.Item): void;
}
export declare namespace Queue {
    interface Item { id: string }
    interface Unread { pending: number }
}

/** A global namespace this module package contributes. */
declare global {
    namespace NestedNameLabGlobal {
        interface Unreached { u: number }
    }
}
