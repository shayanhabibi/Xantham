# Interface bases emitted as classes

The actual Workers → Containers catalog pass now authenticates both native libraries. Its
consumer build exposed two FS0887 errors: ContainerProxy and Container were emitted as
interfaces inheriting the producer's WorkerEntrypoint and DurableObject abstract classes.
The same issue reproduces without a referenced catalog.

The producer declaration is:

```typescript
declare namespace ClassRuntime {
    export abstract class Actor<T = unknown> {
        constructor(seed: T);
        readonly seed: T;
        fetch?(value: T): T;
    }
}
declare module "class-lab:runtime" { export = ClassRuntime; }
```

The consumer declaration is:

```typescript
/// <reference path="./index.d.ts" />
import { Actor } from "class-lab:runtime";
export declare class Explicit<T> extends Actor<T> {
    constructor(seed: T);
    send(value: T): T;
}
export declare class Implicit<T> extends Actor<T> { send(value: T): T; }
export interface Extension<T> extends Actor<T> { label: string; }
export interface Plain<T> { value: T; }
export interface PlainDerived<T> extends Plain<T> { extra: string; }
```

The frozen before tool generates the first three declarations as interfaces inheriting
Actor, and the net8 compile reports exactly three FS0887 errors. The reducer is tracked as
catalog-class-inheritance-lab. The catalog consumer separately authenticates the producer
before checking constructors, inherited members, optional hooks, and ordinary upcasts.

Classes removes interface inheritance edges whose targets its accepted entrypoint map
actually emits as abstract classes. This runs before inherited-member deduplication, so
all inherited members remain available. Existing SI006 records the omitted nominal relation.
The ordinary class constructor exports remain unchanged. The typed test rejects a numeric
seed for a string instance and rejects the nominal interface-to-class upcast; it also
subclasses the ambient Actor directly and implements its optional hook interface.

This repair retains the current ordinary SDK class interface-and-constructor policy.
It does not add F# subclassability to Container. Direct DurableObject subclassability
remains provided by the ambient runtime base.

Focused checks pass in both phases; fresh FCS and independent review report zero errors
and warnings. The full unfiltered gate passes 910 Generator and 90 Wire tests in both
regeneration and independent checking, the compile gate, and 465 Fable runtime checks.
All 107 prior golden trees are byte-identical. The added 67-line fixture has 2 exact,
7 ergonomic, 0 widened and 0 escape symbols, including three existing SI006 findings.
No finding codes or public signatures were added.

The actual native retry succeeds: both catalog generations pass all production guards,
and the unchanged SDKComposition/Native.fs builds against the generated Workers and
Containers assemblies in Release/net8 with zero warnings and errors. The Workers binding
is byte-identical to the prior composed output. Counts remain Workers 421/1057/371/111
and Containers 6/113/74/31 (exact/ergonomic/widened/escape). Copied tool and pinned profile
hashes remain unchanged. The native consumer proves Request → switchPort → Request
through the single Runtime.Workers owner.

Evidence logs: /tmp/xantham-class-inheritance-before-build.log,
/tmp/xantham-class-inheritance-focused.log, /tmp/xantham-class-inheritance-full-gate.log,
and /tmp/xantham-class-inheritance-final-fcs.jsonl. The actual native retry is preserved in
CloudEdge/artifacts/one-shot-20260913/core-catalogs-native-inheritance-repaired with pinned
input provenance, a hashed immutable 50-file tool payload, and normal catalog guards.
