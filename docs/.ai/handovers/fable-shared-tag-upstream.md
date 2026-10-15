# Handover from the Xantham session: shared-tag tagged unions and Fable

You are a Claude session in the Fable repository. This note comes from a Claude session in Xantham, a generator that turns TypeScript `.d.ts` files into F# bindings for Fable. Xantham is at github.com/shayanhabibi/Xantham. The user owns both repositories. **Please read this, then wait for the user's next instruction or prompt before you act. Do not start any work yet.**

Versions in use: the Fable 5.17.2 tool and Fable.Core 5.2.0. Generated bindings target Fable 5.x only.

## 1. The problem: TypeScript unions whose arms share a tag value

Xantham turns a discriminated TS union into `[<TypeScriptTaggedUnion("tag")>]`, with one DU case per tag value. When two arms share a tag value, today it merges them into one case. That case carries only the members every arm declares, which loses data. It cannot build or match the individual arms. The user has ruled that this is wrong: fidelity to the TS source comes first, always.

Example 1, `Terminal` (in `tests/fixtures/shared-tag-lab/index.d.ts` in Xantham):
```ts
export type Terminal =
    | { readonly state: "done"; readonly value: number }
    | { readonly state: "done"; readonly error: string }
    | { readonly state: "pending" };
```
Current F# output: `[<TypeScriptTaggedUnion("state")>] type Terminal = | Done | Pending`. `value` and `error` are lost, and `Terminal.Done` builds `{state:"done"}`, which neither arm accepts.

Example 2, `Task` (in `tests/fixtures/tagged-arm-lab/index.d.ts`):
```ts
export type Task =
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: true; note: { text: string } }
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: false; note: { text: number } }
    | { state: "queued" };
```
Current F# output: one `Done` case that carries only `owner`, which both arms share. `ok` and `note` are lost, and the case builds an object neither arm accepts. Here the arms differ by the literal `ok`.

There are 7 lab cases in all (`Terminal`, `OnEvent.Event`, `Task`, `Run`, `Job`, `Report`, `Outcome`) and one real-world case, Cloudflare workers-types `TailStream` `Log`. Xantham's Core.TS (lib.dom/ES) and Node bindings have none.

## 2. How TypeScript tells the arms apart (every existing case can be told apart)

Within a tag group, TS narrows in this order:
1. **A second literal discriminant.** `Task`: `ok: true` vs `ok: false`. `Log`: `truncated?: false` vs `truncated: true`.
2. **`in` presence.** Each arm has its own required member (`Terminal`, `Run`, `Report`: `'value' in x` vs `'error' in x`). For `Job` and `Outcome`, only one arm has its own member.
3. **`typeof`.** `Log.message` is an object in one arm and a string in the other.

If none of these apply, the faithful fallback is a plain erased `U<n>` of the arm interfaces.

## 3. F# and Fable constraints that block the faithful form

- `TypeScriptTaggedUnion` matches a case only by `x.tag === "<CompiledName>"`. It has no per-case extra test, so two cases cannot share a tag value.
- Xantham's published bindings (Xantham.Fable.Core.TS, Xantham.Fable.Node) are consumed as DLLs, with `<FablePackageType>binding</FablePackageType>` and Fable.Package.SDK 1.4.1. From a DLL, Fable sees attributes and abstract members only, and any member body fails with "Cannot reference member from .dll reference, Fable packages must include F# sources". That rules out generated active patterns, which have bodies. Shipping sources was rejected for compile time: about 321k lines, and the Fable compile went from 22.7s to 6.3s once moved to DLL consumption.
- Complete active patterns allow at most 7 cases.
- Myriad and other build-time generation in the consumer were rejected.

## 4. The candidate upstream fix: per-case discriminant tests in `TypeScriptTaggedUnion`

Let a case add its own test, written as an Emit-style expression. The attribute name `CaseTest` below is made up, so upstream would choose the real shape:
```fsharp
[<TypeScriptTaggedUnion("state")>]
type Terminal =
    | [<CompiledName("done"); CaseTest("'value' in $0")>] Done of value: float
    | [<CompiledName("done"); CaseTest("'error' in $0")>] DoneError of error: string
    | [<CompiledName("pending")>] Pending
```
- Construction already works today: `DoneError "x"` builds `{state:"done", error:"x"}`.
- Matching becomes `x.state === "done" && 'error' in x`.
- One form covers all three TS narrowings: `$0.ok === true`, `'k' in $0`, and `typeof $0.message === "string"`. Xantham computes the expression, and Fable only splices it in.
- The fix uses attributes only, so it works from a DLL.

An alternative is a compound or secondary tag, such as `TypeScriptTaggedUnion("kind", "ok")`. It covers only the literal case, not `in` or `typeof`. The larger alternative, Fable reading inline bodies from DLLs, is unlikely to be accepted.

Not yet known:
- Where Fable generates the case test for `TypeScriptTaggedUnion` (Fable2Babel / the union-case test path), and so how small the patch is.
- Whether upstream wants this attribute shape.

The proposed next step, which the user has not approved, is to read that code at 5.17.2, prototype the attribute in a local Fable build, and check the emitted JS for `Terminal`, `Task` and the `Log` pair consumed from a DLL. Opening any upstream issue or PR needs the user's explicit approval.

## 5. Two other Fable findings, from a Xantham spike (Fable 5.17.2, Fable.Core 5.2.0)

Setup: the consumer ran `dotnet fable . --noCache --exclude <lib projects>` under node.

- **`[<Erase>]` on extension members fails in both modes.**
  - From a DLL, it is a compile error: "Cannot reference member from .dll reference, Fable packages must include F# sources: SpikeLib.Augmentations.eraseProp". A member with no attribute and a `jsNative` body fails the same way.
  - From source, Fable emits a call to `Foreign_Window__Window_get_eraseProp(w)`, imported from `SpikeLib.js`, which never exports it. Node fails with a SyntaxError.
  - What works is per-accessor `[<Emit("$0.x")>]` on the getter and `[<Emit("$0.x = $1")>]` on the setter, and `[<Emit("$0.m($1)")>]` for methods. That includes nested foreign types such as `type NodeJS.ProcessEnv with`.
- **The `[<Global(name)>]` setter on a writable static emits a call.**
  - Given `[<Global("g")>] static member g with get() = jsNative and set (v: float) = jsNative`, the getter emits `g`, which is correct. The setter emits `g(3)`, which throws a TypeError at runtime.
  - `[<Global("h")>] static member h() = jsNative` correctly emits `h()`.
  - The workaround is `[<Erase; Global("globalThis")>] type Globals` with bare static members. That emits `globalThis.g`, `globalThis.g = 4` and `globalThis.h()`, all correct.
  - This looks like a genuine Fable bug and could go upstream.

Spike artefacts are in the Xantham checkout at `C:\Users\shaya\RiderProjects\Xantham\tests\.scratch\pre158\e-global-augmentation\spike\`: the Foreign, SpikeLib and Consumer projects, plus `emit-forms.js`, `erase-from-source.*.js` and `mixed-exports.Program.js`.

## 6. Ground rules the user has set

- Fidelity to the TS source wins. Churn, back-compat and scope are costs to report, never reasons for a decision.
- Pushing, merging, publishing, and opening issues or PRs each need the user's explicit approval.

**Again: please acknowledge this note, then wait for the user's next instruction or prompt before doing anything.**
