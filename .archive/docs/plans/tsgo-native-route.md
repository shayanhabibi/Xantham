# tsgo native / in-process route — feasibility report

> **Packaging note (2026-08-30).** Written when the Go compiler shipped as the separate
> `@typescript/native-preview` preview package; it now ships from `microsoft/TypeScript` as
> `typescript`, with the executable named `tsc`. This does not change the conclusion - the argument
> against the c-shared DLL route was cgo call overhead and the cost of maintaining a fork, and
> moving the source into the main TypeScript repository makes a fork no cheaper. Read `tsgo.exe`
> below as `tsc.exe`.

> **Source-of-truth note (2026-08-31).** `microsoft/typescript-go` is archived and its local
> checkout has been deleted. Do not clone or cite it. All compiler research goes against
> `microsoft/TypeScript@main` or the installed `typescript` 7.x npm package - see the
> "TypeScript 7 compiler sources" section of `AGENTS.md` for the name and path translations.

Investigation of calling the TypeScript-Go compiler (`microsoft/typescript-go`) from .NET/F#
**without** the shipped IPC protocol (msgpack over pipe to a spawned `tsgo.exe`).

Two alternatives assessed:

- **Option B — Go glue layer**: fork/vendor typescript-go, add a Go package built with
  `go build -buildmode=c-shared` exposing a flat C ABI, called from .NET via `DllImport` /
  `NativeLibrary`.
- **Option C — direct DLL**: does upstream already build (or trivially permit building) a shared
  library, WASM target, or anything else that avoids a hand-written glue layer?

All artifacts live in `C:\Users\shaya\RiderProjects\Xantham\tmp\tsgo-native\`.

**Bottom line up front: the c-shared spike works, end to end, including the type checker. But the
recommendation is still IPC, for two independent reasons:**

1. **IPC already ships a bulk binary AST transfer** (`internal/api/encoder`, a 28-byte-per-node flat
   blob returned whole by `getSourceFile`), so exhaustive AST traversal costs one message per *file*,
   not one per *node*. The chattiness premise does not hold. (§5.1)
2. **Per-call FFI is not cheap anyway: 828.8 ns for a no-op Go export, against 1.3 ns for the same
   call into a plain C DLL.** Per-node FFI measured ~9x *slower* than transferring the whole AST as
   one blob. The native route does not make node-by-node access affordable either. (§5.4)

Both routes therefore want the same design — bulk-transfer the AST, traverse locally — at which
point the glue layer buys a `memcpy` instead of a pipe write, in exchange for forking an archived
repo and coupling to `internal/` APIs. See [§5](#5-costbenefit-vs-ipc).

---

## 0. Environment and upstream revision

| Item | Value |
| --- | --- |
| Go | `go version go1.26.3 windows/amd64` |
| C toolchain | `gcc.exe (MinGW-W64 x86_64-posix-seh, built by Brecht Sanders) 11.1.0` |
| Upstream commit | `89d5d5b2849a0db0957065889ca58536fa6d2e4a`, Thu Aug 20 06:46:10 2026 +0000 |
| Commit subject | `Add closure notice and link to original repo (#4919)` |
| Module | `module github.com/microsoft/typescript-go`, `go 1.26` |
| Reported version | `7.1.0-dev` (from `internal/core.Version()`, printed by the spike) |

### The repo is closed and is being archived

`typescript-go/README.md:1`:

```
# This Repo Is Closed

This was staging repo for TypeScript 7.0 release during native port process, now completed!
Please continue development discussion in the original repo
This repo will be permanently archived in September 2026.
```

Today is 2026-08-29. **The fork base is archived next month** and development has moved back to
`microsoft/TypeScript`. This is load-bearing for the maintenance estimate in §5: a fork taken today
is a fork of a repo that stops receiving commits, and the live source moves to a repository whose
Go layout is UNVERIFIED (not inspected in this investigation).

---

## 1. Upstream Go API surface

### 1.1 Fork-vs-import verdict: **fork/vendor is mandatory**

Counted package directories containing `.go` files (excluding `_`-prefixed dirs and `testdata`):

```
$ find . -name '*.go' -not -path './_*' | sed 's|/[^/]*$||' | sort -u | grep -v testdata | wc -l
105

$ find . -name '*.go' -not -path './_*' | sed 's|/[^/]*$||' | sort -u | grep -v '/internal/' | grep -v '^./internal'
./cmd/tsgo
```

**105 package directories; exactly one is outside `internal/`, and it is the `main` package for the
CLI.** The parser, AST, binder, checker, compiler, tsoptions, vfs, and the API/encoder layer are
*all* under `internal/`, which Go's import rules make unimportable from any module other than
`github.com/microsoft/typescript-go` itself.

Consequences, all of them hard:

- `go get github.com/microsoft/typescript-go/internal/checker` from your own module **cannot
  compile**. There is no supported external Go consumer story at all.
- Option B is therefore not "add a package that imports typescript-go". It is **"fork the repo and
  add a package inside it"** — which is exactly what the spike does (`capi/` lives *inside* the
  cloned tree, at `<clone>/capi/`, so it is part of the same module and may import
  `internal/...`).
- Rebasing your fork onto upstream forever is the maintenance model. There is no version-pinning
  escape hatch, because there is no importable API to pin.

### 1.2 (a) Creating a program/project from a tsconfig or file list

- `internal/compiler/program.go:37` — `type ProgramOptions struct { Host CompilerHost; Config
  *tsoptions.ParsedCommandLine; UseSourceOfProjectReference bool; SingleThreaded core.Tristate;
  CreateCheckerPool func(*Program) CheckerPool; TypingsLocation string; ProjectName string;
  Tracing *tracing.Tracing }`
- `internal/compiler/program.go:281` — `func NewProgram(opts ProgramOptions) *Program`
- `internal/tsoptions/tsconfigparsing.go:2058` — `func GetParsedCommandLineOfConfigFile(configFileName
  string, options *core.CompilerOptions, optionsRaw *collections.OrderedMap[string, any], sys
  ParseConfigHost, extendedConfigCache ExtendedConfigCache) (*ParsedCommandLine, []*ast.Diagnostic)`
- `internal/tsoptions/tsconfigparsing.go:715` — `type ParseConfigHost interface { FS() vfs.FS;
  GetCurrentDirectory() string }` — a two-method interface, trivial to satisfy (the spike's
  `simpleHost` is 6 lines).
- `internal/compiler/host.go:44` — `func NewCachedFSCompilerHost(currentDirectory string, fs vfs.FS,
  defaultLibraryPath string, extendedConfigCache tsoptions.ExtendedConfigCache, trace func(msg
  *diagnostics.Message, args ...any), contentMapperProject contentmapper.Project) CompilerHost`
  (`:55` is the uncached `NewCompilerHost` with the same signature).
- `internal/parser/parser.go:135` — `func ParseSourceFile(opts ast.SourceFileParseOptions, sourceText
  string, scriptKind core.ScriptKind) *ast.SourceFile` for the single-file, no-project path.
- `internal/ast/parseoptions.go:8` — `type SourceFileParseOptions struct { FileName string; Path
  tspath.Path; ExternalModuleIndicatorOptions ExternalModuleIndicatorOptions }`.

The reference call sequence is `internal/execute/tsc.go:295-320` (build a host, read build info,
`compiler.NewProgram`). The spike follows it.

**`lib.d.ts` is embedded in the binary by default.** `internal/bundled/embed.go:1` is
`//go:build !noembed` and `:13` is `const embedded = true`; `internal/bundled/libs` is 4.0 MB.
`bundled.WrapFS(osvfs.FS())` overlays them and `bundled.LibPath()` names the virtual directory. This
means **no lib files need to be shipped alongside the DLL** — verified, the spike resolves
`Math.sqrt` and `number` with nothing on disk but the fixture.

### 1.3 (b) Diagnostics

On `*compiler.Program` (`internal/compiler/program.go`):

| Line | Function |
| --- | --- |
| `:512` | `GetConfigFileParsingDiagnostics() []*ast.Diagnostic` |
| `:719` | `GetSyntacticDiagnostics(ctx, sourceFile) []*ast.Diagnostic` |
| `:763` | `GetBindDiagnostics(ctx, sourceFile) []*ast.Diagnostic` |
| `:774` | `GetSemanticDiagnostics(ctx, sourceFile) []*ast.Diagnostic` |
| `:778` | `GetSemanticDiagnosticsWithoutNoEmitFiltering(ctx, sourceFiles) map[*ast.SourceFile][]*ast.Diagnostic` |
| `:787` | `GetSuggestionDiagnostics(ctx, sourceFile) []*ast.Diagnostic` |
| `:791` | `GetProgramDiagnostics() []*ast.Diagnostic` |
| `:1423` | `GetGlobalDiagnostics(ctx) []*ast.Diagnostic` |
| `:1435` | `GetDeclarationDiagnostics(ctx, sourceFile) []*ast.Diagnostic` |

`*ast.Diagnostic` accessors are at `internal/ast/diagnostic.go:58-74`: `File()`, `Pos()`, `End()`,
`Len()`, `Loc()`, `Code() int32`, `Category()`, `Source()`, `MessageText()`, `MessageKey()`,
`MessageArgs()`, `MessageChain()`, `RelatedInformation()`, `ReportsUnnecessary()`,
`ReportsDeprecated()`.

**Gotcha found the hard way:** `MessageText()` returns `""` for ordinary compiler diagnostics. It is
only populated for *external* diagnostics (`internal/ast/diagnostic.go:47` comment: "already-localized
message used when message is nil"). The renderable text comes from
`internal/ast/diagnostic.go:117` — `func (d *Diagnostic) Localize(locale locale.Locale) string`,
which formats `messageKey` + `messageArgs`. The spike initially printed empty strings because of
this; switching to `Localize(locale.Default)` produced `Variable declaration expected.` etc.

### 1.4 (c) Walking the AST

- `internal/ast/ast.go` — `Node`, `SourceFile`, `NodeList`. `SourceFile.Statements.Nodes` is the
  top-level statement slice; `Node.ForEachChild(func(*Node) bool)` is the visitor (used by the
  spike's `findNodeAt`); `Node.Pos()`, `Node.End()`, `Node.Kind`.
- `internal/ast/ast.go:2726` — `func (node *SourceFile) Diagnostics() []*Diagnostic`.
- `internal/astnav` — position→node navigation helpers.

### 1.5 (d) Querying the checker

`internal/checker` is 27 files with **161 exported `*Checker` methods and 1836 unexported ones**:

```
$ grep -h "^func (c \*Checker) [A-Z]" *.go | wc -l
161
$ grep -h "^func (c \*Checker) [a-z]" *.go | wc -l
1836
```

Obtaining a checker (`internal/compiler/program.go`):

- `:570` `GetTypeChecker(ctx) (*checker.Checker, func())`
- `:587` `GetTypeCheckerForFile(ctx, file) (*checker.Checker, func())`
- `:596` `GetTypeCheckerForFileExclusive(ctx, file) (*checker.Checker, func())`

All three return a **release function that must be called** — checkers come from a pool
(`internal/compiler/checkerpool.go`).

Representative exported methods (from `checker/checker.go` and `checker/printer.go`):
`GetSymbolAtLocation`, `GetTypeAtLocation`, `GetTypeOfSymbolAtLocation`, `GetAliasedSymbol`,
`ResolveAlias`, `GetAmbientModules`, `GetDiagnostics`, `GetSuggestionDiagnostics`,
`GetNonNullableType`, `GetPromisedTypeOfPromise`, `GetTypeAliasTypeParameters`, `UnionTypes`,
`IsDeprecatedDeclaration`, `IsNullableType`, `TypeToString` (`checker/printer.go:43`),
`TypeToStringEx` (`:55`).

The exported set is wide enough for binding generation: the calls the shipped IPC session itself
makes (from `internal/api/session.go`) are `GetSymbolAtLocation`, `GetTypeAtLocation`,
`GetTypeOfSymbol`, `GetTypeOfSymbolAtLocation`, `GetTypeFromTypeNode`, `GetTypeArguments`,
`GetSymbolsInScope`, `ResolveName`, `IsTypeAssignableTo`, `IsTupleType`, `IsArrayType`,
`IsArrayLikeType`, `GetWidenedType`, `GetTypePredicateOfSignature`, `GetTypeParameterAtPosition`,
`TryGetMemberInModuleExports`, `GetUnknownType/Symbol/Signature`, `GetUndefinedType/Symbol`,
`GetVoidType`, `GetTrueTypeOfConditionalType`, `IsContextSensitive` — i.e. the in-process surface is
a **superset** of what IPC exposes, since IPC can only offer what someone wrote a protocol method for.

---

## 2. Option C — is there an existing shared-library or WASM target?

**No.** Findings:

- The only non-`main`-adjacent build target is `./cmd/tsgo` (§1.1), an executable.
- No `//export` directives, no `-buildmode=c-shared` invocation, and no `import "C"` exist anywhere
  in the upstream tree outside of what this spike added. (Established by the same `find`/`grep`
  sweep that produced the package census.)
- The npm distribution `@typescript/native-preview` ships the `tsgo` **executable**, and the VS Code
  integration drives it over the API/LSP protocols — i.e. upstream's own first-party consumers use
  IPC, not linking.
- No WASI/`GOOS=wasip1` build target was found in the repo. UNVERIFIED whether one *would* build; see
  §6.

So Option C does not exist as such. **Any native route is Option B**: a hand-written glue layer
inside a fork.

---

## 3. The c-shared spike — it works

### 3.1 What was built

A throwaway Go package at `capi/` (two files, ~260 lines total) inside the cloned
fork, plus a C driver that loads the resulting DLL with `LoadLibraryA` + `GetProcAddress` — i.e.
exactly the mechanism `NativeLibrary.Load` / `NativeLibrary.GetExport` uses from .NET. A C driver was
used deliberately instead of an F# project, per the brief.

| File | Role |
| --- | --- |
| `tmp/tsgo-native/capi/capi.go` | version, parse, diagnostics, AST blob, handle table, panic barrier |
| `tmp/tsgo-native/capi/program.go` | tsconfig → `compiler.Program` → semantic diagnostics → checker query |
| `tmp/tsgo-native/driver.c` | C harness exercising every export |
| `tmp/tsgo-native/fixture/` | `tsconfig.json` + `src/main.ts` with one deliberate type error |
| `tmp/tsgo-native/tsgo_capi.dll` / `.h` | build output (~36 MB) |

### 3.2 Exact commands that worked

Reproducing from a clean checkout:

```bash
# 1. clone (shallow is fine; the tree is ~55k files)
#    HISTORICAL: this was `microsoft/typescript-go` @ 89d5d5b. That repo is archived;
#    the Go compiler now lives under `tsc/` in the main TypeScript repository.
cd /c/Users/shaya/RiderProjects/Xantham/tmp/tsgo-native
git clone --depth 1 --filter=blob:none https://github.com/microsoft/TypeScript.git

# 2. add the glue package INSIDE the module (this is the fork requirement)
#    The module is now `github.com/microsoft/TypeScript/tsc`, rooted at TypeScript/tsc.
mkdir -p TypeScript/tsc/capi
#   ... place capi.go and program.go there, rewriting their imports from
#       github.com/microsoft/typescript-go/internal/... to
#       github.com/microsoft/TypeScript/tsc/internal/...

# 3. build the shared library  <-- THE COMMAND THAT WORKS
cd TypeScript/tsc
CGO_ENABLED=1 go build -buildmode=c-shared -o ../../tsgo_capi.dll ./capi

# 4. build and run the C driver (from tmp/tsgo-native, next to the DLL)
cd ..
gcc driver.c -o driver.exe
./driver.exe
```

Requirements: `CGO_ENABLED=1` and a working `gcc` on `PATH` (MinGW-W64 here). No other flags, no
build tags, no patches to upstream source — **the fork is purely additive**: not one upstream file
was modified.

Build time on this machine: well under the 10-minute timeout, including dependency download
(`xxh3`, `golang.org/x/sync`, `x/text`, `go-json-experiment/json`, `klauspost/cpuid`, `x/sys`,
`Microsoft/go-winio`).

### 3.3 Generated header

`go build -buildmode=c-shared` emits `tsgo_capi.h` alongside the DLL, containing plain
`extern "C"`-guarded declarations — directly usable as the source of truth for `DllImport`
signatures:

```c
extern char* tsgo_version(void);
extern void tsgo_free_string(char* s);
extern uint64_t tsgo_parse(char* fileName, char* text);
extern int32_t tsgo_diagnostic_count(uint64_t h);
extern char* tsgo_diagnostic_message(uint64_t h, int32_t i);
extern int32_t tsgo_statement_count(uint64_t h);
extern void* tsgo_encode_ast(uint64_t h, int32_t* outLen);
extern void tsgo_free_buffer(void* p);
extern void tsgo_release(uint64_t h);
```

(plus `tsgo_last_error`, `tsgo_open_project`, `tsgo_project_diagnostic_count`,
`tsgo_project_diagnostic_message`, `tsgo_project_type_at` added later.)

All parameters are C scalars and `char*`/`void*`. Nothing in the ABI is Go-shaped.

### 3.4 What the exports do

| Export | Behaviour |
| --- | --- |
| `tsgo_version` | returns `internal/core.Version()`; caller frees with `tsgo_free_string` |
| `tsgo_parse(fileName, text)` | `parser.ParseSourceFile` with `core.ScriptKindTS`; stores the `*ast.SourceFile` in a mutex-guarded handle table; returns a `uint64` handle (0 = failure) |
| `tsgo_diagnostic_count(h)` | `len(sf.Diagnostics())`, `-1` for a bad handle |
| `tsgo_diagnostic_message(h, i)` | `d.Localize(locale.Default)` as a fresh `C.CString` |
| `tsgo_statement_count(h)` | `len(sf.Statements.Nodes)` |
| `tsgo_encode_ast(h, *outLen)` | `encoder.EncodeSourceFile(sf)` → `C.malloc` + copy; **the identical binary AST blob the IPC protocol ships for `getSourceFile`** |
| `tsgo_free_buffer` / `tsgo_free_string` / `tsgo_release` | explicit deallocation; `tsgo_release` drops the handle-table entry |
| `tsgo_last_error` | message of the most recent recovered panic, or NULL |
| `tsgo_open_project(configPath, cwd)` | `GetParsedCommandLineOfConfigFile` → `NewCachedFSCompilerHost` → `compiler.NewProgram`, then eagerly collects syntactic + semantic diagnostics for every non-`lib.` source file |
| `tsgo_project_diagnostic_count/_message` | as above, for the program |
| `tsgo_project_type_at(h, fileName, pos)` | finds the innermost node at `pos` via `Node.ForEachChild`, calls `GetTypeCheckerForFile`, `GetTypeAtLocation`, `TypeToString`; releases the checker via the returned `done()` |

Go packages wrapped: `internal/parser`, `internal/ast`, `internal/core`, `internal/locale`,
`internal/tspath`, `internal/api/encoder`, `internal/compiler`, `internal/tsoptions`,
`internal/bundled`, `internal/vfs`, `internal/vfs/osvfs`.

### 3.5 Verified output

`./driver.exe`, exit code 0:

```
=== 1. version ===
tsgo version: 7.1.0-dev
=== 2. parse + AST + diagnostics ===
good.ts  handle=1 statements=2 diagnostics=0
encoded AST blob (same format as IPC getSourceFile): 1155 bytes (ok)
bad.ts   diagnostics=4
   [0] Variable declaration expected.
   [1] Identifier expected.
   [2] Parameter declaration expected.
=== 3. panic recover barrier ===
recovered: handle=0 lastError=fileName should be normalized and absolute: "relative.ts"
=== 4. full Program from tsconfig + checker ===
program semantic+syntactic diagnostics=1
   [0] Type 'string' is not assignable to type 'number'.
checker TypeToString at main.ts pos 20 = Point
OK
```

This is the whole compiler, in-process, from C:

- real parse with real diagnostics,
- the flat binary AST encoder,
- a full `Program` built from a real `tsconfig.json` with embedded `lib.d.ts`,
- a genuine **semantic** error (`Type 'string' is not assignable to type 'number'` — from
  `export const oops: number = "not a number";`),
- and a **type checker** query returning `Point`.

**Option B is mechanically proven on Windows/amd64.** The feasibility question is settled; what
remains is whether it is worth it (§5).

### 3.6 Distribution shape — no MinGW runtime to ship

The DLL is ~36 MB (Go runtime + whole compiler + 4 MB of embedded `lib.d.ts`).

A naive string scan of the binary turns up `libgcc_s_dw2-1.dll`, which looks alarming. **It is a red
herring** — it is inert data, not a binding. Parsing the actual PE import directory
(`tmp/tsgo-native/imports.txt`):

```
tsgo_capi.dll        -> ['KERNEL32.dll', 'msvcrt.dll']
tsgo_capi_static.dll -> ['KERNEL32.dll', 'msvcrt.dll']
```

**The DLL imports only `KERNEL32.dll` and `msvcrt.dll`** — both present on every Windows install.
There is no MinGW runtime, no `libgcc`, no `libwinpthread` to redistribute. Building with
`-ldflags="-extldflags=-static"` changes nothing (byte-identical size, 36482020), which is consistent
with there being nothing to statically link in the first place. Good news for packaging: the native
asset is a single self-contained file.

---

## 4. Threading / GC / ABI hazards

The following are the real constraints on a Go c-shared library called from the CLR. Each is either
verified here or flagged.

### Hazard 1 — a Go panic unwinding into C kills the process (VERIFIED, observed)

Before a `recover()` barrier was added, calling `tsgo_parse("good.ts", ...)` with a relative path
produced:

```
panic: fileName should be normalized and absolute: "good.ts"
goroutine 17 [running, locked to thread]:
github.com/microsoft/typescript-go/internal/ast.(*NodeFactory).NewSourceFile(...)
        .../internal/ast/ast.go:2541 +0x254
github.com/microsoft/typescript-go/internal/parser.(*Parser).parseSourceFileWorker(...)
        .../internal/parser/parser.go:449 +0x2e8
main.tsgo_parse(...)
        .../capi/capi.go:63 +0x54
```

The **entire host process died** (`EXIT=2`). No SEH exception, no return code, nothing the CLR could
catch — the process simply terminated. This is the single most dangerous property of the design,
and it is aggravated by typescript-go's style: `panic` is used liberally for invariant violations
(`ast.go:2541` panics on a non-absolute filename, `bundled/embed.go:157,164,171,178` panic on writes
to the embedded FS, `api/server.go:51` panics on a missing `Cwd`). A parser bug or an unexpected
input becomes a hard crash of the .NET process.

**Mitigation (verified to work):** every single exported function needs its own
`defer func(){ if r := recover(); r != nil { ... } }()` and a named return value:

```go
//export tsgo_parse
func tsgo_parse(fileName *C.char, text *C.char) (result C.uint64_t) {
	defer func() {
		if r := recover(); r != nil { setLastError(r); result = 0 }
	}()
	...
}
```

After this change the same call returns `handle=0` with
`lastError=fileName should be normalized and absolute: "relative.ts"` and the process survives.
Note this is **per-export boilerplate that cannot be centralised** and that a reviewer must never
forget — one unguarded export is a latent process-killer. Also note `recover()` does **not** catch
runtime fatals: concurrent map writes, stack exhaustion, and OOM are unrecoverable and will still
take the process down. UNVERIFIED whether any typescript-go code path can trigger those under
adversarial input.

### Hazard 2 — cgo pointer-passing rules: no Go pointer may be stored on the C side (VERIFIED by design)

Go's cgo pointer rules forbid C code from retaining a Go pointer after the call returns, and the Go
GC is free to move stacks. A `*ast.SourceFile` therefore **cannot** be handed to .NET as an opaque
`IntPtr`.

The spike's answer, which is the correct general shape:

```go
var (
	mu      sync.Mutex
	nextID  uint64
	handles = map[uint64]*ast.SourceFile{}
)
```

**A handle table keyed by `uint64`.** The integer crosses the boundary; the pointer never does. The
Go value stays reachable from a package-level map (so the GC keeps it alive), and `tsgo_release(h)`
deletes the entry. This is non-negotiable, and it means **the .NET side owns object lifetime
explicitly** — a leaked handle is a leaked `Program` with its whole type graph, which for a
TypeScript program is hundreds of MB.

On GC movement specifically: the Go heap is currently non-moving for heap objects, but *stacks*
move, and the rules are deliberately written so you may not depend on the current behaviour. Treat
"Go GC may move memory the CLR holds" as **true and unfixable**; never let the CLR hold Go memory.

### Hazard 3 — memory ownership across the boundary (VERIFIED)

Every buffer returned to C is allocated with `C.CString` / `C.malloc` and **copied**:

```go
*outLen = C.int32_t(len(data))
buf := C.malloc(C.size_t(len(data)))
copy(unsafe.Slice((*byte)(buf), len(data)), data)
return buf
```

So the ownership rule is: **Go allocates with the C allocator, .NET frees by calling back into
`tsgo_free_string` / `tsgo_free_buffer`.** .NET must *not* use `Marshal.FreeHGlobal` — it is a
different allocator, and the Go side's `C.free` is the matching one. Every string-returning export
is a `try/finally` on the .NET side or it leaks. `Marshal.PtrToStringUTF8` is the correct read (Go
strings are UTF-8; .NET strings are UTF-16, so **every string crossing the boundary is transcoded**,
which is pure per-call cost).

An important corollary for the chatty-AST workload: you cannot return a Go string as a borrowed
view. Each of the *millions* of identifier names in a large program would be a `malloc` + copy +
UTF-16 transcode + a second P/Invoke to free. This is a strong argument for the blob approach
(§5.1) even inside the FFI design.

### Hazard 4 — threading and goroutine affinity (PARTIALLY VERIFIED)

- Calls from .NET enter Go on whatever OS thread the CLR happens to be on; the Go runtime attaches
  it (`goroutine 17 [running, locked to thread]` in the panic trace above is exactly that — an
  attached, locked thread). This works, but thread attachment is not free (see §5 benchmark).
- The Go runtime **starts its own threads** (GC workers, sysmon, and typescript-go's own checker
  pool, `internal/compiler/checkerpool.go`). These are invisible to the CLR. A .NET debugger, a
  profiler, and any thread-enumeration logic will see them.
- `*checker.Checker` is explicitly **not** free-threaded — hence `GetTypeCheckerForFileExclusive`
  and the mandatory `done()` release function. Concurrent .NET callers must not share a checker.
  The handle table's `sync.Mutex` protects the table, **not** the objects in it. UNVERIFIED: whether
  `Program`'s diagnostic methods are safe to call concurrently from multiple attached threads;
  `ctx context.Context` on every signature suggests a cancellation story exists, but nothing here
  tested parallel entry.
- Cancellation: the Go API is `context.Context`-based. Bridging a .NET `CancellationToken` requires
  registering a callback that flips a Go-side context — meaning either a callback into .NET, or a
  polled flag. Not attempted.
- Callbacks Go→.NET are possible (`internal/api/callbackfs.go` proves upstream already models a
  client-provided filesystem) but require `//export`ed trampolines and a function-pointer table, and
  every such callback can re-enter the Go runtime on an arbitrary thread. UNVERIFIED and expensive;
  avoid by using `osvfs` in-process.
- `DllMain`/loader-lock: `buildmode=c-shared` initialises the Go runtime on load. Do not `LoadLibrary`
  it from a constrained context. UNVERIFIED for the .NET default `AssemblyLoadContext` unload path —
  **the Go runtime cannot be shut down or re-initialised**, so a collectible ALC that unloads and
  reloads the native library is likely to crash. This matters for `dotnet watch` / plugin scenarios.

### The safe ABI shape, stated plainly

Everything above collapses into five rules, all of which the spike follows:

1. **Handle table + explicit free.** Opaque `uint64`, never a pointer. `tsgo_release` mandatory.
2. **All strings copied**, `C.CString` out / `tsgo_free_string` back; UTF-8 on the wire.
3. **No Go pointer escapes** — cgo rules, non-negotiable.
4. **`recover()` in every single export**, with a named return and an out-of-band
   `tsgo_last_error`. Errors are return codes, never exceptions.
5. **Bulk transfers as one `malloc`'d blob**, not per-item calls (`tsgo_encode_ast` is the model).

---

## 5. Cost/benefit vs IPC

### 5.1 The decisive finding: IPC already does bulk binary AST transfer

The brief asks whether per-node traversal makes FFI worth it. **The premise does not hold, because
the IPC protocol does not do per-node round trips for AST access.**

`internal/api/encoder/encoder.go` is a **flat, columnar, whole-file AST encoder**:

```go
const (
	NodeOffsetKind = iota * 4
	NodeOffsetPos
	NodeOffsetEnd
	NodeOffsetNext
	NodeOffsetParent
	NodeOffsetData
	NodeOffsetFlags
	// NodeSize is the number of bytes that represents a single node in the encoded format.
	NodeSize
)
```

Seven `uint32` fields — **28 bytes per node**, in one contiguous buffer, with a separate string table
(`encoder/stringtable.go`), a header carrying string offsets/extended data/structured data offsets,
and 2-bit tagging (`NodeDataTypeChildren` / `NodeDataTypeString` / `NodeDataTypeExtendedData`) packed
into the data word. Node kinds are asserted to fit in 6 bits.

And it is wired straight into the protocol — `internal/api/session.go:1375`:

```go
data, _, err := encoder.EncodeSourceFile(sourceFile)
```

reached from `MethodGetSourceFile Method = "getSourceFile"` (`internal/api/proto.go:85`). There are
also `encoder.EncodeNode` at `session.go:2525,2565` and `encoder.DecodeNodes` at `:2612,2783` for
sub-trees, and `encoder.GetNodeIndexTable(sourceFile)` at `:109,3488`.

**So `getSourceFile` returns the entire AST of a file as a single binary blob in one round trip.**
The consumer decodes it locally and traverses it in-process at memory speed, with zero further
boundary crossings per node. Exhaustive AST traversal to generate F# bindings — the stated workload
— costs **one IPC message per file**, not one per node.

The spike confirms the same encoder is what the native route would expose: `tsgo_encode_ast` produced
a **1155-byte** blob for a 2-statement file, byte-identical in format to what the pipe carries.

In other words, **for the AST half of the workload, the native route and the IPC route transfer
exactly the same bytes through the same encoder.** The only difference is a pipe write versus a
`memcpy` — and the pipe cost is amortised over the whole file.

The protocol is further hardened against chattiness: alongside the singular methods there are
explicit **batch** variants (`internal/api/proto.go`), including `GetSymbolsAtPositions`,
`GetTypesAtPositions`, `GetSymbolsAtLocations`, `GetSymbolsOfSourceFiles`, `GetTypesOfSymbols`,
`GetTypeAtLocations`, `GetSourceFileNames`. Someone upstream already fought the round-trip battle
and shipped plural forms specifically to amortise it. That is exactly the escape hatch a binding
generator needs for the *checker* half: collect every position you care about from the locally
decoded AST, then ask for all their types in one message.

The residual risk is checker queries whose *inputs depend on previous checker answers* (walking a
type graph: type → its type arguments → their symbols → their declarations). Those cannot be batched
ahead of time and do degrade to a round trip apiece. **This is the one scenario where FFI wins**,
and it is worth measuring against the real binding generator before committing.

### 5.2 Effort, maintenance, distribution

**(a) Initial effort.** The spike — a working DLL exposing parse, diagnostics, a full tsconfig
program, and a checker query — took a few hours including discovering the `internal/` constraint,
the `Localize` gotcha, and the panic hazard. That is genuinely encouraging. **But the spike is ~260
lines exposing 14 functions.** A binding generator needs a meaningful slice of those 161 exported
checker methods, plus symbol/type/signature handle lifetimes, plus flags enums mirrored into F#.
Each one needs: a C-ABI wrapper, a `recover()` barrier, handle-table entries for any returned
`*Type`/`*Symbol`/`*Signature`, a free path, and a .NET `DllImport` with correct marshalling. Realistic
estimate: **2-4 weeks** to a usable surface, most of it mechanical glue rather than hard problems.
Against that, speaking the IPC protocol needs a msgpack codec and a pipe client — the other agent's
work — and gets all ~130 protocol methods at once, because they are already defined.

**(b) Maintenance.** This is where the native route hurts, and §0 makes it worse than the usual fork
story:

- You must fork, because `internal/` forbids importing (§1.1). There is no pinning alternative.
- The fork base is **archived in September 2026**; development returns to `microsoft/TypeScript`,
  whose Go layout you have not inspected. Your rebase target moves out from under you.
- You are coupling to **unexported-by-intent internals**. `internal/` is Go's way of saying "no
  compatibility promise, at all". The IPC protocol is called "unstable", but it is at least a
  *declared* surface with named methods and a msgpack schema — breaking changes are visible as
  method/shape changes. Internal Go API churn is invisible until your fork fails to compile, and
  worse, until it compiles but behaves differently.
- Every upstream refactor of `checker` (1836 unexported methods being moved around freely) can break
  glue that reached past the 161 exported ones.

**(c) Build and distribution.** Substantially heavier than IPC:

- A cgo cross-compilation matrix: `win-x64`, `linux-x64`, `linux-arm64`, `osx-x64`, `osx-arm64`.
  cgo means **you cannot cross-compile from one machine without cross-toolchains** — realistically a
  CI matrix with a native runner per RID. Every one needs Go + a C toolchain.
- Per-RID NuGet packages with `runtimes/{rid}/native/`, plus a meta-package. ~36 MB per RID
  (verified), so ~180 MB across five RIDs.
- Against which: the IPC route ships **no native asset of your own at all**. `tsgo` comes from
  `@typescript/native-preview` on npm, which already solves the per-platform binary problem,
  upstream, for free.
- One genuine plus for native (§3.6): the DLL imports only `KERNEL32.dll` and `msvcrt.dll`, and
  `lib.d.ts` is embedded, so the Windows asset is a single self-contained file with no runtime
  prerequisites.

### 5.3 Verdict

**Not worth it. Speak the IPC protocol.** The argument that would have justified the glue layer —
per-node chattiness — fails twice over. Upstream answers it with `api/encoder`'s whole-file binary
AST blob plus the batch query methods (§5.1); and even if it had not, FFI's own per-call cost of
828.8 ns (§5.4) means per-node access is unaffordable over FFI too, ~9x worse than one bulk blob.
You would take on a fork of an archived repo, a coupling to `internal/` APIs with no compatibility
promise, a five-RID cgo CI matrix, and a class of failure (Hazard 1) where a compiler assertion kills
the .NET process — in exchange for a `memcpy` instead of a pipe write, on data that is already
batched, under a traversal design you would have to adopt on either transport.

**The decision flips if, and only if:** profiling the real binding generator shows the *checker*
half (not the AST half) dominating, in a **pointer-chasing pattern that cannot be batched** —
type→argument→symbol→declaration walks where each query's input is the previous query's output. Only
there does 829 ns beat a pipe round trip by a real multiple. If that is >10^5 dependent round trips
per run, remeasure. Note the bar is higher than it first looked: the honest comparison is 829 ns
against a local pipe round trip, not against zero.

A pragmatic hedge: **build against IPC first, but keep the seam narrow** — a single interface with
`GetSourceFileBlob(path) -> byte[]` and `BatchQuery(requests) -> responses`. Both routes implement
it. The spike in `tmp/tsgo-native/` proves the native implementation is available if the profile ever
demands it, and it is preserved for exactly that reason.

### 5.4 FFI call-overhead benchmark — the surprise

Measured with `bench.c` / `ctrl.c` (`gcc -O2`, `QueryPerformanceCounter`, warm-up loop discarded,
2M and 500K iterations). **These numbers changed the shape of the argument.**

| Call | ns/call | Notes |
| --- | --- | --- |
| `c_noop` in a plain **C** DLL (control) | **1.3** | same `GetProcAddress` fn-pointer loop shape |
| `tsgo_noop` in the **Go c-shared** DLL | **828.8** | `func tsgo_noop(x C.int32_t) C.int32_t { return x + 1 }` |
| `tsgo_echo_string` + `tsgo_free_string` | **1773.1** | returns an 18-char owned string; 2 crossings |

The control is the important half: an identical loop calling an identical trivial function through an
identical function pointer into a *C* DLL costs **1.3 ns**. So the ~827 ns delta is **not**
measurement overhead, not the indirect call, and not the loop — **it is the Go runtime boundary
itself**. A c-shared library entered from a foreign (non-Go-created) OS thread pays Go runtime
entry/exit per call, and that is precisely the situation a .NET caller is in.

This is roughly **640x more expensive than a C call** and about an order of magnitude worse than the
~50-80 ns figure cgo is usually quoted at. UNVERIFIED why it is this high on this configuration
(Windows/amd64, Go 1.26.3, MinGW); candidates are per-call `needm`/`dropm` M-attachment and Windows
TLS handling. It was measured consistently across 2M warm iterations, so it is not a cold-start
artifact. **UNVERIFIED from .NET specifically** — the brief forbade F# project code, so this is C. A
.NET `DllImport` will be *this plus* P/Invoke marshalling; blittable-only signatures add little, and
`SuppressGCTransition` is not safely applicable to a call that can block or run Go GC.

**Bulk transfer, for contrast.** The same DLL, encoding a 110,670-byte / 2,000-interface source file:

```
source bytes       : 110670
encode_ast (bulk)  :   2745.5 us for 830952 bytes (~29676 nodes @28B)
```

**One call** moves a 29,676-node AST in 2.75 ms — about **92 ns per node**, fully inclusive of
encoding and the `malloc`+copy.

Now put those together, for the same file:

| Strategy | Cost for 29,676 nodes |
| --- | --- |
| One bulk `tsgo_encode_ast` call | **2.7 ms** |
| One FFI call per node (829 ns each) | **24.6 ms** |

**Per-node FFI is ~9x slower than shipping the entire AST as one blob** — and that is comparing
against the blob *including* its encode cost, while the per-node figure is a bare no-op that does no
actual work. At 1.21M FFI calls/sec, a million-node traversal costs **0.83 s in pure boundary
overhead**, and ~3.5 s if each node needs a string back (two crossings at 1773 ns).

**This inverts the premise of the whole native investigation.** The hypothesis was "per-node IPC
round trips are unaffordable, so pay for FFI to make per-node access cheap." But per-node access is
*not* cheap over FFI either — 829 ns is within ~1-2 orders of magnitude of a local named-pipe round
trip (UNVERIFIED, but typically tens of µs), not the ~1000x improvement that would justify the fork.

The correct design under *both* transports is identical: **transfer the AST in bulk and traverse it
in local memory.** IPC already does exactly that via `api/encoder` (§5.1), using the same encoder the
spike called. Once you adopt the bulk design — and you must, on either transport — FFI's remaining
advantage shrinks to one pipe write per file.

FFI's genuine edge survives only for the **non-batchable dependent checker walk** of §5.3: there,
829 ns versus a pipe round trip is a real multiple. That remains the sole trigger for revisiting.

---

## 6. WASM / WASI — short verdict

**No. Do not pursue.** Reasoning:

- No WASI build target exists upstream (§2). UNVERIFIED whether `GOOS=wasip1 GOARCH=wasm go build`
  would even succeed on this codebase — not attempted.
- It would not dodge marshalling; it makes it strictly worse. A wasm guest has a **separate linear
  memory**, so every string and every AST blob must be copied *into or out of* the sandbox's memory
  through the host's memory-access API. That is the same copy as the FFI route plus sandbox
  bookkeeping, and pointers are guest-relative `u32` offsets rather than real addresses.
- Filesystem access (essential — the compiler reads `node_modules`, `tsconfig`s, and source trees)
  must go through WASI preopens, and every read is a host call. The compiler is I/O-heavy by nature.
- Go's wasm GC/threading story is weaker than native: no real threads under `wasip1`, so the checker
  pool's parallelism is lost.
- You would still be forking to add an entry point, so the §5(b) maintenance cost stays, while
  losing native performance and gaining an immature toolchain.

Wasmtime hosting is a real technology, but it solves *sandboxing*, which is not a problem you have —
you already trust the TypeScript compiler.

---

## 7. Reproduction summary

From a clean machine with Go 1.26+, MinGW-W64 gcc, and git:

```bash
mkdir -p /c/Users/shaya/RiderProjects/Xantham/tmp/tsgo-native
cd /c/Users/shaya/RiderProjects/Xantham/tmp/tsgo-native
git clone --depth 1 --filter=blob:none https://github.com/microsoft/TypeScript.git
mkdir -p TypeScript/tsc/capi
# copy capi/capi.go and capi/program.go into TypeScript/tsc/capi/, then rewrite their
# imports: github.com/microsoft/typescript-go/internal -> github.com/microsoft/TypeScript/tsc/internal
cd TypeScript/tsc
CGO_ENABLED=1 go build -buildmode=c-shared -o ../../tsgo_capi.dll ./capi
cd ../..
gcc driver.c -o driver.exe
./driver.exe        # must print "OK" and exit 0

# benchmark (bench.c) + the C-DLL control (cnoop.c/ctrl.c)
gcc -O2 bench.c -o bench.exe && ./bench.exe
gcc -O2 -shared cnoop.c -o cnoop.dll && gcc -O2 ctrl.c -o ctrl.exe && ./ctrl.exe
```

No upstream file is modified; the fork is purely additive (`capi/` only).

### Artifact inventory (`tmp/tsgo-native/`)

| Path | What |
| --- | --- |
| ~~`typescript-go/`~~ | **deleted 2026-08-31.** Was an upstream `microsoft/typescript-go` clone @ `89d5d5b`; that repo is archived. Re-clone `microsoft/TypeScript` if this route is ever revisited. |
| `capi/capi.go` | version/parse/diagnostics/AST-blob/handle-table/panic-barrier/bench exports (lifted out of the deleted clone; imports still use the old `typescript-go` module path) |
| `capi/program.go` | tsconfig -> Program -> semantic diagnostics -> checker |
| `tsgo_capi.dll`, `tsgo_capi.h` | the shared library and its generated header (~36 MB) |
| `tsgo_capi_static.dll` | `-extldflags=-static` variant; byte-identical size, no difference |
| `driver.c` / `driver.exe` | functional harness (the §3.5 output) |
| `bench.c` / `bench.exe` | FFI overhead + bulk-encode benchmark (§5.4) |
| `cnoop.c` / `cnoop.dll` / `ctrl.c` / `ctrl.exe` | plain-C control establishing the 1.3 ns baseline |
| `fixture/` | `tsconfig.json` + `src/main.ts` with one deliberate type error |
| `imports.txt` | PE import-table dump (KERNEL32 + msvcrt only) |

### What was NOT verified

- **Nothing was called from .NET.** Per the brief, no F# project code was written; the harness is C.
  P/Invoke marshalling cost sits on top of the 828.8 ns figure.
- **No IPC baseline was measured.** The comparison against "a local pipe round trip" is reasoned,
  not timed. If the decision is close, time the other agent's IPC client against `bench.exe`.
- Concurrent entry into `Program`/`Checker` from multiple threads.
- `AssemblyLoadContext` unload / reload of the Go runtime (suspected unsafe).
- Cross-compilation for non-Windows RIDs; only win-x64 was built.
- Whether `GOOS=wasip1` builds at all.
- The Go layout of `microsoft/TypeScript`, the repository development moves back to.
